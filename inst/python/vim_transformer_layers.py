"""Serializable layers for VIM's legacy tf-keras transformer backend."""
import tensorflow as tf
import tf_keras as keras


@keras.utils.register_keras_serializable(package="VIM")
class MaskedMean(keras.layers.Layer):
    def call(self, inputs):
        features, tokens = inputs
        valid = tf.cast(tf.not_equal(tokens, 0), features.dtype)[..., None]
        return tf.math.divide_no_nan(tf.reduce_sum(features * valid, axis=1),
                                     tf.reduce_sum(valid, axis=1))


@keras.utils.register_keras_serializable(package="VIM")
class ValidAttentionMask(keras.layers.Layer):
    def call(self, tokens):
        valid = tf.not_equal(tokens, 0)
        return tf.logical_and(valid[:, :, None], valid[:, None, :])


@keras.utils.register_keras_serializable(package="VIM")
class ValidPositionEmbedding(keras.layers.Layer):
    def __init__(self, maxlen, width, **kwargs):
        super().__init__(**kwargs)
        self.maxlen, self.width = int(maxlen), int(width)
        self.embedding = keras.layers.Embedding(self.maxlen, self.width)

    def call(self, tokens):
        valid = tf.cast(tf.not_equal(tokens, 0), tf.int32)
        position = tf.maximum(tf.cumsum(valid, axis=1) - 1, 0)
        return self.embedding(position) * tf.cast(valid[..., None], self.compute_dtype)

    def get_config(self):
        return dict(super().get_config(), maxlen=self.maxlen, width=self.width)


@keras.utils.register_keras_serializable(package="VIM")
class ColumnSlice(keras.layers.Layer):
    def __init__(self, start, width=1, **kwargs):
        super().__init__(**kwargs)
        self.start, self.width = int(start), int(width)

    def call(self, x):
        return x[:, self.start:self.start + self.width]

    def get_config(self):
        return dict(super().get_config(), start=self.start, width=self.width)


def build_tabular(cardinalities, output_units, categorical, width=32, heads=4,
                  loss="mse", learning_rate=0.001):
    """One token per predictor, with a learned identity for each column."""
    width, heads = int(width), int(heads)
    inputs = keras.Input(shape=(2 * len(cardinalities),))
    tokens = []
    for j, cardinality in enumerate(cardinalities):
        if int(cardinality) > 0:
            value = ColumnSlice(2 * j)(inputs)
            token = keras.layers.Embedding(int(cardinality), width)(value)
        else:
            value = ColumnSlice(2 * j, 2)(inputs)
            token = keras.layers.Reshape((1, width))(keras.layers.Dense(width)(value))
        tokens.append(token)
    x = tokens[0] if len(tokens) == 1 else keras.layers.Concatenate(axis=1)(tokens)
    attention = keras.layers.MultiHeadAttention(num_heads=heads, key_dim=width // heads)(x, x)
    x = keras.layers.LayerNormalization()(x + attention)
    residual = keras.layers.Dense(width)(keras.layers.Dense(2 * width, activation="relu")(x))
    x = keras.layers.LayerNormalization()(x + residual)
    x = keras.layers.GlobalAveragePooling1D()(x)
    output = keras.layers.Dense(int(output_units), activation="softmax" if categorical else None)(x)
    model = keras.Model(inputs, output)
    model.compile(optimizer=keras.optimizers.legacy.Adam(learning_rate=float(learning_rate)),
                  loss="sparse_categorical_crossentropy" if categorical else loss)
    return model
