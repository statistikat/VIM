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


def shared_mask(encoded, observed, target_indices, rate, rng, block=False):
    import numpy as np
    result = np.array(encoded, copy=True)
    observed = np.asarray(observed, dtype=bool)
    random = rng.random((len(result), 1 if block else len(target_indices)))
    mask = (random < rate) & observed
    weights = []
    for k, j in enumerate(target_indices):
        candidates = np.flatnonzero(observed[:, k])
        if not len(candidates):
            raise ValueError("Every target needs observed labels in each partition")
        if not mask[:, k].any():
            mask[rng.choice(candidates), k] = True
        result[mask[:, k], 2 * int(j)] = 0
        result[mask[:, k], 2 * int(j) + 1] = 1
        weights.append(mask[:, k].astype('float32') * len(result) / mask[:, k].sum())
    return result, weights, mask


def fit_shared(encoded, labels, observed, cardinalities, target_indices,
               output_units, categorical, train_rows, validation_rows,
               width=32, heads=4, epochs=20, batch_size=64, patience=3,
               mask_rate=0.3, seed=None, verbose=0):
    import numpy as np
    rng = np.random.default_rng(seed)
    encoded = np.asarray(encoded, dtype='float32')
    labels = [np.asarray(y, dtype='float32').reshape(-1, 1) for y in labels]
    observed = np.asarray(observed, dtype=bool)
    tr = np.asarray(train_rows, dtype=int); va = np.asarray(validation_rows, dtype=int)
    base = build_tabular(cardinalities, 1, False, width, heads)
    representation = base.layers[-2].output
    outputs = [keras.layers.Dense(int(units), activation='softmax' if cat else None,
                                  name=f'target_{k}')(representation)
               for k, (units, cat) in enumerate(zip(output_units, categorical))]
    model = keras.Model(base.input, outputs)
    model.compile(optimizer=keras.optimizers.legacy.Adam(),
                  loss=['sparse_categorical_crossentropy' if cat else 'mse' for cat in categorical])
    vx, vw, validation_mask = shared_mask(encoded[va], observed[va], target_indices, mask_rate, rng)
    best, best_weights, stale, history = float('inf'), None, 0, []
    for epoch in range(int(epochs)):
        x, weights, _ = shared_mask(encoded[tr], observed[tr], target_indices,
                                    mask_rate, rng, block=bool(epoch % 2))
        result = model.fit(x, [y[tr] for y in labels], sample_weight=weights,
                           validation_data=(vx, [y[va] for y in labels], vw),
                           epochs=1, batch_size=int(batch_size), verbose=int(verbose))
        score = float(result.history['val_loss'][0]); history.append(score)
        if not np.isfinite(score):
            raise ValueError('Non-finite shared-model validation loss')
        if score < best:
            best, best_weights, stale = score, model.get_weights(), 0
        else:
            stale += 1
            if stale >= int(patience):
                break
    model.set_weights(best_weights)
    return dict(model=model, validation_loss=history, validation_mask=validation_mask)
