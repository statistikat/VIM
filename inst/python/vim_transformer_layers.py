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
