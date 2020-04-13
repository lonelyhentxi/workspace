import numpy as np
from .activation import *


class MLPClassifier(object):

    def __init__(self,
                 layers,
                 activation='tanh',
                 epochs=20, batch_size=1, learning_rate=0.01):
        self.epochs = epochs
        self.learning_rate = learning_rate
        self.layers = []
        self.weights = []
        self.batch_size = batch_size

        for i in range(0, len(layers) - 1):
            weight = np.random.random((layers[i], layers[i + 1]))
            layer = np.ones(layers[i])
            self.layers.append(layer)
            self.weights.append(weight)
        self.layers.append(np.ones(layers[-1]))

        self.thresholds = []
        for i in range(1, len(layers)):
            threshold = np.random.random(layers[i])
            self.thresholds.append(threshold)

        if activation == 'tanh':
            self.activation = tanh
            self.dactivation = dtanh
        elif activation == 'sigmoid':
            self.activation = sigmoid
            self.dactivation = dsigmoid
        elif activation == 'relu':
            self.activation = relu
            self.dactivation = drelu

    def fit(self, X, y):
        for _ in range(self.epochs * (X.shape[0] // self.batch_size)):
            i = np.random.choice(X.shape[0], self.batch_size)
            # i = np.random.randint(X.shape[0])
            self.update(X[i])
            self.back_propagate(y[i])
        return self

    def predict(self, X):
        self.update(X)
        return self.layers[-1].copy()

    def update(self, inputs):
        self.layers[0] = inputs
        for i in range(len(self.weights)):
            next_layer_in = self.layers[i] @ self.weights[i] - self.thresholds[i]
            self.layers[i + 1] = self.activation(next_layer_in)

    def back_propagate(self, y):
        errors = y - self.layers[-1]

        gradients = [(self.dactivation(self.layers[-1]) * errors).sum(axis=0)]

        self.thresholds[-1] -= self.learning_rate * gradients[-1]
        for i in range(len(self.weights) - 1, 0, -1):
            tmp = np.sum(gradients[-1] @ self.weights[i].T * self.dactivation(self.layers[i]), axis=0)
            gradients.append(tmp)
            self.thresholds[i - 1] -= self.learning_rate * gradients[-1] / self.batch_size
        gradients.reverse()
        for i in range(len(self.weights)):
            tmp = np.mean(self.layers[i], axis=0)
        self.weights[i] += self.learning_rate * tmp.reshape((-1, 1)) * gradients[i]