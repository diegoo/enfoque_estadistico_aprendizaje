#!/usr/bin/env python
# -*- coding:utf-8 -*-

from __future__ import print_function
print(__doc__)

import numpy as np
import matplotlib.pyplot as plt

from sklearn import cross_validation, datasets, linear_model

diabetes = datasets.load_diabetes()
X = diabetes.data[:150]
y = diabetes.target[:150]

alphas = np.logspace(-4, -.5, 30)
modelo = linear_model.ElasticNetCV(alphas = alphas)
k_fold = cross_validation.KFold(len(X), 50)

alphas_modelos = []
scores = []
for k, (train, test) in enumerate(k_fold):
    modelo.fit(X[train], y[train])
    # print("[fold {0}] alpha: {1:.5f}, score: {2:.5f}".format(k, modelo.alpha_, modelo.score(X[test], y[test])))
    alphas_modelos.append(modelo.alpha_)
    scores.append(modelo.score(X[test], y[test]))

print(scores)

plt.scatter(alphas_modelos, scores)
plt.show()
