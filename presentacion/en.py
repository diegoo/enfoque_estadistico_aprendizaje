#!/usr/bin/env python
# -*- coding:utf-8 -*-

# regularized regression method that linearly combines the L1 and L2 penalties of the lasso and ridge methods.

# alpha : α with range α∈[0,1]. α=1 is the lasso (default) and α=0 is the ridge.

# ElasticNet is a linear regression model trained with L1 and L2 prior as regularizer. This combination allows for learning a sparse model where few of the weights are non-zero like Lasso, while still maintaining the regularization properties of Ridge. We control the convex combination of L1 and L2 using the l1_ratio parameter.

# Elastic-net is useful when there are multiple features which are correlated with one another. Lasso is likely to pick one of these at random, while elastic-net is likely to pick both.

import numpy as np
import matplotlib.pyplot as plt

from sklearn.metrics import r2_score

# generate sparse data
np.random.seed(12345)

n_samples, n_features = 50, 200
X = np.random.randn(n_samples, n_features)
coef = 3 * np.random.randn(n_features)
inds = np.arange(n_features)
np.random.shuffle(inds)
coef[inds[10:]] = 0  # sparsify coef
y = np.dot(X, coef)

# add noise
y += 0.01 * np.random.normal((n_samples,))

# Split data in train set and test set
n_samples = X.shape[0]
X_train, y_train = X[:n_samples / 2], y[:n_samples / 2]
X_test, y_test = X[n_samples / 2:], y[n_samples / 2:]

# elastic net
from sklearn.linear_model import ElasticNet

alpha = 0.1
l1_ratio=0.7

enet = ElasticNet(alpha=alpha, l1_ratio=l1_ratio)
enet_model = enet.fit(X_train, y_train)
y_pred_enet = enet_model.predict(X_test)
r2_score_enet = r2_score(y_test, y_pred_enet)

print(enet)
print("r^2 on test data : %f" % r2_score_enet)
# r^2 on test data : 0.100723

# plt.plot(enet.coef_, label='Elastic net coefficients')
# plt.plot(coef, '--', label='original coefficients')
# plt.legend(loc='best')
# plt.title("R^2: %f" % (r2_score_enet))
# plt.show()

# set the parameters alpha (\alpha) and l1_ratio (\rho) by cross-validation.
from sklearn.linear_model import ElasticNetCV

enetcv = ElasticNetCV(l1_ratio=[.1,.2,.3,.4,.5,.6,.7,.8,.9])
enetcv_model = enetcv.fit(X_train, y_train)
y_pred_enetcv = enetcv_model.predict(X_test)
r2_score_enetcv = r2_score(y_test, y_pred_enetcv)

print(enetcv)
print("r^2 on test data : %f" % r2_score_enetcv)
# r^2 on test data : 0.22553

assert(r2_score_enetcv > r2_score_enet)
