#!/usr/bin/env python
# -*- coding:utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt
import mlpy

np.random.seed(0)
mean1, cov1, n1 = [1, 5], [[1,1],[1,2]], 200  # 200 samples of class 1
x1 = np.random.multivariate_normal(mean1, cov1, n1)
y1 = np.ones(n1, dtype=np.int)
mean2, cov2, n2 = [2.5, 2.5], [[1,0],[0,1]], 300 # 300 samples of class -1
x2 = np.random.multivariate_normal(mean2, cov2, n2)
y2 = -np.ones(n2, dtype=np.int)
x = np.concatenate((x1, x2), axis=0) # concatenate the samples
y = np.concatenate((y1, y2))
en = mlpy.ElasticNetC(lmb=0.01, eps=0.001)
en.learn(x, y)
w = en.w()
b = en.bias()
en.iters()

xx = np.arange(np.min(x[:,0]), np.max(x[:,0]), 0.01)
yy = - (w[0] * xx + b) / w[1] # separator line
fig = plt.figure(1) # plot
plot1 = plt.plot(x1[:, 0], x1[:, 1], 'ob', x2[:, 0], x2[:, 1], 'or')
plot2 = plt.plot(xx, yy, '--k')
plt.show()

test = [[1, 4], [2, 2]] # test points
en.pred(test)
