#!/usr/bin/env python
# -*- coding:utf-8 -*-

from sklearn import linear_model

# Lasso

# preferir menos parámetros. minimiza esta función: \underset{w}{min\,} { \frac{1}{2n_{samples}} ||X w - y||_2 ^ 2 + \alpha ||w||_1}
# usa L1 como regularizador

clf = linear_model.Lasso (alpha = .1)
clf.fit ([[0, 0], [0, 0], [1, 1]], [0, .1, 1]) 
clf.predict([[1, 1]])
clf.coef_
clf.intercept_ 
