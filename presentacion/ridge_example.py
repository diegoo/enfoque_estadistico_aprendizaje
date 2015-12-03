#!/usr/bin/env python
# -*- coding:utf-8 -*-

from sklearn import linear_model

# Ridge

# penalizar el tamaño de los coeficientes, tal que los coeficientes minimicen una suma de cuadrados penalizada: \underset{w}{min\,} {{|| X w - y||_2}^2 + \alpha {||w||_2}^2}
# alpha = parámetro que controla cuánto se reducen los coeficientes: mayor alpha, más se reducen, más robustos se vuelven los coeficientes ante el problema de colinealidad.

clf = linear_model.Ridge (alpha = .5)
clf.fit([[0, 0], [0, 0], [1, 1]], [0, .1, 1]) 
clf.predict([[1, 1]])
clf.coef_
clf.intercept_ 

# encontrar alpha por cross-validation

clf2 = linear_model.RidgeCV(alphas=[0.1, 1.0, 10.0])
clf2.fit([[0, 0], [0, 0], [1, 1]], [0, .1, 1])       
clf2.predict([[1, 1]])
clf2.coef_
clf2.intercept_ 
clf2.alpha_
