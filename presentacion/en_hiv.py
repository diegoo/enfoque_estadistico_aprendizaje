#!/usr/bin/python
# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
import numpy as np
from sklearn import datasets, linear_model
from sklearn.metrics import r2_score

# exportamos el dataset de HIV a .csv desde su formato original de .RData

# cargamos el dataset

hiv_train_x = np.genfromtxt('hiv.train.x.csv', delimiter=",")
hiv_train_y = np.genfromtxt('hiv.train.y.csv', delimiter=",")
hiv_test_x = np.genfromtxt('hiv.test.x.csv', delimiter=",")
hiv_test_y = np.genfromtxt('hiv.test.y.csv', delimiter=",")

# 704 observaciones, 208 variables predictoras binarias (1/0) 

hiv_train_x.shape
# (704, 208)

# una observación de ejemplo

hiv_train_x[0]
# array([ 1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,
#         1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,
#         1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  0.,  0.,
#         0.,  1.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  1.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0.,  1.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
#         0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.])

hiv_train_y.shape
# (704,)

# la variable de respuesta es numérica

hiv_train_y[0]
# 2.30102999566

# preparamos 3 modelos: Lineal, Lasso y Elastic Net
# los comparamos usando la función score(), que devuelve la varianza explicada; score=1 es óptimo

l = linear_model.LinearRegression()
l_model = l.fit(hiv_train_x, hiv_train_y)
y_pred_l_model = l_model.predict(hiv_test_x)
r2_score_l_model = r2_score(hiv_test_y, y_pred_l_model)

r2_score_l_model
# 0.88868424783910593

lassocv = linear_model.LassoCV()
lassocv_model = lassocv.fit(hiv_train_x, hiv_train_y)
y_pred_lassocv = lassocv_model.predict(hiv_test_x)
r2_score_lassocv = r2_score(hiv_test_y, y_pred_lassocv)

r2_score_lassocv
# 0.93820094210822402

enetcv = linear_model.ElasticNetCV(l1_ratio=[.5,.6,.7,.8,.9,1])
enetcv_model = enetcv.fit(hiv_train_x, hiv_train_y)
y_pred_enetcv = enetcv_model.predict(hiv_test_x)
r2_score_enetcv = r2_score(hiv_test_y, y_pred_enetcv)

r2_score_enetcv
# 0.93825334982548758

# l_model.coef_
# np.mean((l_model.predict(hiv_test_x) - hiv_test_y) ** 2)

# plt.scatter(hiv_test_x, hiv_test_y,  color='black')
# plt.plot(hiv_test_x, regr.predict(hiv_test_x), color = 'blue', linewidth = 3)
# plt.xticks(())
# plt.yticks(())
# plt.show()
