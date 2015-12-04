library(glmnet)
load("hiv.rda")

class(hiv.train)                                                                        # The data are stored as a list
names(hiv.train)                                                                        # The names of the list elements are x and y
dim(hiv.train$x)                                                                        # 704 observaciones
[1] 704 208											# predictoras : 208 variables (binarias, 1/0)
											# dependiente: cambios en susceptibilidad a drogas antivirales
head(hiv.train[[1]])
head(hiv.train[[2]])                                                                    

modelo_ridge <- glmnet(hiv.train$x, hiv.train$y, alpha = 0)
plot(modelo_ridge, label = TRUE)

modelo_lasso <- glmnet(hiv.train$x, hiv.train$y, alpha = 1)
plot(modelo_lasso, label = TRUE)

modelo_EN <- glmnet(hiv.train$x, hiv.train$y, alpha = 0.5)

coef(modelo, s = 0.1) # ver coeficientes para un valor dado de lambda

modelo <- glmnet(hiv.train$x, hiv.train$y)
plot(modelo, label = TRUE)

modelo                                                                                     # fit for each coefficient

# Each curve represents a coefficient in the model. eje x: lambda. y: valor del coeficiente. coeficientes entran al modelo cuando el valor de log(lambda) decrece.
# Each curve corresponds to a variable. It shows the path of its coefficient against the  l 1 ­norm of the whole coefficient vector at as  λ  varies. The axis above indicates the number of nonzero coefficients at the current  λ , which is the effective degrees of freedom (df) for the lasso.

cv.modelo <- cv.glmnet(hiv.train$x, hiv.train$y)                                          # ajustar el modelo por cross-validation
plot(cv.modelo, label = TRUE)                                                             # MSE vs lambda
cv.modelo

# ver el lambda que dé el error mínimo
# ver el lambda que dé el modelo más regularizado, con un error dentro de una desviación del mínimo

cv.modelo$lambda.min 
cv.modelo$lambda.lse 

coef(cv.modelo, s = cv.modelo$lambda.min)

predicciones <- predict(modelo, hiv.test$x)
error <- apply((predicciones - hiv.test$y)^2, 2, mean)

points(log(modelo$lambda), mte, col="blue", pch="*")
legend("topleft",legend=c("10 fold CV","Test"),pch="*",col=c("red","blue"))
plot(cv.modelo, xvar="lambda")
plot(cv.modelo, xvar="dev")

# http://blog.revolutionanalytics.com/2013/05/hastie-glmnet.html
# hiv.coefficients.plot.png
# hiv article: http://www.pnas.org/content/103/46/17355.full.pdf
# hastie slides: http://web.stanford.edu/~hastie/TALKS/enet_talk.pdf
