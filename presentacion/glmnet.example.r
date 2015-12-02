Elastic Net creates a regression model that is penalized with both the L1-norm and L2-norm. This has the effect of effectively shrinking coefficients (as in ridge regression) and setting some coefficients to zero (as in LASSO).


# load the package
library(glmnet)
# load data
data(longley)
x <- as.matrix(longley[,1:6])
y <- as.matrix(longley[,7])
# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
rmse <- mean((y - predictions)^2)
print(rmse)
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
# load the package
library(glmnet)
# load data
data(longley)
x <- as.matrix(longley[,1:6])
y <- as.matrix(longley[,7])
# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
rmse <- mean((y - predictions)^2)
print(rmse)

