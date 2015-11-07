datos <- read.csv('PVA97_EEA2015.csv', sep='\t')

datos$TARGET_D <- as.numeric(sub(",", ".", datos$TARGET_D, fixed = TRUE))

modelo.1 <- lm(TARGET_D ~ DemAge, data=datos)
summary(modelo.1)
## Call:
## lm(formula = TARGET_D ~ DemAge, data = datos)
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.023  -6.388  -1.954   4.337 185.177 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 18.24977    0.77009   23.70  < 2e-16 ***
## DemAge      -0.04231    0.01241   -3.41 0.000658 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## Residual standard error: 12 on 3677 degrees of freedom
## Multiple R-squared:  0.003152,  Adjusted R-squared:  0.00288 
## F-statistic: 11.62 on 1 and 3677 DF,  p-value: 0.0006578

summary(datos$DemAge)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 4.00   48.00   61.00   59.98   73.00   87.00

plot(datos$DemAge, datos$TARGET_D)
abline(modelo.1, col="red")

modelo.1.predicciones <- predict(modelo.1, newdata = datos, interval = c("confidence"), level = 0.95, type="response")
lines(datos, modelo.1.predicciones[,2], col="red", lty=2)
lines(datos, modelo.1.predicciones[,3], col="red", lty=2)

## gráfico

predict(modelo.1, newdata = data.frame(DemAge=69), interval = c("confidence"), level = 0.95, type="response")
## media:
##       fit      lwr      upr
## 1 15.33056 14.88495 15.77618

predict(modelo.1, newdata = data.frame(DemAge=69), interval = c("prediction"), level = 0.95, type="response")
## individuos
##        fit       lwr      upr
## 1 15.33056 -8.197332 38.85846










## summary(fit) # show results
## coefficients(fit) # model coefficients
## confint(fit, level=0.95) # CIs for model parameters 
## fitted(fit) # predicted values
## residuals(fit) # residuals
## anova(fit) # anova table 
## vcov(fit) # covariance matrix for model parameters 
## influence(fit) # regression diagnostics
