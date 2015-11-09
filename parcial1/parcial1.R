datos <- read.csv('PVA97_EEA2015.csv', sep='\t')

datos$TARGET_D <- as.numeric(sub(",", ".", datos$TARGET_D, fixed = TRUE))

## estas ya venían continuas:
## GiftCnt36      : int  4 4 1 6 2 1 1 2 1 1 ...
## GiftCntAll     : int  8 25 1 28 5 4 15 11 1 1 ...
## GiftCntCardAll : int  5 17 1 17 2 1 7 9 1 1 ...
## GiftTimeFirst  : int  68 129 24 127 68 49 129 93 20 24 ...
## DemAge         : int  27 81 70 17 78 55 73 47 37 60 ...
## DemPctVeterans : int  44 45 62 0 48 28 0 1 32 23 ...
## GiftTimeLast   : int  24 17 24 5 26 19 16 21 20 24 ...

## estas se convierten de factor a continuas:
datos$GiftAvgLast     <- as.numeric(sub(",", ".", datos$GiftAvgLast     , fixed = TRUE))
datos$GiftAvg36       <- as.numeric(sub(",", ".", datos$GiftAvg36       , fixed = TRUE))
datos$GiftAvgAll      <- as.numeric(sub(",", ".", datos$GiftAvgAll      , fixed = TRUE))
datos$DemMedHomeValue <- as.numeric(sub(",", ".", datos$DemMedHomeValue , fixed = TRUE))
datos$DemMedIncome    <- as.numeric(sub(",", ".", datos$DemMedIncome    , fixed = TRUE))

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

modelo.2.none <- lm(TARGET_D ~ 1, data=datos)
modelo.2.full.12 <- lm(TARGET_D ~ GiftCnt36 + GiftCntAll + GiftCntCardAll + GiftTimeFirst + GiftTimeLast + GiftAvgLast + GiftAvg36 + GiftAvgAll + DemAge + DemPctVeterans + DemMedHomeValue + DemMedIncome, data=datos)
modelo.2.fw <- step(modelo.2.none, scope=list(lower=modelo.2.none, upper=modelo.2.full.12), direction="forward")
summary(modelo.2.fw)

## Call:
## lm(formula = TARGET_D ~ GiftAvgLast + GiftAvg36 + GiftCnt36 + GiftTimeFirst + GiftTimeLast, data = datos)

## Residuals:
##     Min      1Q  Median      3Q     Max 
## -77.933  -2.275  -0.775   1.799 156.859 

## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    4.682614   0.789657   5.930 3.31e-09 ***
## GiftAvgLast    0.512253   0.023447  21.847  < 2e-16 ***
## GiftAvg36      0.314746   0.027491  11.449  < 2e-16 ***
## GiftCnt36     -0.325368   0.067807  -4.798 1.66e-06 ***
## GiftTimeFirst -0.014193   0.003635  -3.904 9.61e-05 ***
## GiftTimeLast   0.055916   0.032904   1.699   0.0893 .  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Residual standard error: 8.07 on 3673 degrees of freedom
## Multiple R-squared:  0.5496,    Adjusted R-squared:  0.549 
## F-statistic: 896.3 on 5 and 3673 DF,  p-value: < 2.2e-16

## los que quedan significativos:
## GiftAvgLast    0.512253   0.023447  21.847  < 2e-16 ***
## GiftAvg36      0.314746   0.027491  11.449  < 2e-16 ***
## GiftCnt36     -0.325368   0.067807  -4.798 1.66e-06 ***
## GiftTimeFirst -0.014193   0.003635  -3.904 9.61e-05 ***

## GiftTimeLast no es significativa, la sacamos y rearmamos el modelo:
modelo.3 <- lm(formula = TARGET_D ~ GiftAvgLast + GiftAvg36 + GiftCnt36 + GiftTimeFirst, data = datos)

## Betas estandarizados (coeficientes tipificados)
library(lm.beta)
lm.beta(modelo.3)

## Call:
## lm(formula = TARGET_D ~ GiftAvgLast + GiftAvg36 + GiftCnt36 + 
##     GiftTimeFirst, data = datos)
## Standardized Coefficients::
##   (Intercept)   GiftAvgLast     GiftAvg36     GiftCnt36 GiftTimeFirst 
##    0.00000000    0.48053125    0.25296787   -0.06415372   -0.04442186
## GiftAvgLast es la más importante
## GiftTimeFirst es la menos importante

## otra forma es directamente estandarizar las variables
## lm(scale(your.y) ~ scale(your.x), data=your.Data)

variables.modelo.3 <- data.frame(datos$GiftAvgLast, datos$GiftAvg36, datos$GiftCnt36, datos$GiftTimeFirst)
library(corrplot)
variables.modelo.3.cor <- cor(variables.modelo.3)
corrplot(variables.modelo.3.cor, method = "circle")

## alta correlación entre GiftAvgLast y GiftAvg36

## veamos ahora multicolinearidad usando FIV (valores de referencia entre 1 y 10)
library(car)
library(MASS)
vif(modelo.3)

## GiftAvgLast     GiftAvg36     GiftCnt36 GiftTimeFirst 
##    3.909093      4.024103      1.177776      1.032788

vifs <- c(3.909093, 4.024103, 1.177776, 1.032788)
summary(vifs)

par(mfrow=c(2,2))
plot(modelo.3)

## plot 3, residuos vs valores predichos: se acumulan al principio: varianza no es homogenea a lo largo de los valores de la variable dependiente

## modelo stepwise

modelo.2.none <- lm(TARGET_D ~ 1, data=datos)
modelo.2.full.12 <- lm(TARGET_D ~ GiftCnt36 + GiftCntAll + GiftCntCardAll + GiftTimeFirst + GiftTimeLast + GiftAvgLast + GiftAvg36 + GiftAvgAll + DemAge + DemPctVeterans + DemMedHomeValue + DemMedIncome, data=datos)
modelo.4.sw <- step(modelo.2.none, scope=list(lower=modelo.2.none, upper=modelo.2.full.12), direction="both")
summary(modelo.4.sw)

## Call:
## lm(formula = TARGET_D ~ GiftAvgLast + GiftAvg36 + GiftCnt36 + 
##     GiftTimeFirst + GiftTimeLast, data = datos)

## Residuals:
##     Min      1Q  Median      3Q     Max 
## -77.933  -2.275  -0.775   1.799 156.859 

## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    4.682614   0.789657   5.930 3.31e-09 ***
## GiftAvgLast    0.512253   0.023447  21.847  < 2e-16 ***
## GiftAvg36      0.314746   0.027491  11.449  < 2e-16 ***
## GiftCnt36     -0.325368   0.067807  -4.798 1.66e-06 ***
## GiftTimeFirst -0.014193   0.003635  -3.904 9.61e-05 ***
## GiftTimeLast   0.055916   0.032904   1.699   0.0893 .  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Residual standard error: 8.07 on 3673 degrees of freedom
## Multiple R-squared:  0.5496,    Adjusted R-squared:  0.549 
## F-statistic: 896.3 on 5 and 3673 DF,  p-value: < 2.2e-16

## R cuadrado 0.549 para modelo.3 y modelo.4, son mejores que modelo.1

## stepwise da estos como mejores con 3 y 4 variables:
## Step:  AIC=15384.65
## TARGET_D ~ GiftAvgLast + GiftAvg36 + GiftCnt36
## Step:  AIC=15371.09
## TARGET_D ~ GiftAvgLast + GiftAvg36 + GiftCnt36 + GiftTimeFirst

library(leaps)
modelos.subsets <- regsubsets(TARGET_D ~ GiftCnt36 + GiftCntAll + GiftCntCardAll + GiftTimeFirst + GiftTimeLast + GiftAvgLast + GiftAvg36 + GiftAvgAll + DemAge + DemPctVeterans + DemMedHomeValue + DemMedIncome,
         data = datos,
         nbest = 1,       # 1 best model for each number of predictors
         nvmax = NULL,    # NULL for no limit on number of variables
         force.in = NULL, force.out = NULL,
         method = "exhaustive")

modelos.subsets.summary.out <- summary(modelos.subsets)
as.data.frame(modelos.subsets.summary.out$outmat)

##  GiftCnt36 GiftCntAll GiftCntCardAll GiftTimeFirst GiftTimeLast  GiftAvgLast GiftAvg36 GiftAvgAll DemAge DemPctVeterans  DemMedHomeValue DemMedIncome           
#1                                                                        *                                                                                     
#2                                                                        *         *                                                                           
#3          *                                                             *         *                                                                           
#4          *                                       *                     *         *                                                                           
##          *                                       *            *        *         *                                                                           
##          *                                       *            *        *         *                                *                                          
##          *                                       *            *        *         *          *                     *                                          
##          *                                       *            *        *         *          *                     *                                 *        
##          *                                       *            *        *         *          *                     *                    *            *        
##          *                         *             *            *        *         *          *                     *                    *            *        
##          *                         *             *            *        *         *          *      *              *                    *            *        
##          *          *              *             *            *        *         *          *      *              *                    *            *        

## 3 variables: GiftCnt36, GiftAvg36, GiftAvgLast
## 4 variables: GiftCnt36, GiftAvg36, GiftAvgLast, GiftTimeFirst
## el criterio es menor AIC

plot(modelos.subsets, scale = "adjr2", main = "Adjusted R^2")

modelo.5.min <- lm(TARGET_D ~ DemAge, data=datos)
modelo.2.full.12 <- lm(TARGET_D ~ GiftCnt36 + GiftCntAll + GiftCntCardAll + GiftTimeFirst + GiftTimeLast + GiftAvgLast + GiftAvg36 + GiftAvgAll + DemAge + DemPctVeterans + DemMedHomeValue + DemMedIncome, data=datos)
modelo.5 <- step(modelo.5.min, scope=list(lower=modelo.5.min, upper=modelo.2.full.12), direction="both")
summary(modelo.5)

## Call:
## lm(formula = TARGET_D ~ DemAge + GiftAvgLast + GiftAvg36 + GiftCnt36 + 
##     GiftTimeFirst + GiftTimeLast, data = datos)
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -77.915  -2.271  -0.771   1.797 156.841 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    4.7216589  0.9458254   4.992 6.25e-07 ***
## DemAge        -0.0006444  0.0085888  -0.075 0.940201    
## GiftAvgLast    0.5122808  0.0234536  21.842  < 2e-16 ***
## GiftAvg36      0.3146809  0.0275086  11.439  < 2e-16 ***
## GiftCnt36     -0.3253707  0.0678164  -4.798 1.67e-06 ***
## GiftTimeFirst -0.0141374  0.0037116  -3.809 0.000142 ***
## GiftTimeLast   0.0556810  0.0330570   1.684 0.092191 .  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## Residual standard error: 8.071 on 3672 degrees of freedom
## Multiple R-squared:  0.5496,    Adjusted R-squared:  0.5488 
## F-statistic: 746.7 on 6 and 3672 DF,  p-value: < 2.2e-16

## usando r cuadrado ajustado (para comparar modelos con distinta cantidad de variables):
## con edad: 0.5488
## sin edad: 0.549
## son iguales => por parsimonia, preferir modelo sin edad.