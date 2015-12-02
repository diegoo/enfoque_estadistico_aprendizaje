data <- read.csv('sick.tsv',sep='\t',colClasses=c(
"numeric",   #caso                      
"numeric",   #age                       
"factor",    #sex                       
"factor",    #on_thyroxine              
"factor",    #query_on_thyroxine        
"factor",    #on_antithyroid_medication 
"factor",    #sick                      
"factor",    #pregnant                  
"factor",    #thyroid_surgery           
"factor",    #I131_treatment            
"factor",    #query_hypothyroid         
"factor",    #query_hyperthyroid        
"factor",    #lithium                   
"factor",    #goitre                    
"factor",    #tumor                     
"factor",    #hypopituitary             
"factor",    #psych                     
"numeric",   #TSH                       
"numeric",   #T3                        
"numeric",   #TT4                       
"numeric",   #T4U                       
"numeric",   #FTI                       
"factor",    #referral_source           
"factor"))   #clase                     

names(data) <- c('caso','age','sex','on_thyroxine','query_on_thyroxine','on_antithyroid_medication','sick','pregnant','thyroid_surgery','I131_treatment','query_hypothyroid','query_hyperthyroid','lithium','goitre','tumor','hypopituitary','psych','TSH','T3','TT4','T4U','FTI','referral_source','clase')

library(car)
data$referral_source_recoded <- recode(data$referral_source, '1=1;2=0;3=0;4=0;5=0')

modelo.0.none <- glm(clase ~ 1, family = binomial, data = data)

modelo.1.full <- glm(clase ~ age + sex + on_thyroxine + query_on_thyroxine + on_antithyroid_medication + sick + pregnant + thyroid_surgery + I131_treatment + query_hypothyroid + query_hyperthyroid + lithium + goitre + tumor + hypopituitary + psych + TSH + T3 + TT4 + T4U + FTI + referral_source, family = binomial, data = data)

## T3                         -5.511e+00  7.704e-01  -7.154 8.44e-13 ***
## TSH                        -1.393e-01  5.035e-02  -2.766  0.00567 ** 
## query_hypothyroid1          1.763e+00  8.599e-01   2.051  0.04029 *  
## sick1                       1.877e+00  9.351e-01   2.007  0.04471 *  

modelo.2.fw <- step(modelo.0.none, scope=list(lower=modelo.0.none, upper=modelo.1.full), direction="forward")

## glm(formula = clase ~ T3 + TSH + referral_source + query_hypothyroid + TT4 + hypopituitary + query_hyperthyroid + sick, family = binomial, data = data)

## T3                  -5.541e+00  7.289e-01  -7.602 2.92e-14 ***
## TT4                  2.678e-02  9.885e-03   2.709  0.00675 ** 
## TSH                 -1.500e-01  4.644e-02  -3.229  0.00124 ** 
## query_hyperthyroid1  2.138e+00  9.837e-01   2.173  0.02976 *  
## query_hypothyroid1   1.749e+00  7.979e-01   2.193  0.02834 *  
## sick1                1.629e+00  9.072e-01   1.796  0.07252 .  

modelo.3.bw <- step(modelo.1.full, direction="backward")

## glm(formula = clase ~ sick + query_hypothyroid + query_hyperthyroid + hypopituitary + TSH + T3 + TT4 + referral_source, family = binomial, data = data)
  
## T3                  -5.541e+00  7.289e-01  -7.602 2.92e-14 ***
## TT4                  2.678e-02  9.885e-03   2.709  0.00675 **
## TSH                 -1.500e-01  4.644e-02  -3.229  0.00124 ** 
## query_hyperthyroid1  2.138e+00  9.837e-01   2.173  0.02976 *
## query_hypothyroid1   1.749e+00  7.979e-01   2.193  0.02834 *  
## sick1                1.629e+00  9.072e-01   1.796  0.07252 .

modelo.4.recoding <- glm(formula = clase ~ sick + query_hypothyroid + query_hyperthyroid + hypopituitary + TSH + T3 + TT4 + referral_source_recoded, family = binomial, data = data)

## glm(formula = clase ~ sick + query_hypothyroid + query_hyperthyroid + hypopituitary + TSH + T3 + TT4 + referral_source_recoded, family = binomial, data = data)

## T3                       -5.963e+00  6.942e-01  -8.589  < 2e-16 ***
## TSH                      -1.453e-01  4.363e-02  -3.330 0.000867 ***
## TT4                       2.314e-02  8.892e-03   2.603 0.009246 ** 
## query_hypothyroid1        1.459e+00  6.931e-01   2.105 0.035258 *  
## query_hyperthyroid1       1.515e+00  9.114e-01   1.663 0.096388 .  
## hypopituitary1            2.266e+01  1.075e+04   0.002 0.998319    
## sick1                     1.207e+00  7.425e-01   1.626 0.104010    
## referral_source_recoded1 -1.785e+01  1.183e+03  -0.015 0.987963    

set.seed(12345)
sample_size <- floor(0.70 * nrow(data))
train_indices <- sample(seq_len(nrow(data)), size = sample_size)

training          <- data[train_indices,]
testing           <- data[-train_indices,]
testing.sin.clase <- testing[, -which(names(testing) %in% c("clase"))]

modelo.final <- glm(formula = clase ~ sick + query_hypothyroid + query_hyperthyroid + hypopituitary + TSH + T3 + TT4 + referral_source_recoded, family = binomial, data = training)

predicciones.testing.modelo.2.fw       <- predict(modelo.2.fw, testing.sin.clase)
predicciones.testing.modelo.3.bw       <- predict(modelo.3.bw, testing.sin.clase)
predicciones.testing.modelo.4.recoding <- predict(modelo.4.recoding, testing.sin.clase)
predicciones.testing.modelo.final      <- predict(modelo.final, testing.sin.clase)

## tabla.2.fw <- ifelse(predicciones.testing.modelo.2.fw >= .5, 1, 0)
## table("predicho"=tabla.2.fw, "observado"=testing[,"clase"])
## tabla.3.bw <- ifelse(predicciones.testing.modelo.3.bw >= .5, 1, 0)
## table("predicho"=tabla.3.bw, "observado"=testing[,"clase"])
## tabla.4.recoding <- ifelse(predicciones.testing.modelo.4.recoding >= .5, 1, 0)
## table("predicho"=tabla.4.recoding, "observado"=testing[,"clase"])

tabla.final <- ifelse(predicciones.testing.modelo.final >= .5, 1, 0)
table("predicho"=tabla.final, "observado"=testing[,"clase"])

## predicho   0   1
##        0 269   7
##        1   2   9

library(pROC)

prob.pred.testing <- predict(modelo.final, testing.sin.clase, type = c("response"))
g.testing <- roc(clase ~ prob.pred.testing, data = testing)
plot(g.testing, col = "red")

prob.pred.training <- predict(modelo.final, type = c("response"))
g.training <- roc(clase ~ prob.pred.training, data = training)
lines(g.training, col = "blue")
legend("bottomright", c("training", "testing"), col = c("blue", "red"), lty = 1)

## wald: tester el efecto total de una predictora
## library(aod)
## coef(modelo)
## coeficientes empiezan en 1 con intercept
## wald.test(b = coef(modelo), Sigma = vcov(modelo), Terms = 4:6)


## plot
##
## plot(vx,vy,xlab="vx",ylab="Probability of vy") # 
## g <- glm(vy ~ vx,family=binomial,dat)
## curve(predict(g,data.frame(vx=x),type="resp"),add=TRUE)
## points(vy,fitted(g),pch=20)

## para interpretar los coeficientes:
##
## exp(coef(modelo))
## (Intercept)         gre         gpa       rank2       rank3       rank4 
##      0.0185      1.0023      2.2345      0.5089      0.2618      0.2119
##
## con intervalos de confianza
## exp(cbind(OR = coef(modelo), confint(modelo)))
##                 OR   2.5 % 97.5 %
## (Intercept) 0.0185 0.00189  0.167
## gre         1.0023 1.00014  1.004
## gpa         2.2345 1.17386  4.324
## rank2       0.5089 0.27229  0.945
## rank3       0.2618 0.13164  0.512
## rank4       0.2119 0.09072  0.471
## for a one unit increase in gpa, the odds of being admitted to graduate school (versus not being admitted) increase by a factor of 2.2345





## selección de variables con all subsets
##
## glmulti.lm.out <-
##     glmulti(bwt ~ age + lwt + race.cat + smoke + preterm + ht + ui + ftv.cat, data = lbw,
##             level = 1,               # No interaction considered
##             method = "h",            # Exhaustive approach
##             crit = "aic",            # AIC as criteria
##             confsetsize = 5,         # Keep 5 best models
##             plotty = F, report = F,  # No plot or interim reports
##             fitfunction = "glm")     # lm function
##
## ## Show 5 best models (Use @ instead of $ for an S4 object)
## glmulti.lm.out@formulas
##
## Show result for the best model
## summary(glmulti.lm.out@objects[[1]])



## Backward Elimination

## This is the simplest of all variable selection procedures and can be easily implemented without special
## software. In situations where there is a complex hierarchy, backward elimination can be run manually while taking account of what variables are eligible for removal.
## 1. Start with all the predictors in the model
## 2. Remove the predictor with highest p-value greater than αcrit
## 3. Refit the model and goto 2
## 4. Stop when all p-values are less than αcrit.
## The αcrit is sometimes called the “p-to-remove” and does not have to be 5%. If prediction performance
## is the goal, then a 15-20% cut-off may work best, although methods designed more directly for optimal
## prediction should be preferred.

## Forward Selection
##
## This just reverses the backward method.
## 1. Start with no variables in the model.
## 2. For all predictors not in the model, check their p-value if they are added to the model. Choose the one with lowest p-value less than αcrit .
## 3. Continue until no new predictors can be added.





> data[8452,caso]
       caso AAGE ACLSWKR_C ADTIND_C ADTOCC_C AHGA_C AHRSPAY AHSCOL_C AMARITL_C
 167842   65         5        1        1     10       0        3         3
 AMJIND_C AMJOCC_C ARACE_C AREORGN_C ASEX_C AUNMEM_C AUNTYPE_C AWKSTAT_C
        2       12       5         9      M        9         6         1
 CAPGAIN CAPLOSS DIVVAL FILESTAT_C GRINREG_C GRINST_C HHDFMX_C HHDREL_C
   99999       0   7500          4         6        0        1        5
 MIGMTR1_C MIGMTR3_C MIGMTR4_C MIGSAME_C MIGSUN_C NOEMP PARENT_C PEFNTVTY_C
         6         7         8         1        9     1        9          2
 PEMNTVTY_C PENATVTY_C PRCITSHP_C SEOTR_C VETQVA_C VETYN_C WKSWORK Clase
          2          2          5       2        9       2      52     0
 AMJOCC_C_061214 AMJOCC_C_030810 ADTOCC_C_0203
               1               0             0
> data[9223,]
   caso AAGE ACLSWKR_C ADTIND_C ADTOCC_C AHGA_C AHRSPAY AHSCOL_C AMARITL_C
 183334   37         9        4        5     10       0        3         3
 AMJIND_C AMJOCC_C ARACE_C AREORGN_C ASEX_C AUNMEM_C AUNTYPE_C AWKSTAT_C
        7       15       5         1      F        9         6         3
 CAPGAIN CAPLOSS DIVVAL FILESTAT_C GRINREG_C GRINST_C HHDFMX_C HHDREL_C
   99999       0      0          3         6        0        1        5
 MIGMTR1_C MIGMTR3_C MIGMTR4_C MIGSAME_C MIGSUN_C NOEMP PARENT_C PEFNTVTY_C
         1         1         1         9        8     0        9          2
 PEMNTVTY_C PENATVTY_C PRCITSHP_C SEOTR_C VETQVA_C VETYN_C WKSWORK Clase
          2          2          5       0        9       2       0     0
 AMJOCC_C_061214 AMJOCC_C_030810 ADTOCC_C_0203
               0               0             0


9223
8452

testing_sin_clase <- testing[,-41]
predicciones <- predict(modelo.fw.recodificado, testing_sin_clase, type = c("response"))
predicciones_02 <- round(predicciones,2)
predicciones_02_ordenadas_25_porciento <- sort(predicciones_02)[3379:4503]
length(which(predicciones_02_ordenadas_25_porciento >= 0.5))





cutoff <- 4 / (nrow(data) - length(modelo.fw.recodificado$coefficients) - 2)
plot(modelo.fw.recodificado, which=4, cook.levels=cutoff)

influencePlot(modelo.fw.recodificado, id.method="identify")