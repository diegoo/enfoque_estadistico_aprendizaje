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
