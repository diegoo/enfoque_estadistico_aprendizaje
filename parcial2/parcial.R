data <- read.csv('census_examen_muestra.dat',sep='\t',colClasses=c("numeric","numeric","factor","factor","factor","factor","numeric","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","numeric","factor","factor","factor","factor","factor","factor","factor","factor","numeric","factor"))

# dividir training/test

set.seed(12345)
sample_size <- floor(0.70 * nrow(data))
train_indices <- sample(seq_len(nrow(data)), size = sample_size)
training          <- data[train_indices,]
testing           <- data[-train_indices,]
testing.sin.clase <- testing[, -which(names(testing) %in% c("Clase"))]

# modelos

modelo.vacio <- glm(Clase ~ 1, family = binomial, data = training)

modelo.full <- glm(Clase ~ AAGE + ACLSWKR_C + ADTIND_C + ADTOCC_C + AHGA_C + AHRSPAY + AHSCOL_C + AMARITL_C + AMJIND_C + AMJOCC_C + ARACE_C + AREORGN_C + ASEX_C + AUNMEM_C + AUNTYPE_C + AWKSTAT_C + CAPGAIN + CAPLOSS + DIVVAL + FILESTAT_C + GRINREG_C + GRINST_C + HHDFMX_C + HHDREL_C + MIGMTR1_C + MIGMTR3_C + MIGMTR4_C + MIGSAME_C + MIGSUN_C + NOEMP + PARENT_C + PEFNTVTY_C + PEMNTVTY_C + PENATVTY_C + PRCITSHP_C + SEOTR_C + VETQVA_C + VETYN_C + WKSWORK, family = binomial, data = training)

modelo.fw <- step(modelo.vacio, scope=list(lower=modelo.vacio, upper=modelo.full), direction="forward")

## con muestra
## Clase ~ AHGA_C + ADTOCC_C + CAPGAIN + HHDFMX_C + ASEX_C + DIVVAL + WKSWORK + AAGE + CAPLOSS + NOEMP + ACLSWKR_C + PEMNTVTY_C + AMJOCC_C + SEOTR_C + ADTIND_C + AUNMEM_C + AHSCOL_C

## ADTOCC_C2   -1.232e+00  2.784e-01  -4.425 9.66e-06 ***
## ADTOCC_C3   -9.795e-01  3.172e-01  -3.088 0.002015 ** 
## CAPGAIN      1.431e-04  1.742e-05   8.215  < 2e-16 ***
## HHDFMX_C9   -1.745e+00  3.362e-01  -5.189 2.11e-07 ***
## ASEX_CM      1.033e+00  1.402e-01   7.368 1.73e-13 ***
## DIVVAL       1.723e-04  2.240e-05   7.692 1.45e-14 ***
## WKSWORK      4.496e-02  6.638e-03   6.773 1.26e-11 ***
## AAGE         3.014e-02  4.765e-03   6.324 2.54e-10 ***
## CAPLOSS      6.578e-04  1.131e-04   5.815 6.06e-09 ***
## NOEMP        1.738e-01  3.354e-02   5.181 2.20e-07 ***
## ACLSWKR_C7  -1.348e+00  4.118e-01  -3.274 0.001059 ** 
## PEMNTVTY_C4 -1.918e+00  5.281e-01  -3.633 0.000280 ***
## AMJOCC_C11   2.581e+00  5.326e-01   4.845 1.26e-06 ***
## AMJOCC_C12   9.471e-01  3.806e-01   2.488 0.012829 *  
## AMJOCC_C14   9.803e-01  4.399e-01   2.229 0.025844 *
## AMJOCC_C6    8.896e-01  4.181e-01   2.128 0.033339 *
## AMJOCC_C13   1.025e+00  4.251e-01   2.411 0.015915 *  
## AMJOCC_C3    1.128e+00  3.853e-01   2.927 0.003423 ** 
## AMJOCC_C10   1.125e+00  3.690e-01   3.047 0.002308 ** 
## AMJOCC_C8    1.582e+00  4.313e-01   3.669 0.000244 ***
## SEOTR_C1     5.515e-01  2.542e-01   2.170 0.030044 *  
## SEOTR_C2    -4.128e-01  1.840e-01  -2.243 0.024869 *  
## AUNMEM_C9    3.754e-01  1.610e-01   2.333 0.019672 *  

library(pROC)
prob.pred.testing <- predict(modelo.fw, testing.sin.clase, type = c("response"))
g.testing <- roc(Clase ~ prob.pred.testing, data = testing)
plot(g.testing, col = "red")
## Area under the curve: 0.9365
prob.pred.training <- predict(modelo.fw, type = c("response"))
g.training <- roc(Clase ~ prob.pred.training, data = training)
lines(g.training, col = "blue")
legend("bottomright", c("training", "testing"), col = c("blue", "red"), lty = 1)

library(car)
data$AMJOCC_C_061214 <- recode(data$AMJOCC_C, '1=0;2=0;3=0;4=0;5=0;6=1;7=0;8=0;9=0;10=0;11=0;12=1;13=0;14=1;15=0')
data$AMJOCC_C_030810 <- recode(data$AMJOCC_C, '1=0;2=0;3=1;4=0;5=0;6=1;7=0;8=1;9=0;10=1;11=0;12=0;13=0;14=0;15=0')
data$ADTOCC_C_0203 <- recode(data$ADTOCC_C, '1=0;2=1;3=1;4=0;5=0;6=0')

modelo.fw.recodificado <- glm(Clase ~ ADTOCC_C_0203 + CAPGAIN + HHDFMX_C + ASEX_C + DIVVAL + WKSWORK + AAGE + CAPLOSS + NOEMP + ACLSWKR_C + AMJOCC_C_061214 + AMJOCC_C_030810 + SEOTR_C + + AUNMEM_C, family = binomial, data = training)

prob.pred.testing.2 <- predict(modelo.fw.recodificado, testing.sin.clase, type = c("response"))
g.testing.2 <- roc(Clase ~ prob.pred.testing.2, data = testing)

## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      -7.917e+00  5.237e-01 -15.116  < 2e-16 ***
## ADTOCC_C_02031   -1.285e+00  1.216e-01 -10.565  < 2e-16 ***
## CAPGAIN           1.618e-04  1.669e-05   9.694  < 2e-16 ***
## HHDFMX_C2        -1.984e-02  1.440e-01  -0.138  0.89037    
## HHDFMX_C3         5.937e-02  1.433e-01   0.414  0.67872    
## HHDFMX_C9        -2.012e+00  3.303e-01  -6.092 1.12e-09 ***
## ASEX_CM           1.222e+00  1.287e-01   9.495  < 2e-16 ***
## DIVVAL            2.186e-04  2.256e-05   9.687  < 2e-16 ***
## WKSWORK           4.756e-02  6.288e-03   7.563 3.93e-14 ***
## AAGE              2.893e-02  4.241e-03   6.823 8.94e-12 ***
## CAPLOSS           7.258e-04  1.032e-04   7.033 2.02e-12 ***
## NOEMP             2.214e-01  3.138e-02   7.056 1.71e-12 ***
## ACLSWKR_C2        1.455e-02  3.083e-01   0.047  0.96235    
## ACLSWKR_C3       -8.088e+00  2.600e+02  -0.031  0.97519    
## ACLSWKR_C4        6.251e-02  2.556e-01   0.245  0.80682    
## ACLSWKR_C5        8.918e-01  3.383e-01   2.637  0.00838 ** 
## ACLSWKR_C6        4.170e-01  3.227e-01   1.292  0.19633    
## ACLSWKR_C7       -9.235e-01  3.909e-01  -2.363  0.01815 *  
## ACLSWKR_C8       -1.159e+01  4.552e+02  -0.025  0.97969    
## ACLSWKR_C9        1.312e-01  3.717e-01   0.353  0.72403    
## AMJOCC_C_0612141  2.688e-01  1.477e-01   1.820  0.06880 .  
## AMJOCC_C_0308101  1.124e+00  1.215e-01   9.252  < 2e-16 ***
## SEOTR_C1          6.546e-01  2.367e-01   2.766  0.00568 ** 
## SEOTR_C2         -3.369e-01  1.739e-01  -1.937  0.05270 .  
## AUNMEM_C1        -2.345e-01  3.468e-01  -0.676  0.49902    
## AUNMEM_C9         2.462e-01  1.517e-01   1.623  0.10461    

modelo.fw.recodificado.bis <- glm(Clase ~ ADTOCC_C_0203 + CAPGAIN + HHDFMX_C + ASEX_C + DIVVAL + WKSWORK + AAGE + CAPLOSS + NOEMP + ACLSWKR_C + AMJOCC_C_061214 + AMJOCC_C_030810 + SEOTR_C + + AUNMEM_C, family = binomial, data = training)

> round(coef(modelo.fw.recodificado),2)
     (Intercept)   ADTOCC_C_02031          CAPGAIN        HHDFMX_C2 
           -7.92            -1.28             0.00            -0.02 
       HHDFMX_C3        HHDFMX_C9          ASEX_CM           DIVVAL 
            0.06            -2.01             1.22             0.00 
         WKSWORK             AAGE          CAPLOSS            NOEMP 
            0.05             0.03             0.00             0.22 
      ACLSWKR_C2       ACLSWKR_C3       ACLSWKR_C4       ACLSWKR_C5 
            0.01            -8.09             0.06             0.89 
      ACLSWKR_C6       ACLSWKR_C7       ACLSWKR_C8       ACLSWKR_C9 
            0.42            -0.92           -11.59             0.13 
AMJOCC_C_0612141 AMJOCC_C_0308101         SEOTR_C1         SEOTR_C2 
            0.27             1.12             0.65            -0.34 
       AUNMEM_C1        AUNMEM_C9 
           -0.23             0.25
	   
# coeficientes del modelo

    (Intercept)   ADTOCC_C_02031          CAPGAIN        HHDFMX_C2 
   -7.916822e+00    -1.284621e+00     1.618294e-04    -1.984182e-02
   
       HHDFMX_C3        HHDFMX_C9          ASEX_CM           DIVVAL 
    5.936836e-02    -2.012362e+00     1.222220e+00     2.185626e-04
    
         WKSWORK             AAGE          CAPLOSS            NOEMP 
    4.756063e-02     2.893191e-02     7.257466e-04     2.214378e-01
    
      ACLSWKR_C2       ACLSWKR_C3       ACLSWKR_C4       ACLSWKR_C5 
    1.455094e-02    -8.087957e+00     6.250623e-02     8.918100e-01
    
      ACLSWKR_C6       ACLSWKR_C7       ACLSWKR_C8       ACLSWKR_C9 
    4.169621e-01    -9.234997e-01    -1.158728e+01     1.312374e-01
    
AMJOCC_C_0612141 AMJOCC_C_0308101         SEOTR_C1         SEOTR_C2 
    2.687806e-01     1.124263e+00     6.546265e-01    -3.368805e-01
    
       AUNMEM_C1        AUNMEM_C9 
   -2.344741e-01     2.461912e-01




testing_sin_clase <- testing[,-41]
predicciones <- predict(modelo.fw.recodificado, testing_sin_clase, type = c("response"))
predicciones_02 <- round(predicciones,2)
predicciones_02_ordenadas_25_porciento <- sort(predicciones_02)[3379:4503]
length(which(predicciones_02_ordenadas_25_porciento >= 0.5))

# 107













modelo.bw <- step(modelo.full, direction="backward")



## coeficientes del modelo, con intervalos de confianza
## coef(modelo)
## exp(cbind(OR = coef(modelo), confint(modelo)))















 glmulti.lm.out <-
     glmulti(Clase ~ ADTOCC_C_0203 + CAPGAIN + HHDFMX_C + ASEX_C + DIVVAL + WKSWORK + AAGE + CAPLOSS + NOEMP + ACLSWKR_C + AMJOCC_C_061214 + AMJOCC_C_030810 + SEOTR_C + + AUNMEM_C, data = training,
             level = 1,               
             method = "h",            
             crit = "aic",            
             confsetsize = 2,         
             plotty = F, report = F,  
             fitfunction = "glm")     


# glmulti.lm.out <-
#     glmulti(bwt ~ , data = training,
#             level = 1,               
#             method = "h",            
#             crit = "aic",            
#             confsetsize = 5,         
#             plotty = F, report = F,  
#             fitfunction = "glm")     
# glmulti.lm.out@formulas
# summary(glmulti.lm.out@objects[[1]])
