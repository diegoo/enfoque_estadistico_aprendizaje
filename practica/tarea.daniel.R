library(foreign)
dataset <- read.spss('GSS_EEA2010_minimod.sav', to.data.frame=TRUE)

names(dataset)

#  [1] "id"       "educ"     "ingfam"   "horastv"  "sexo"     "sexo1"   
#  [7] "sexo2"    "sitlab"   "ecivil"   "edadboda" "hermanos" "hijos"   
# [13] "edad"     "titestud" "titpadre" "titmadre" "ingper"   "penacap" 
# [19] "leyarmas" "legdroga" "vida"     "cachete"  "ciencias" "amantes" 
# [25] "frecsex"  "indsocec" "relig"

summary(dataset$sexo)
# Hombre  Mujer 
#    631    844 

summary(dataset$sexo1)
# Mujer Hombre 
#   844    631 

summary(dataset$sexo2)
# Hombre  Mujer 
#    631    844 

fit_sexo1 <- lm(horastv ~ educ + sexo1, data=dataset)
summary(fit_sexo1)

# Call:
# lm(formula = horastv ~ educ + sexo1, data = dataset)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6.8197 -1.9445 -0.4007  1.1583 12.9155 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 12.92256    0.28864  44.771  < 2e-16 ***
# educ        -0.68380    0.02189 -31.241  < 2e-16 ***
# sexo1Hombre -0.40475    0.14978  -2.702  0.00696 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 2.819 on 1472 degrees of freedom
# Multiple R-squared:  0.4106,    Adjusted R-squared:  0.4098 
# F-statistic: 512.7 on 2 and 1472 DF,  p-value: < 2.2e-16

coefficients(fit_sexo1)

# (Intercept)        educ sexo1Hombre 
#  12.9225562  -0.6838044  -0.4047501 

fit_sexo <- lm(horastv ~ educ + sexo, data=dataset)
summary(fit_sexo)

# Call:
# lm(formula = horastv ~ educ + sexo, data = dataset)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6.8197 -1.9445 -0.4007  1.1583 12.9155 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 12.51781    0.31309  39.982  < 2e-16 ***
# educ        -0.68380    0.02189 -31.241  < 2e-16 ***
# sexoMujer    0.40475    0.14978   2.702  0.00696 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 2.819 on 1472 degrees of freedom
# Multiple R-squared:  0.4106,    Adjusted R-squared:  0.4098 
# F-statistic: 512.7 on 2 and 1472 DF,  p-value: < 2.2e-16

fit_sexo2 <- lm(horastv ~ educ + sexo2, data=dataset)
summary(fit_sexo2)

# Call:
# lm(formula = horastv ~ educ + sexo2, data = dataset)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6.8197 -1.9445 -0.4007  1.1583 12.9155 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 12.51781    0.31309  39.982  < 2e-16 ***
# educ        -0.68380    0.02189 -31.241  < 2e-16 ***
# sexo2Mujer   0.40475    0.14978   2.702  0.00696 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 2.819 on 1472 degrees of freedom
# Multiple R-squared:  0.4106,    Adjusted R-squared:  0.4098 
# F-statistic: 512.7 on 2 and 1472 DF,  p-value: < 2.2e-16

coefficients(fit_sexo2)

# (Intercept)        educ	sexo2Mujer 
#  12.5178061  -0.6838044   	0.4047501

library(plyr)
dataset$sexo2_recodificado <- revalue(dataset$sexo2, c("Hombre"="1", "Mujer"="2"))
fit_sexo2_recodificado <- lm(horastv ~ educ + sexo2_recodificado, data=dataset)
summary(fit_sexo2_recodificado)

# Call:
# lm(formula = horastv ~ educ + sexo2_recodificado, data = dataset)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6.8197 -1.9445 -0.4007  1.1583 12.9155 
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         12.51781    0.31309  39.982  < 2e-16 ***
# educ                -0.68380    0.02189 -31.241  < 2e-16 ***
# sexo2_recodificado2  0.40475    0.14978   2.702  0.00696 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 2.819 on 1472 degrees of freedom
# Multiple R-squared:  0.4106,    Adjusted R-squared:  0.4098 
# F-statistic: 512.7 on 2 and 1472 DF,  p-value: < 2.2e-16

coefficients(fit_sexo2_recodificado)
# (Intercept)                educ	sexo2_recodificado2 
#  12.5178061          -0.6838044       0.4047501

# recodificando al revés:

dataset$sexo2_recodificado <- revalue(dataset$sexo2, c("Hombre"="2", "Mujer"="1"))
fit_sexo2_recodificado <- lm(horastv ~ educ + sexo2_recodificado, data=dataset)
summary(fit_sexo2_recodificado)

# Call:
# lm(formula = horastv ~ educ + sexo2_recodificado, data = dataset)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6.8197 -1.9445 -0.4007  1.1583 12.9155 
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         12.51781    0.31309  39.982  < 2e-16 ***
# educ                -0.68380    0.02189 -31.241  < 2e-16 ***
# sexo2_recodificado1  0.40475    0.14978   2.702  0.00696 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 2.819 on 1472 degrees of freedom
# Multiple R-squared:  0.4106,    Adjusted R-squared:  0.4098 
# F-statistic: 512.7 on 2 and 1472 DF,  p-value: < 2.2e-16

coefficients(fit_sexo2_recodificado)
# (Intercept)                educ sexo2_recodificado1 
#  12.5178061          -0.6838044           0.4047501


# residuals(fit) 			
# anova(fit)
# confint(fit, level=0.95)		# CIs for model parameters 
# fitted(fit)  				# predicted values
# vcov(fit)				# covariance matrix for model parameters 
# influence(fit)			# regression diagnostics
# layout(matrix(c(1,2,3,4),2,2)) 	# optional 4 graphs/page, diagnostic plots provide checks for heteroscedasticity, normality, and influential observerations.
# plot(fit)
# fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
# fit2 <- lm(y ~ x1 + x2)
# anova(fit1, fit2)			# compare models