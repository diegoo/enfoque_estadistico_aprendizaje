# logit(P(Y=1|X1,..,Xp)) = log(P(Y=1|X1,..,Xp) / 1−P(Y=1|X1,..,Xp)) = β0 + β1X1 + ... + βpXp

library(ResourceSelection)
set.seed(43657)
n <- 100
x <- rnorm(n)
xb <- x
pr <- exp(xb)/(1+exp(xb))
y <- 1*(runif(n) < pr)
mod <- glm(y~x, family=binomial)

hl.test <- hoslem.test(mod$y, fitted(mod), g=10)
hl.test

# cantidad de grupos óptima debería ser cantidad de predictoras + 1
# para probar todos:

for (i in 5:15) {
  print(hoslem.test(mod$y, fitted(mod), g=i)$p.value)
}

# p-valor bajo: modelo es malo.
# p-valor alto: modelo no es necesariamente bueno; pero no hay suficiente evidencia en contra de H0.

# ver los valores de los grupos de hosmer-lemeshow
# cbind(hl$observed,hl$expected)
