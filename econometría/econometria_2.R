
mod1 <- lm(food_exp ~ income)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
smod1 <- summary(mod1)
smod1
(varb1 <- vcov(mod1)[1, 1])
(varb2 <- vcov(mod1)[2, 2])
(covb1b2 <- vcov(mod1)[1,2])
alpha <- 0.05
x <- 20 # el valor del presupuesto
tcr <- qt(1-alpha/2, df) # área de rechazo.
df <- df.residual(mod1)
L <- b1+b2*x # estimated L
varL = varb1 + x^2 * varb2 + 2*x*covb1b2 # var(L)
seL <- sqrt(varL) # standard error of L
lowbL <- L-tcr*seL
upbL <- L+tcr*seL

library(xtable)
library(knitr)

#para el caso de FOOD_EXP=250 e INCOME=2000, dos colas
c <- 250
alpha <- 0.05
t <- (L-c)/seL # t < tcr --> Rechazamos Ho.
tcr <- qt(1-alpha/2, df)
#También podemos calcular el valor p:
p_value <- 2*(1-pt(abs(t), df)) #p<alpha -> Reject Ho
p_value # Jorge Barragan