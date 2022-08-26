rm(list=ls()) #Remueve todos los objetos el escritorio
library(lmtest) #para coeftest() y bptest().
library(broom) #para glance() y tidy()
library(car) #para hccm() errores estándar robustos
library(sandwich)
library(knitr)
library(stargazer)
library(haven)

library(readxl)
food <- read_excel("Downloads/food.xlsx")
attach(food)

mod1 <- lm(food_exp~income, data=food)
plot(food$income,food$food_exp, type="p",
     xlab="Presupuesto", ylab="Gasto semanal en alimentos")
abline(mod1)
res <- residuals(mod1)
yhat <- fitted(mod1)
plot(food$income,res, xlab="Presupuesto", ylab="errores")
plot(yhat,res, xlab="Valores ajustados", ylab="errores")
 #####################3
alpha <- 0.05
# errores cuadrados
ressq <- resid(mod1)^2

# paso 2 
mdr <- lm(ressq~income, data = food)
N <- nobs(mdr)
gmdr <- glance(mdr)
s <- gmdr$df
chisq <- qchisq(1-alpha, s-1 )
rs2 <- gmdr$r.squared
chisq1 <- N * rs2
Pvalue <- 1 - pchisq(chisq, s-1)

#ahora con white 
mdres1 <- lm(ressq~income+I(income^2), data=food)
gmdres1 <- glance(mdres1)
Rsq <- gmdres1$r.squared
S <- gmdres1$df #Number of Betas in model
chisq <- N*Rsq
pval <- 1-pchisq(chisq, S-1)

foodeq <- lm(food_exp~income, data=food)
tst <- gqtest(foodeq, point=0.5, alternative="greater",
              order.by=food$income)
kable(tidy(tst),
      caption="Prueba GQ para el ejemplo de comida")


foodeq <- lm(food_exp~income,data=food)

kable(tidy(foodeq),caption=
        "Errores estándar en la regresión de comida")##Errores robustos##

cov1 <- hccm(foodeq, type="hc1") #se necesita el paquete "car"

food.HC1 <- coeftest(foodeq, vcov.=cov1)

kable(tidy(food.HC1),caption=
        
        "Robust (HC1) standard errors in the 'food' equation")


