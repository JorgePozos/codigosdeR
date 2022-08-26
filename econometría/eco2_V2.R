#videoconferecia 2 
#7 enero 2022
#Jorge U. Barragan Pozos 
library(stargazer)
library(knitr)
library(broom)
library(haven)
library(stats)
library(lmtest)

library(xtable)
library(printr)
library(effects)
library(car)
library(AER)
library(haven)

#importamos los datos 
edu_inc <- read_dta("Downloads/edu_inc.dta")
attach(edu_inc)

#definicion de los modelos 
mod1 <- lm(
  faminc~he+we,
  edu_inc
)
summary(mod1)

mod2 <- lm(
  faminc~he,
  edu_inc
)
summary(mod2)

mod3 <- lm(
  faminc~he+we+kl6,
  edu_inc
)
summary(mod3)

mod4 <- lm(
  faminc~he+we+kl6+xtra_x5+xtra_x6,
  edu_inc
)
summary(mod4)

#comparativo de los cuatro modelos 
stargazer(mod1, mod2, mod3, mod4, type="text", title = "Comparación modelos", align = TRUE, out ="tabla1.txt", flip = TRUE, digits = 1)

#hacer un pdf explicando la diferencia entre variable omitidas y variables irrelevantes 
#cual es la diferencia entre las variables omitidas y las variables irrelevantes 

# Estadisticos de informacion 
r1 <- as.numeric(glance(mod1))
r2 <- as.numeric(glance(mod2))
r3 <- as.numeric(glance(mod3))
r4 <- as.numeric(glance(mod4))

tab <- data.frame(rbind(r1 , r2, r3, r4))[,c(1,2,8,9)]
row.names(tab) <- c("he","he","we","he","we","kl6","he","we","kl6","xtra_x5","xtra_x6")
kable(tab,caption="Comparación de los modelos, 'faminc' ", digits=4,col.names=c("Rsq","AdjRsq","AIC","BIC"))

# Prueba Reset 
smod1 <- summary(mod1)
Rsq <- smod1$r.squared
AdjRsq <- smod1$adj.r.squared
aic <- AIC(mod1)
bic <- BIC(mod1)
c(Rsq, AdjRsq, aic, bic)

## Reset al modelo 3 
resettest(mod3, power = 2)
resettest(mod3, power = 2:3)

# Predicción y pronóstico 

andy <- read_dta("Downloads/andy.dta")
attach(andy)

predpoint <- data.frame(
  price = 6,
  advert = 1.9
)

mod5 <- lm(sales~price + advert + I(advert^2))
summary(mod5)
predict(mod5, newdata = predpoint, interval = 'predict')

