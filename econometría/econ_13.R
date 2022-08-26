########## Utown Regresión ################
library(bookdown)
library(knitr)  #for referenced tables with kable()
library(xtable) #makes data frame for kable
library(printr) #automatically prints output nicely
library(effects)
library(car)
library(AER)
library(broom) #for tidy lm output and function glance()
library(stats)
library(lmtest)#for coeftest() and other test functions 
library(stargazer) #nice and informative tables


library(haven)
utown <- read_dta("Downloads/utown.dta")
attach(utown)

######## Declarar la Variables Dummy ###########
utown$utown <- as.factor(utown$utown)
utown$pool <- as.factor(utown$pool)
utown$fplace <- as.factor(utown$fplace)

kable(summary.data.frame(utown),
      caption = "summary for 'utown' dataset")

########## Realizamos el Modelo ##########
mod_utown <- lm(price~utown*sqft + age + pool + fplace,
                data = utown)
###############tabla 
kable(tidy(
  mod_utown,
  caption = "Modelo de los precios de las casas"
))


