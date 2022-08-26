library(plm)
library(tseries)
library(dynlm)
library(vars)
library(nlWaldTest)
library(lmtest)
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(systemfit)
library(AER)
library(xtable)
library(haven)

nls_panel <- read_dta("Downloads/nls_panel.dta")
attach(nls_panel)

# Declaramos datos panel y mostramos un par de años en una tabla 
nlspd <- pdata.frame(nls_panel ,  index = c('id','year'))
smpl <- nlspd[nlspd$id %in% c(1,2),c(1:6, 14:15) ]
tbl <- xtable(smpl)
kable(tbl, digits= 4 , align = 'c', caption = 'muestra')

# Verificamos que sea un panel balanceado 
pdim(nlspd)

#Modelo de datos panel para el salario 
wage.pooled <- plm(lwage ~ educ + exper+ I(exper^2)+ tenure + I(tenure^2)+ black+south+union,
                   data = nls_panel)
kable(tidy(wage.pooled), digits = 3, caption = 'Modelo para el Salario')
summary(wage.pooled)

nls10 <- pdata.frame(nls_panel[nls_panel$id %in% 1:10,])
age.fixed <- lm(lwage ~ exper + I(exper^2)+ tenure+ I(tenure^2) + union + 
                  factor(id) - 1 , data = nls10)
summary(age.fixed)
kable(tidy(age.fixed), digits = 3, caption = 'Modelo de efectos fijos')

#Modelo de efectos fijos sin crear variables dummy 
wage.within <- plm(lwage ~ exper + I(exper^2) + tenure + I(tenure^2) + south + union,
                  data = nlspd, 
                  model = 'within')
tbl <- tidy(wage.within )
kable(tbl, digist = 5 , caption = 'Modelo de efectos dijos de estimador hacia adentro')
summary(wage.within)

#Ahora con 10 individuos
wage10.within <- plm(lwage ~ exper + I(exper^2) + tenure + I(tenure^2) + south + union,
                   data = nls10, 
                   model = 'within')
tbl <- tidy(wage.within )
kable(tbl, digist = 5 , caption = 'Modelo de efectos dijos de estimador hacia adentro, 10 individuos')
summary(wage10.within)

