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
library(haven)
nls_panel <- read_dta("Downloads/nls_panel.dta")
attach(nls_panel)
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
nlspd<-pdata.frame(nls_panel,index=c("id","year"))
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
################################################# VIDEOCONFERENCIA 23

kable(tidy(pFtest(wage.within,wage.pooled)),caption = "Prueba de efectos fijos: Ho Efectos Fijos")

##Errores Robustos
tb1<- tidy(coeftest(wage.pooled,vcov=vcovHC(wage.pooled,type = "HC0", cluster = "group")))
kable(tb1,digits = 5,caption = "Modelo de datos panel con errores est robustos")

################################################# VIDEOCONFERENCIA 24   
# JORGE U. BARRAGAN POZOS 
wageretest<-plmtest(wage.pooled,effect = "individual")
kable(tidy(wageretest),caption = "Prueba para la ec del salario para los efectos aleatorios" )
#Rechazamos H0 de que la heterogene está presente en los individuos

wage.random <- plm(lwage~exper+I(exper^2)+tenure+black+I(tenure^2)+south+union,data = nlspd,random.method = "swar",model = "random")
kable(tidy(wage.random),digits=4, caption="modelo de efectos Aleatorios")
