#videoconferencia 27
library(knitr)
library(systemfit)
library(broom)
library(haven)

#cargamos datos 
truffles <- read_dta("Downloads/truffles.dta")
attach(truffles)

#definimos oferta y demanda
demanda <- q~ p + ps + di
oferta <- q~ p + pf

lista <- list( demanda, oferta)

instrumentos <- ~ps + di + pf

m_trufa <- systemfit(lista, inst = instrumentos, method = '2SLS',data= truffles)
summary(m_trufa)

#Formas estructurales para la oferta y la demanda
q_red <- lm(q~ p + ps + di, truffles)
p_red <- lm(q~ ps + di + pf, truffles)

kable(tidy(q_red), digits = 4, caption = 'Forma reducida de la cantidad')
kable(tidy(p_red), digits = 4, caption = 'Forma reducida del precio')
#Jore U. Barragan Pozos 
