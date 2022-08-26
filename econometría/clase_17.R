######### clase 17 
library(haven)
stockton96 <- read_dta("Downloads/stockton96.dta")
attach(stockton96)

modelo_1 <- lm(price ~ sqft + age)
summary(modelo_1)
  

