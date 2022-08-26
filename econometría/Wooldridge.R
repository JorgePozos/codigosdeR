#Ejercicio Wooldridge
library(haven)
library(dplyr)

VOTE1 <- read_dta("Downloads/VOTE1.dta")
attach(VOTE1)
VOTE1

# Consideramos el modelo con interaccion entre los gastos 
modelo1 <- lm( 
  voteA ~ prtystrA + expendA + expendB + expendA:expendB,
VOTE1
  )

pronosticos = predict(modelo1)
data = data.frame(VOTE1$voteA,
                  pronosticos)
data <- data %>% 
  summarise( RMSE = sqrt(mean((VOTE1.voteA - pronosticos)^2)))

data

2.041241
## inciso 2
# Estimamos la ecuación a partir de los sig datos
# Vemos que la interaccion no es significativa 
summary(modelo1)

## inciso 3
#Determine el promedio de expendA en la muestra 
VOTE1 %>% 
  summarise(promedio = mean(expendA))

#Fijar expendA en 300 y veamos el efecto de aumentar expendB 100 unidades 
nuevos_datos <- data.frame(
  prtystrA = .342,
  expendA = 300,
  expendB=.0317 
)
nuevos_datos100 <- data.frame(
  prtystrA = .342,
  expendA = 300,
  expendB=.0317 + 100
)

#Efecto estimado  
efecto_estimado <- predict(modelo1, nuevos_datos100) - predict(modelo1, nuevos_datos) 
efecto_estimado

## inciso 4
#Fijar expendB en 100 y veamos el efecto de aumentar expendA 100 unidades 
nuevos_datosa <- data.frame(
  prtystrA = .342,
  expendA = .0383,
  expendB= 100  
)
nuevos_datos100a <- data.frame(
  prtystrA = .342,
  expendA = .0383 + 100,
  expendB= 100 
)

#Efecto estimado  
efecto_estimado <- predict(modelo1, nuevos_datos100a) - predict(modelo1, nuevos_datosa) 
efecto_estimado

## inciso 5
#consideramos el modelo con shareA en lugar de la interaccion 
modelo2 <- lm( 
  voteA ~ prtystrA + expendA + expendB + shareA,
  VOTE1
)
summary(modelo2)
