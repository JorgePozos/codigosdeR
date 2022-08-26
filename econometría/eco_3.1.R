#Ejercicio 3.1####################
library(ggplot2)
library(dplyr)
library(readxl)
food <- read_excel("Downloads/food.xlsx")
attach(food)


#Construimos Intervalo de Confianza para beta1#######
mod1 <- lm(food_exp ~ income)

b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
p_valor <- .0622/2
p#En este caso N = 40 y K = 2.
#Así, consideramos N - K = df = 38 grados de libertad.
df = 38
alpha <- 0.05

se <- (sqrt((varb1 <- vcov(mod1)[1, 1]))) #se(b1)
tc <- qt(1-alpha, df) #área de rechazo.
tc #el percentil 97.5

t_es <- b1/se
t_es <= abs(tc) #no rechazamos H0
#gráfica de la región de rechazo
x_dt <- seq(- 5, 5, by = 0.01)
y_dt <- dt(x_dt, df = 2)
data <- tibble(x_dt, y_dt)
ggplot(data,aes(x_dt, y_dt)) + geom_point() + 
  geom_vline(xintercept = c(tc), color='red') +
  geom_vline(xintercept = p_valor, color = 'green')


