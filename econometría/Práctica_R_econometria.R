library(ggplot2)
library(readxl)
library(tibble)
library(dplyr)
library(xtable)
library(knitr)
#importamos los datos 
food <- read_excel("Downloads/food.xlsx")

attach(food)

names(food)
summary(food$food_exp)
summary(food$income)

#histograma de gasto en comida 
hist(food_exp,main = paste("Histograma de Gasto en Comida "))

#Resión lineal simple 
income_vs_food_exp <- lm(food_exp ~ income , food)
summary(income_vs_food_exp)
income_vs_food_exp

#predicción
pre_income <- tibble(
  income = 20 
)

pre_estatica

#parejas de puntos 
pronosticos <- pre_income %>%
  mutate(
    food_exp =   predict(income_vs_food_exp , pre_income)
  )
pronosticos

#gráfica de la recta ajustada a los puntos 
ggplot( food, aes( income , food_exp )) + geom_point() + 
  geom_smooth(method = 'lm',se = FALSE)  + geom_point(data = pronosticos, color = 'green')

#trabajo de submuestras 
N <-  nrow(food)
C <- 50
S <- 38

sumb2 <- 0
for (i in 1:C){   # Un bucle sobre el número de submuestras
  set.seed(3*i)   #resultados de cada submuestra
  submuestra <- food[sample(1:N, size=S, replace=TRUE), ]
  mod2 <- lm(food_exp~income, data=submuestra)
  #sum b2 para todas las submuestras:
  sumb2 <- sumb2 + coef(mod2)[[2]]
}
print(sumb2/C, digits = 3)

#Obtener varianzas 
(varb1 <- vcov(income_vs_food_exp)[1,1])
(varb2 <- vcov(income_vs_food_exp)[2,2])
(covb1b2 <- vcov(income_vs_food_exp)[1,2])

#intervalo de confianza 
alpha <- .05 
mod1 <- lm(food_exp~income, data = food)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
df <- df.residual(mod1)
smod1<- summary(mod1)
seb2<- coef(smod1)[2,2]
tc <- qt(1- alpha/2,df)
lowb <- b2-tc*seb2
ubp <- b2+tc*seb2


ci <- confint(mod1)
print(ci)
pronosticos