library(ggplot2)
library(tibble)
library(dplyr)

#contruimos datos 
estatica <- c(  39, 37.5, 22.2, 17.5, .64, .45, 2.62, 2.36, 32, .77)
flujo   <- c(  23, 22.3, 9.4,   9.7, .15, .28,  .75,  .51, 28, .39)
toxicos <- data.frame(estatica, flujo)

#regresión lineal simple 
flujo_vs_estatica <- lm(flujo ~ estatica , toxicos)

#valores a pronosticar 
pre_estatica <- tibble(
  estatica = 25
)
pre_estatica
#pronostico de ye 
pronosticos <- pre_estatica %>%
  mutate(
    flujo =   predict(flujo_vs_estatica , pre_estatica)
  )
#gráfica de la recta ajustada a los puntos 
ggplot( toxicos, aes( estatica, flujo )) + geom_point() + 
  geom_smooth(method = 'lm',se = FALSE) + 
  geom_point( data = pronosticos, 
              color = 'green')




