#Modelo AR(5)
library(dynlm) #<- librerias para modelo autoregresivo
#Importamos los datos de google
library(readxl)
precios <- read_excel("Desktop/Tarea Final.xlsx", 
                          sheet = "Problema 2", range = "b2:c2520")
library(knitr)
library(broom)
attach(precios)

#Definimos los datos como una serie de tiempo 
series_tiempo <- ts(precios$Close, start = c(2004,11,29), frequency = 360)
plot(series_tiempo, col= 'blue', main= 'Precio de Google')
kable(head(precios),digits = 4, caption = 'Precio de Google al Cierre',)
#Definimos el modelo para la autoregresión con 5 rezagos 
modelo_ar5 <- dynlm(series_tiempo ~ L( series_tiempo, 5 ) )
kable(tidy(summary(modelo_ar5)))
 #¿Cuál es la R^2?
 #- R^2 es de 0.991

 #¿Qué variables son estadísticamente significantes?
 #- Como vemos en el summary tanto el intercepto como el modelo con 5 
 #rezagos son estadísticamente significantes al 99% 

 #¿Qué dice esto de la hipotesis de mercado eficiente?
 #Los resultados apoyan la hipotesis de mercado eficiente pues usando valores 
 #anteriores para la serie de tiempo como variables independientes obtenemos 
 #R^2 muy alta, lo que quiere decir que sin importar el tiempo, los movimientos
 #en el precio son muy parecidos día con día.



