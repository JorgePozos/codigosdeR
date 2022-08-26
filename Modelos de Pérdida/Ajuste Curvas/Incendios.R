#Ajuste de una curva de Probabilidad de Evento de Incendio Forestal
library(knitr)
library(dplyr) #Manipulación de Dataframes
library(readxl) #Importar Datos de Excel
library(MASS) #Distribucion de Probabilidad
library(actuar) #Distribuciones Adicionales
library(fitdistrplus) #Ajuste de Curvas de Probabilidad
library(goftest) #Pruebas de Bondad de Ajuste 
library(ggplot2) #Gráficas

#1-importamos los datos de año y costo del siniestro 
incendios <- read_excel(
  "Desktop/codigosde R/Ajuste Curvas/AP Ajuste de Curvas de Probabilidad - Atlas de Riesgos.xlsx",
  sheet = "IF"
  )

#2-Actualizamos los Montos de Pérdida, considerendo incrementos anuales de inflación

#importamos una tabla con los datos de la tasa de interés anual 
inflacion <- read_excel("Desktop/codigosde R/Ajuste Curvas/INFLACION.xlsx")%>%
  filter(Año >= 2000 & Año <= 2015)#nos quedamos con la información entre 2000 y 2015

#juntamos la tablas de incendios e inflación en una sola tabla incendio_ajustado
incendio_ajustado <- incendios%>%
  inner_join(inflacion, by = "Año")%>%
  mutate(costo_real = `Incendio Forestal (mdp)`*factor_ajuste)

#3-Análisis de Estadística Descriptiva de los datos
#Tranformamos los datos para dar escala conveniente
datos <- incendio_ajustado %>% 
  mutate(logdata = log(costo_real) + 7 )
#Estadística Descriptiva
est_des <- summary(datos$logdata)
ggplot(datos, aes(logdata))  + geom_histogram(aes(y = ..density..), bins = 13)+   geom_density(alpha = 0.05 ,fill = "green") + 
  labs(x= "Intervalos", title = "Densidad de Montos de Pérdida a Escala (mdp)")+
  
  geom_vline(aes(xintercept=  est_des[4] ),#Media
             color="blue", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=  est_des[2] ),#Primer Cuartil
             color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=  est_des[3] ), #Mediana
             color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=  est_des[5] ),#Tercer Cuartil
             color="red", linetype="dashed", size=1)
est_des  

#Función de Distribución
plot(ecdf(datos$logdata), main = "Función de Distribución a Escala")

#Ajuste de curvas de probabilidad
mod1 <- fitdist(datos$logdata, "gamma", method = c("mle"))
mod2 <- fitdist(datos$logdata, "weibull", method = "mle")
mod3 <- fitdist(datos$logdata, "pareto", method = "mle", 
                start = list(shape = 1, scale =300))
mod4 <- fitdist(datos$logdata, "lnorm", method = "mle")
mod5 <- fitdist(datos$logdata, "norm", method = "mle")
mod6 <- fitdist(datos$logdata, "llogis", method = "mle")
mod7 <- fitdist(datos$logdata, "invgamma", method = "mle")
mod8 <- fitdist(datos$logdata, "invweibull", method = "mle")

#Comparativo de ajustes de curvas de Probabilidad 
par(mfrow = c(2,2))
leyenda <- c("Gamma", "Weibull", "Pareto", "Lognormal","Normal","loglogistica","Gamma Inversa","Weibull Inversa")
denscomp(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8), legendtext = leyenda)
cdfcomp(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8), legendtext = leyenda)  
qqcomp(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8), legendtext = leyenda, xlim = c(0,12))  
ppcomp(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8), legendtext = leyenda)  

#Pruebas de Bondad de Ajuste 

gofstat(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8))

