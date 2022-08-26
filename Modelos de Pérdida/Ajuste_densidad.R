library(MASS) #distribucion de probabilidad
library(actuar) #distribucuion adicionales
library(fitdistrplus) #Ajuste de curvas de Probabilidad
library(goftest) #Pruebas de bondad de ajuste 
library(rriskDistributions)
#carga de datos
datos <- rweibull(1000,1,2000)

#1. Análisis Descriptivo de los Datos 
summary(datos) 
hist(datos, freq = FALSE)
plot(ecdf(datos))

#2. Ajuste de Curvas de Probabilidad
help("fitdistrplus")

mod1 <- fitdist(datos, "gamma", method = c("mme"))
mod1
summary(mod1)

par(mfrow = c(2,2))
denscomp(mod1) # Función de Densidad
cdfcomp(mod1) #Función de Distribución 
qqcomp(mod1) #cuantiles Teóricos vs Empiricos 
ppcomp(mod1) #Probabilidades Teóricas vs Empiricas 

#3. Validación con pruebas de bondad de ajuste 
(hip1 <- ks.test(datos,"pweibull", mod1$estimate[1], mod1$estimate[2]))
(hip2 <- ad.test(datos,"pweibull", mod1$estimate[1], mod1$estimate[2]))

#4. Comparativo de ajustes de curvas de Probabilidad 
(mod2 <- fitdist(datos, "weibull", method = "mle"))
(mod3 <- fitdist(datos, "pareto", method = "mle", 
                start = list(shape = 1, scale =300)))
(mod4 <- fitdist(datos, "lnorm", method = "mle"))

(leyenda <- c("Weibull", "Pareto", "Lognormal"))
denscomp(list(mod2, mod3, mod4), legendtext = leyenda) # Función de Densidad
cdfcomp(list(mod2, mod3, mod4), legendtext = leyenda) #Función de Distribución 
qqcomp(list(mod2, mod3, mod4), legendtext = leyenda) #cuantiles Teóricos vs Empiricos 
ppcomp(list(mod2, mod3, mod4), legendtext = leyenda) #Probabilidades Teóricas vs Empiricas 

gofstat(list(mod2, mod3, mod4))
        fitnames = c("Weibull", "Pareto", "Lognormal")
######################3
        


        