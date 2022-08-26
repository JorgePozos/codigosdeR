###### Modelos de Severidad (Modificaciones Póliza) #######

# Librerias 
library(MASS) #distr de probabilidad 
library(actuar) #modificaciones en la v-a- de pérdida X
help("distributions")
# Función Coverage  (Cobertura)
help("coverage")

# Deducicle 
 m1 <- coverage(
   dgamma,
   pgamma,
   0.5, # <- deducible
   TRUE, # <- franquicia 
   Inf, # <- límite 
   1, #coaseguro 
   0, # <- inflación 
   FALSE  # <- per.loss 
 )
 
 m2 <- coverage(
   dgamma,
   pgamma,
  limit = 1, # <- límite 
   per.loss =FALSE  # <- per.loss 
 )
 
 m1( 1,  # prob. de ocurrencia, (punto a evaluar) 
     4,  # <- alpha
     3) # <- beta 
 
 
 # Gráfica 
 eje <- 4
 
 curve(dgamma(x,4,3), xlim = c(0,eje), ylim = c(0,1))
 curve(m2(x,4,3), xlim = c(0,eje), ylim = c(0,1), 
       col = 5, add = TRUE)

 ####### deducible y límite de póliza  #####
 m3 <- coverage(
   dgamma,
   pgamma,
   .5,
   FALSE,
   1,
   per.loss =FALSE  # <- per.loss 
 )
 curve(dgamma(x,4,3), xlim = c(0,eje), ylim = c(0,1))
 curve(m1(x,4,3), xlim = c(0,eje), ylim = c(0,1), 
       col = 5, add = TRUE)

 ### coaseguro  
 # Y = \alpha * X
 m4 <- coverage(
   dgamma,
   pgamma,
   coinsurance = 0.9,
   per.loss = TRUE
 )
 curve(dgamma(x,4,3), xlim = c(0,eje), ylim = c(0,1))
 curve(m4(x,4,3), xlim = c(0,eje), ylim = c(0,1), 
       col = 5, add = TRUE)  # la curva se hace más Kúrtica 
 
 ### inflación  
 
 m5 <- coverage(
   dgamma,
   pgamma,
   inflation = 0.75,
   per.loss = FALSE
 )
 curve(dgamma(x,4,3), xlim = c(0,eje), ylim = c(0,1))
 curve(m5(x,4,3), xlim = c(0,eje), ylim = c(0,1), 
       col = 5, add = TRUE)  # la curva se hace más Kúrtica 

 ### Caso General  
 
 m6 <- coverage(
   dgamma,
   pgamma,
   deductible = .5,
   franchise = FALSE,
   limit = 1,
   coinsurance = .9,
   inflation = 0.05,
   per.loss = FALSE
 )
 curve(dgamma(x,4,3), xlim = c(0,eje), ylim = c(0,1))
 curve(m6(x,4,3), xlim = c(0,eje), ylim = c(0,1), 
       col = 5, add = TRUE)  # la curva se hace más Kúrtica 
 
  