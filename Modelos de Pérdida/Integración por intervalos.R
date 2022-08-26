#Metodo de Integración por rectangulos 
confianza <- .95
z<-qnorm(confianza, 10, 10*sqrt(3))
ub <- 680
Vec<-c()
particiones <- 99999
for (i in 1:particiones){
  base <- (ub-z)/particiones
  x<- (z+i*base+z+(i-1)*base)/2
  altura <- x*dnorm(x,10,sqrt(3)*10)
  Vec[i]<- base*altura
}
Integral<-sum(Vec)
Integral
ti_var <- Integral/(1- confianza)
ti_var


