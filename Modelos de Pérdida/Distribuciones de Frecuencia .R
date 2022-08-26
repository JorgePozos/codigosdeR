#### distribuciones de frecuencia
library(actuar)
library(fitdistrplus)
library(goftest)

#binomial(n,p)
#Poisson(lambda)
#geométrica(p)
#Binomialneg(r,p)

#Funciones de Densidad 
n = 10
p = .3
lambda = 2 
r= 10

#caso1.- Distribución Binomial (n,p)

fden <- dbinom(0:10,n,p)
fdis <- pbinom(0:10,n,p)
fsob <- 1 - fdis

par(mfrow= c(2,3))
barplot(fden,main = 'función de densidad', col= 2)
barplot(fdis,main = 'función de distribución', col= 3)
barplot(fsob,main = 'función de sobrevivencia', col= 4)

plot(fden,type = 'b', lwd=3, main= 'Función de Densidad', col = 2 )
plot(fdis,type = 'b', lwd=3, main= 'Función de Distribución', col = 3 )
plot(fsob,type = 'b', lwd=3, main= 'Función de Sobrevivencia', col = 4 )

#Caso 2 Poisson(lambda)
fden <- dpois(0:10,lambda)
fdis <- dpois(0:10,lambda)
fsob <- 1 - fdis

par(mfrow= c(2,3))
barplot(fden,main = 'función de densidad', col= 2)
barplot(fdis,main = 'función de distribución', col= 3)
barplot(fsob,main = 'función de sobrevivencia', col= 4)

plot(fden,type = 'b', lwd=3, main= 'Función de Densidad', col = 2 )
plot(fdis,type = 'b', lwd=3, main= 'Función de Distribución', col = 3 )
plot(fsob,type = 'b', lwd=3, main= 'Función de Sobrevivencia', col = 4 )

#Caso 3 Geométrica(r,p)
fden <- dgeom(0:10,p)
fdis <- dpois(0:10,p)
fsob <- 1 - fdis

par(mfrow= c(2,3))
barplot(fden,main = 'función de densidad', col= 2)
barplot(fdis,main = 'función de distribución', col= 3)
barplot(fsob,main = 'función de sobrevivencia', col= 4)

plot(fden,type = 'b', lwd=3, main= 'Función de Densidad', col = 2 )
plot(fdis,type = 'b', lwd=3, main= 'Función de Distribución', col = 3 )
plot(fsob,type = 'b', lwd=3, main= 'Función de Sobrevivencia', col = 4 )

#Caso 4 binomial negativa(r,p)
fden <- dnbinom(0:10,r,p)
fdis <- pnbinom(0:10,r,p)
fsob <- 1 - fdis

par(mfrow= c(2,3))
barplot(fden,main = 'función de densidad', col= 2)
barplot(fdis,main = 'función de distribución', col= 3)
barplot(fsob,main = 'función de sobrevivencia', col= 4)

plot(fden,type = 'b', lwd=3, main= 'Función de Densidad', col = 2 )
plot(fdis,type = 'b', lwd=3, main= 'Función de Distribución', col = 3 )
plot(fsob,type = 'b', lwd=3, main= 'Función de Sobrevivencia', col = 4 )

### Dsitribuciones de la clase (a,b,0) y (a,b,1)
library(actuar) # <- tiene distribuciones de las clases 
#parámetros
n = 10
p = .3
lambda = 2 
r= 10
p0 = 0.2

#caso1.- Distribución Binomial (n,p)

fden <- dbinom(0:10, n, p)
fmod <- dzmbinom(0:10, n, p,p0)
ftrun <- dztbinom(0:10, n, p)

par(mfrow= c(2,3))
barplot(fden,main = 'función de densidad', col= 2)
barplot(fmod,main = 'función modificada', col= 3)
barplot(ftrun,main = 'función de truncada', col= 4)

plot(fden,type = 'b', lwd=3, main= 'Función de Densidad', col = 2 )
plot(fmod,type = 'b', lwd=3, main= 'Función de modificada', col = 3 )
plot(ftrun,type = 'b', lwd=3, main= 'Función de truncada', col = 4 )

#caso2.- Distribución poisson (lambda)

fden <- dpois(0:10, lambda)
fmod <- dzmpois(0:10, lambda,p0)
ftrun <- dztpois(0:10, lambda)

par(mfrow= c(2,3))
barplot(fden,main = 'función de densidad', col= 2)
barplot(fmod,main = 'función modificada', col= 3)
barplot(ftrun,main = 'función de truncada', col= 4)

plot(fden,type = 'b', lwd=3, main= 'Función de Densidad', col = 2 )
plot(fmod,type = 'b', lwd=3, main= 'Función de modificada', col = 3 )
plot(ftrun,type = 'b', lwd=3, main= 'Función de truncada', col = 4 )

#caso3.- Distribución geométrica  (p)

fden <- dgeom(0:10,p)
fmod <- dzmgeom(0:10, p,p0)
ftrun <- dztgeom(0:10, p)

par(mfrow= c(2,3))
barplot(fden,main = 'función de densidad', col= 2)
barplot(fmod,main = 'función modificada', col= 3)
barplot(ftrun,main = 'función de truncada', col= 4)

plot(fden,type = 'b', lwd=3, main= 'Función de Densidad', col = 2 )
plot(fmod,type = 'b', lwd=3, main= 'Función de modificada', col = 3 )
plot(ftrun,type = 'b', lwd=3, main= 'Función de truncada', col = 4 )

#caso4.- Distribución binomial negativa   (r,p)

fden <- dnbinom(0:50,r,p)
fmod <- dzmnbinom(0:50, r,p,p0)
ftrun <- dztnbinom(0:50, r,p)

par(mfrow= c(2,3))
barplot(fden,main = 'función de densidad', col= 2)
barplot(fmod,main = 'función modificada', col= 3)
barplot(ftrun,main = 'función de truncada', col= 4)

plot(fden,type = 'b', lwd=3, main= 'Función de Densidad', col = 2 )
plot(fmod,type = 'b', lwd=3, main= 'Función de modificada', col = 3 )
plot(ftrun,type = 'b', lwd=3, main= 'Función de truncada', col = 4 )

################# Estimación de Parámetros 
library(fitdistrplus)
library(goftest)

#Caso 1.- Variables Originales 
y = rpois(1000, 1) #<- obtencion de datos 

mod1 <- fitdist(y, 'binom', method = 'mle',
                discrete = TRUE, start = list(prob= p),
                fix.arg = list(size = n))
summary(mod1)


mod2 <- fitdist(y, 'pois', method = 'mle',
                discrete = TRUE, start = list(lambda= lambda))
summary(mod2)

mod3 <- fitdist(y, 'nbinom', method = 'mle',
                discrete = TRUE, start = list(size=r, prob=p))
summary(mod3)

gofstat(list(
  mod1,mod2,mod3
), fitnames = c("Binomial","Poisson", "Binomial Negativo"))

par(mfrow = c(2,2))
denscomp(list(mod1,mod2, mod3), 
         legendtext = c('Binomial','Poisson','BinN'))
cdfcomp(list(mod1,mod2, mod3), 
        legendtext = c('Binomial','Poisson','BinN'))
ppcomp(list(mod1,mod2, mod3), 
       legendtext = c('Binomial','Poisson','BinN'))
qqcomp(list(mod1,mod2, mod3), 
       legendtext = c('Binomial','Poisson','BinN'))

# Caso 2 .- ajuste con funciones de la clas (a,b,0) y (a,b,1) 
#parámetros
n = 100
p = .3
lambda = 2 
r= 10
p0 = 0.2

y <- rzmnbinom(1000, r, p, p0) 
#vamos a empezar a comparar los modelos 

mod4 <- fitdist( y, 'zmbinom', 'mle',discrete = T, 
                 start = list(prob= p ),
                 fix.arg = list(size=n, p0 = p0))
summary(mod4)

mod5 <- fitdist( y, 'zmpois', 'mle', discrete =  T,
                 start =  list(lambda = lambda), #en donde va a empezar 
                 fix.arg = list(p0 = p0))      #argumentos que dejamos fijos 
summary(mod5)

mod6 <- fitdist( y , 'zmnbinom', 'mle', discrete = T,
                 start = list(prob = p), fix.arg = list(size = r , p0 = p0 ))
summary(mod6)

gofstat(list(mod4, mod5, mod6), 
        fitnames = c('ZMBin','ZMPois','ZMNbin'))


denscomp(list(mod4,mod5, mod6), 
         legendtext = c('ZMBin','ZMPois','ZMNbin'))
cdfcomp(list(mod4,mod5, mod6), 
        legendtext = c('ZMBin','ZMPois','ZMNbin'))
ppcomp(list(mod4,mod5, mod6), 
       legendtext = c('ZMBin','ZMPois','ZMNbin'))
qqcomp(list(mod4,mod5, mod6), 
       legendtext = c('ZMBin','ZMPois','ZMNbin'), xlim = c(0,50))

# Caso Práctico 
# Ajuste  de Siniestro de Automoviles 

eventos <- c(370412, 46545 , 3935, 317 , 28, 3)
(total = sum(eventos))
(p0 <- eventos[1]/total)
mod7 <- fitdist(eventos , 'zmnbinom', 'mle', discrete = T,
                start = list(prob = 0.8),
                fix.arg = list(size = 2 , p0 = p0 ))

