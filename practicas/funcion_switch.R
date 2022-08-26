perro <- "Normal"
centre <- function(v, type) {
  switch(type,
         "Beta" = 'beta',
         "Binomial" ='binom',
         "Cauchy"= 'cauchy',
         "Xi-Cuadrada"= 'chisq',
         "Exponencial" = 'exp',
         "Distribución-F"='f',
         "Gamma"= 'gamma',
         "Geométrica"= 'geom',
         "Hipergeómetrica"= 'hyper',
         "Lognormal"= 'lnorm',
         "Multinomial"= 'multinom',
         "Binomial Negativa"= 'nbinom',
         "Normal"= 'norm',
         "Poisson"= 'pois',
         "T Student"= 't',
         "Uniforme"= 'unif',
         "Weibull"= 'weibull'}
centre(v,perro)

paraqq <- function(v,type){
  switch(type,
         "Beta" = 'beta',
         "Binomial" ='binom',
         "Cauchy"= 'cauchy',
         "Xi-Cuadrada"= 'chisq',
         "Exponencial" = 'exp',
         "Distribución-F"='f',
         "Gamma"= 'gamma',
         "Geométrica"= 'geom',
         "Hipergeómetrica"= 'hyper',
         "Lognormal"= 'lnorm',
         "Multinomial"= 'multinom',
         "Binomial Negativa"= 'nbinom',
         "Normal"= 'norm',
         "Poisson"= 'pois',
         "T Student"= 't',
         "Uniforme"= 'unif',
         "Weibull"= 'weibull')
}
paraqq(2,perro)




