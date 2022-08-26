##### Números Aleatorios LGC ########
library(dplyr)
#m <- modulus 
#a  <- multiplier 
#c  <- increment 
#z  <- seed 

random_lcg <- function(n=13, m, a, c, z) {
  zetas <- vector(length = n)   
  for (i in 1:n) {
    z <- (a * z + c) %% m
    zetas[i] <- z
  }
  return(zetas)
}

#Problema 7.2 
#inciso a) 
random_lcg(m = 16, # <- modulus 
           a = 11,  # <- multiplier 
           c = 0, # <- increment 
           z = 1) # <- seed 
#inciso b) 
random_lcg(m = 16, # <- modulus 
           a = 11,  # <- multiplier 
           c = 0, # <- increment 
           z = 2) # <- seed 
#inciso c)
random_lcg(m = 13, # <- modulus 
           a = 2,  # <- multiplier 
           c = 0, # <- increment 
           z = 1) # <- seed 
#inciso d) 
random_lcg(m = 13, # <- modulus 
           a = 3,  # <- multiplier 
           c = 0, # <- increment 
           z = 1) # <- seed 

#Problema 7.4
random_lcg(m = 16, # <- modulus 
           a = 13,  # <- multiplier 
           c = 13, # <- increment 
           z = 1) # <- seed 

random_lcg(m = 16, # <- modulus 
           a = 12,  # <- multiplier 
           c = 13, # <- increment 
           z = 1) # <- seed 

random_lcg(m = 16, # <- modulus 
           a = 13,  # <- multiplier 
           c = 12, # <- increment 
           z = 1) # <- seed 

random_lcg(m = 13, # <- modulus 
           a = 1,  # <- multiplier 
           c = 12, # <- increment 
           z = 1) # <- seed 

#Problema 7.6 d3 en a2

unif_lcg <- function(n=13, m, a, c, z) {
  zetas <- vector(length = n)   
  for (i in 1:n) {
    z <- (a * z + c) %% m
    zetas[i] <- floor((z/m)/.5+1)
  }
  return(zetas)
}
unif_lcg(100, 16, 11, 0, 1)

V_1 <-  random_lcg(100, 13, 1, 12, 1)
tabla <- data.frame(V_1)
tabla <-  tabla %>% 
  mutate(V_2 = random_lcg(100, 13, 1, 12, V_1) )
tabla <-  tabla %>% 
  mutate(I = unif_lcg(100, 16, 11, 0, 1) )
tabla <-  tabla %>% 
  mutate(V_I = random_lcg(2, 13, 1, 12, 1)[I])

tabla
