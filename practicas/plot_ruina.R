C <- 999
fortuna <- C 

while (fortuna > 0) {
  
  monto <- 2 * ( fortuna <= 6 ) + floor ( fortuna/2 ) * (fortuna > 6)
  
  fortuna<-fortuna+monto*(2*(runif(1)>1/2)-1)

  C<-c(C,fortuna)
  
}

C

plot(C)