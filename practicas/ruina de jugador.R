#funcion de incremento o decremento aleatorio
randomw <- function(c, p){
  if((0 < p) & (p < 1) == FALSE){
    print("no se puede")
  } else {
      if(rbinom(1,1, p ) == 1){
      c = c + 1
      } else {
      c = c - 1
      }
  }
}

#funcion de caminata aleatorio a- limite inferior, b_superior, p-porb, n-capital

caminata <- function(a , b, c, p){ 
Y <- array()
Y[1] <- c
i <- 1
  if(((a < c &  c < b) == FALSE) & ((0 < p) & (p < 1)) == FALSE){
    print("no es posible realizar el calculo")
  } else {
    while(a < c &  c < b){
          c <- randomw(c,p)
          i <- i+1
          Y[i] <- c
        }
  out <- list(Y, i, c)
  names(out) <- c('resultado en cada iteracion', 'numero de iteraciones', 'capital')
  out
  }
}

#corremos la función asignando valores


#corremos la funcion n-veces
intentos <- function(a , b, c, p, n){
  r <- 1
  X <- array()
  Z <- array()
  
  if((a < c &  c < b) == FALSE){
    print("no se puede")
  } else {
    while(r <= n){
      X[r] <- caminata( a , b , c , p )[[2]]
      Z[r] <- caminata( a , b , c , p )[[3]]
      r <- r + 1
    }
  lista <- list(X, Z)
  names(lista) <- c('numero de intentos', 'resultado del juego')
  lista
  }
}

#corremos las funciones con valores 
caminata(0, 10, 5, 9/20)
#repitiendo la funcion n número de veces
intentos(0, 10, 5, 9/20, 99)