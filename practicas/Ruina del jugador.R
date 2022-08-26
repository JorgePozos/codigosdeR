# funcion que realiza simulaciones hasta que se llega a 'a' o a 'b' e imprime el proceso
ruina <- function (x,a,b,p){
  cont <- 0
  Y <- array()
  Y[1] <- x
  i <- 2
  repeat {
    pr <- sample(c(0,1), 1, replace = TRUE, prob = c(p,1-p))
    if (pr == 0) {
      Y[i] <- Y[i-1] + 1
    } else {
      Y[i] <- Y[i-1] - 1
    }

    #cont <- cont + 1
    if (Y[i] == a | Y[i] == b) break
    i <- i + 1
  }
  Y
}

#ejemplo
ruina(4,1,7,1/2)
ruina(4,1,7,1/2)
ruina(4,1,7,1/2)
ruina(4,1,7,1/2)
ruina(4,1,7,1/2)

