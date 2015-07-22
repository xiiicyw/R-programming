add2 <- function(x,y){
  x+y
}

above10 <- function(x){
  use <- x > 10
  x[use]
}

above <- function(x,n){
  use <- x > n
  x[use]
}
above <- function(x,n = 10){
  use <- x > n
  x[use]
}

column <- function(y){
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[ ,i], na.rm = T)
  }
  means
}