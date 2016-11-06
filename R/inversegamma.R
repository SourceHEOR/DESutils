#'Returns inverse gamma distribution
#'
#'Generate inverse gamma distribution based on algorithms on p. 451 of
#'Law, A. Simulation modelling and analysis. 4th Ed. 2007
#'
#'@param alpha alpha coefficient
#'@param beta beta coefficient
#'@export


InverseGamma<-function(alpha, beta=1) {
  
if (alpha>=1) {
  
  x <- 321 #Just make sure that x!=y for first evaluation
  y <- 123
  
  a <- 1 / sqrt((2*alpha) - 1)
  b <- alpha - log(4)
  q <- alpha + (1 / a)
  theta <- 4.5
  d <- 1 + log(theta)
  
while (x != y) {
  u1 <- runif(1)
  u2 <- runif(1)
  v <- a*log(u1 / (1 - u1))
  y <- a*exp(v)
  z <- (u1 ^ 2)*u2
  w <- b + q*v - y
  eval <- w + d - (theta*z)
  
  if (eval >= 0) {
    x <- y
  }  else if (w >= log(z)) {
    x <- y
  }
}
 
} else {

x <- 321 #Just make sure that x!=y for first evaluation
y <- 123  
  
while (x != y) {   
  
  u1=runif(1)
  b<-(exp(1) + alpha) / exp(1)
  p<-b*u1
  
  if (p > 1) {
    y <- -log( (b-p) / alpha)
    u2<-runif(1)
      
    if (u2 <- y ^ (alpha - 1)) {
      x <- y
    }
    
  } else {
  
  y <- p ^ (1/alpha)
  u2 <- runif(1)
    
    if (u2 <= exp(-y)) {
      x <- y
    }
  }
}
}

x <- x*beta
return(x)

}