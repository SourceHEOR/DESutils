#' Generate stochastic time to event
#' 
#' Returns inverse of survival distributions. Designed to give time to event.
#' 
#' @param a scale parameter of distribution
#' @param b shape parameter (e.g. b for exponential distribution is 1)
#' @param distribution one of "expo","weib","logl","logn" or "gomp"
#' @param probability normally random number. Providing probability=0.5 gives median time to event.
#' @param coefficients currently a single value representing the sum product of
#'   the values taken by variables and the associated coefficients
#' @export

InverseSurvival<-function(a, b=1, distribution, probability = runif(1), coefficients) {

dist.list<-c("expo","weib","logl","logn","gomp")
if (distribution %in% dist.list){
if  (distribution!=dist.list[1] && missing(b)){
  
warning('You have not provided a shape parameter; this is set at 1 by defualt')  
} 

probability <- 1-probability

if (distribution==dist.list[1]){
#derived from SAS output b = mu, a = sigma
        if (missing(coefficients)){
        time <- -exp(b) * log(probability)
        }
        else {
        time <- (-exp(b) * log(probability)) * exp(coefficients)
        }
}
else if (distribution==dist.list[2]){
        if (missing(coefficients)){
        time <- (-log(probability) * exp(b / a)) ^ a
        #time <- b * (-log(1 - probability)) ^ (1 / a)
        }
        else {
        time <- ((-log(probability) * exp(b / a)) ^ a) * exp(coefficients)
        }
}
else if (distribution==dist.list[3]) {
        if (missing(coefficients)){
        time <- (exp(b / a) * (probability / (1 - probability))) ^ a
        #CT - think this should be
        # time = (exp(b / a) * ((1-probability) / probability)) ^ a
        } 
        else {
        time <- ((exp(b / a) * (probability / (1 - probability))) ^ a) * exp(coefficients)
        }
}
else if (distribution==dist.list[4]){
        if (missing(coefficients)){
        time <- qlnorm(probability,mean = a, sd = b)
        }
        else {
        time <- qlnorm(probability, mean = a, sd = b) * exp(coefficients)
        }
}
else if (distribution==dist.list[5]){
        if (missing(coefficients)){
        time <- (1 / a) * log(1 - (a * log(probability) / exp(b)))
        }
        else {
        time <- (1 / a) * log(1 - (a * log(probability) / exp(b + coefficients)))
        }
}


    if (time<0){
    stop('Obviously something has gone wrong, your times are less than 0')  
      }
    return(time)
}
else{
stop('The distribution argument must be one of expo, weib, logl, logn, gomp')
}
}