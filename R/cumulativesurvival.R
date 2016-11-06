#' Generate stochastic cumulative survival
#' 
#' Returns cumulative survival probability for a given time point
#' 
#' @param a scale parameter of distribution
#' @param b shape parameter (e.g. b for exponential distribution is 1)
#' @param distribution one of "expo","weib","logl","logn" or "gomp"
#' @param time time for which the cumulative survival is required
#' @param coefficients currently a single value representing the sum product of
#'        the values taken by variables and the associated coefficients
#'        
#' @export

CumulativeSurvival <- function(a, b=1, distribution, time,coefficients) {
  
  dist.list<-c("expo","weib","logl","logn","gomp")
  
  if (distribution %in% dist.list){
    if  (distribution!=dist.list[1] && missing(b)){
      
      warning('You have not provided a shape parameter; this is set at 1 by defualt')  
    } 
    
    if (distribution==dist.list[1]){
      
      if (missing(coefficients)){
        prob <- exp(-exp(a) * time)
      }
      else {
        prob <- exp(-exp(a + coefficients) * time)
      }
      
    }
    else if (distribution==dist.list[2]){
      if (missing(coefficients)){
        prob <- exp(-exp(a) * time ^ b)
        #time <- b * (-log(1 - probability)) ^ (1 / a)
      }
      else {
        prob <- exp(-exp(a + coefficients) * time ^ b)
      }
      
    }
    else if (distribution==dist.list[3]) {
      if (missing(coefficients)){
        prob <- (1 + (exp(-a) * time) ^ (1 / b)) ^ -1
        #CT - think this should be
        # time = (exp(b / a) * ((1-probability) / probability)) ^ a
      } 
      else {
        prob <- (1 + (exp(-a - coefficients) * time) ^ (1 / b)) ^ -1
      }
      
    }
    else if (distribution==dist.list[4]){
      if (missing(coefficients)){
        prob <- 1 - plnorm(time,mean = a, sd = b)
      }
      else {
        prob <- 1 - plnorm(time, mean = (a + coefficients), sd = b)
      }
      
    }
    else if (distribution==dist.list[5]){
      if (missing(coefficients)){
        prob <- exp(((exp(a) / b) * (1 - exp(b * time))))
      }
      else {
        prob <- exp(((exp(a + coefficients) / b) * (1 - exp(b * time))))
      }
      
    }
    
    if (prob<0){
      stop('Obviously something has gone wrong, your probabilities are less than 0')  
    }
    
    return(prob)
    
  }
  else{
    stop('The distribution argument must be one of expo, weib, logl, logn, gomp')
  }
}