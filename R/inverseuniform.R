#' Inverse of uniform distribution
#' 
#' Provides time to event from an inverse distribution.
#' Probably provided elsewhere by other functions.
#' 
#' @param lower minimum time
#' @param upper maximum time
#' @param prob probaility, normally random
#' @export
#' @examples
#' curr.age <- 26
#' max.age.death <- 85
#' InverseUniform(lower = curr.age, upper = max.age.death)

InverseUniform<-function(lower=0,upper=1,prob=runif(1)){
 x=lower + (upper - lower) *prob
 return(x) 
}