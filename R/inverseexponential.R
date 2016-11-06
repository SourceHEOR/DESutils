#'Inverse exponential distribution
#'
#'Generate draw from inverse exponential distribution.
#'
#'@param mean.time mean time to event
#'@param proability normally random number
#'@export
#'@examples
#'time.to.see.doctor <- 1 #week
#'InvExponential(time.to.see.doctor)
InvExponential=function(mean.time, probability=runif(1)){
exp.time = -mean.time * log(1 - probability)
return(exp.time)
}