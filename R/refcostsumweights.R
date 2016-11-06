#' Returns weighted lower or upper quartile for selection of NHS ref costs
#' 
#' Normally called by RefCostsRange. In order to estimate the appropriate weighting
#' of lower/upper quartiles a 'cut-point' is used.
#' 
#' @param cut cut-point on log-scale
#' @param data dataset in .csv format to use with 9 columns of data, 1 row per currency code
#' @param low low=TRUE if we wish to find the cut-point corresponding to 25th percentile,
#'   or low=FALSE if we wish to fnd the cut-point corresponding to 75th percentile
#' @export

SumWeights<-function(cut,data,low=TRUE){
  
  mydata <- read.csv(data,head=T,sep=",")
  mydata$mean <- log(mydata[,4])
  mydata$sd <- (log(mydata[,6]) - log(mydata[,5])) / (qnorm(p=0.75,mean=0,sd=1)-qnorm(p=0.25,0,1))
  mydata$se <- mydata$sd / sqrt(mydata[,9])
  mydata$wght <- mydata[,3] / sum(mydata[,3])
  mydata$norm.dist <- pnorm(q=cut,mean=mydata$mean,sd=mydata$sd)
  mydata$mywght <- mydata$norm.dist * mydata$wght
  
  quart <- sum(mydata$mywght)
  
  if (low==TRUE) {
    sum.dist <- abs(quart - .25)
  } else {
    sum.dist <- abs(quart - .75)
  }
  
  return(sum.dist)
}
