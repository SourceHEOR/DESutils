#' Determine which mututally exclusive outcome occurs
#' 
#' Uses accept/reject methodology to determine which mututally exclusive
#' event occurs (maximum 6 possible events). Note sum of probabilities
#' should be less than 1. The probabiity of the final even is defined as
#' 1 minus the sum of the other probabilities.
#' 
#' @param p1 probability of 1st outcome
#' @param p2 probability of 2nd outcome
#' @param p3 probability of 3rd outcome
#' @param p4 probability of 4th outcome
#' @param p5 probability of 5th outcome
#' @param probability value to evaluate against probabilities (normally random number)
#' @export
#' @examples
#' prob.coin.is.heads <- 0.5
#' AcceptReject(prob.coin.is.heads)
#' 
#' dice1 <- 1/6
#' dice2 <- 1/6
#' dice3 <- 1/6
#' dice4 <- 1/6
#' dice5 <- 1/6
#' dice6 <- 1/6
#' AcceptReject(p1=dice1, p2=dice2, p3=dice3, p4=dice4, p5=dice5)
 
AcceptReject<-function(p1,p2=0,p3=0,p4=0,p5=0,probability=runif(1)) {
  
  p.x<-1-p1+p2+p3+p4+p5
  
#Determine how many mutually exclusive event there are  
  if (p2==0){
    event=2
  }else if(p3==0){
    event=3
  }else if(p4==0){
    event=4
  }else if(p5==0){
    event=5
  }else{
    event=6
  }
  
#Check that probabilities have been entered correctly  
if (p.x<0) {
    stop('1 minus the sum of the probabilities hsould be greater than 1')
}

#Work out which event has happened  
if (probability<p1){
  return(1)
} else if(probability<p1+p2) {
  return(2)
} else if(probability<p1+p2+p3) { 
  return(3)
} else if (probability<p1+p2+p3+p4){
  return(4)
} else if (probability<p1+p2+p3+p4+p5){
  return(5)
} else {
  return(event)
}

}

  