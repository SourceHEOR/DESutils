#' Retrieve AIC statistics for alternative parametric survival models
#' 
#' Returns table AIC table for list of distributions given in dlist.
#' Writes model output to a .txt file
#' 
#' @param surv.object a 'Surv' object
#' @param surv.data survival dataset on which calcs will be based
#' @param vars variables to include within the models
#' @param dlist list of distributions to feed to survreg
#' @param out.path location for storing .txt fils of fited model.
#'   Defaults to current working directory.
#' @param file.name name of output file created
#' @export
#' @examples
#' require(survival)
#' bladder
#' bsurv <- Surv(bladder$stop, bladder$event)
#' a <- DistFind(bsurv,
#'               bladder,
#'               bladder$rx,
#'               out.path="data/bladder",
#'               file.name="output")

DistFind <- function(surv.object,
                     surv.data,
                     vars  = 1,
                     dlist = c("exponential",
                               "weibull",
                               "lognormal",
                               "loglogistic"),
                     out.path = NULL,
                     file.name = "output") {

if (is.null(out.path) == TRUE) {
  out.path = getwd()
}

####################################################################
####################################################################
#Do all distributions and compare AIC ##############################
#1. Create model
#2. Produce output
#3. Produce variance-covariance matrix
#4. Calculate AIC

#open sink
sink(file.path(out.path, paste(file.name, ".txt",sep="")), append = FALSE, split = FALSE)
myListLength <- length(dlist)
AICtable <- matrix(nrow = myListLength, ncol = 2, byrow = TRUE)
colnames(AICtable) <- c("df", "AIC")
rownames(AICtable) <- dlist

for (i in 1 : myListLength) {
  #1
  print(paste(dlist[i],"##################################################################"))
  myModel<-survreg(surv.object ~ vars, data = surv.data,dist = dlist[i])
    
  #2
  print(summary(myModel))
  
  #3
  print(myModel$var)
  
  #4
  myModel$aic<-extractAIC(myModel,k = 2)
  print(myModel$aic)
  
  AICtable[i,1]<-myModel$aic[1]
  AICtable[i,2]<-myModel$aic[2]
}
sink()
print(AICtable)
return(AICtable)
}