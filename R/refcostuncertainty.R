#'Summary statistics for selection of NHS ref costs
#'
#'Returns weighted mean, std. error (log scale) and 95% CI for a group of NHS Reference
#'Costs. Assumes that they follow a log standard normal distribution.
#'
#'@param data file path a .csv file in the same format as NHS ref costs, with 9 columns
#'  of data
#'@export

RefCostRange <- function(data){
#data is a file path to a .csv file in the same format as NHS ref costs; 9 columns of data
  
low.cut <- optimize(SumWeights,data=data,low=TRUE,maximum=FALSE,lower=0, upper=10)  
low.quartile <- exp(low.cut$minimum[1])

up.cut <- optimize(SumWeights,data=data,low=FALSE,maximum=FALSE,lower=0, upper=10)  
up.quartile <- exp(up.cut$minimum[1])

stdv <- (up.cut$minimum[1] - low.cut$minimum[1]) / (qnorm(p=0.75,mean=0,sd=1) - qnorm(p=0.25,0,1))

mydata <- read.csv(data,head=T,sep=",")
data.subs <- sum(mydata[,9])
se <- stdv / sqrt(data.subs)

mydata$wght <- mydata[,4]*(mydata[,3] / sum(mydata[,3]))
myweighted.mean <- sum(mydata$wght)

int <- qnorm(p=.975,mean=0,sd=1)*se
low.95 <- exp(log(myweighted.mean) - int)
up.95 <- exp(log(myweighted.mean) + int)

cat(paste(" Mean: ",format(myweighted.mean), '\n', "Std Error (log scale): ",format(se), '\n',"95% CI: ", format(low.95)," - ",format(up.95)),sep="")

myobj<-c(as(myweighted.mean, "numeric"),as(se, "numeric"), as(low.95, "numeric"), as(up.95, "numeric"))
return(myobj)
}



