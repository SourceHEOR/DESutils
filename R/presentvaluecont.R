#'Present value of continous quantity
#'
#'Provides present value of a quantity which is accrued continuously. An example might
#'be QALYs. In perfect health, these are accrued at 1 per year. They are discounted at
#'3.5 percent.
#'
#'@param annual.increase annual increase in the quantity of interest
#'@param discount.rate e.g. enter 0.035 for 3.5 percent.
#'@param time.to.change how long the quantity is accrued for
#'@param time.at.start how far through the model are we when we start counting. If we
#'  are at the start, this value is 0.
#'@export
#'@examples
#'#5 years in perfect health
#'qalys.per.year <- 1
#'disc.rate <- 0.035
#'duration <- 5 #5 years
#'
#'#At start of model
#'PresentValueCont(qalys.per.year, disc.rate, duration)
#'
#'#10 years into model
#'PresentValueCont(qalys.per.year, disc.rate, duration, time.at.start=10)

PresentValueCont<-function(annual.increase,discount.rate,time.to.change,time.at.start=0){
  pv <- ((annual.increase * exp(-log(1 + discount.rate) * time.at.start)) / log(1 + discount.rate)) * (1 - exp(-log(1 + discount.rate) * time.to.change))
  return(pv) 
}