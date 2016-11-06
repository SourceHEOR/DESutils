#'Return present value of a one-off cost in the future
#'
#'Reurns present value of a one-off cost at some point in the future
#'
#'@param value.to.discount value of one-off event
#'@param discount.rate discount rate e.g. 0.035
#'@param time.point time at which event occurs (in years)
#'@export

PresentValueOne<-function(value.to.discount,discount.rate,time.point=0){
  pv = value.to.discount * exp(-log(1 + discount.rate) * time.point)
  return (pv)
}