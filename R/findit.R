#' Return Index of value from vector
#' 
#' Returns a row number corresponding to the closest value within a vector of data
#' 
#' @param x value to look up
#' @param vec vector of data to look up from
#' @export
FindIt = function(x,vec){
  y = vec - x
  y[y<=0] = NA
  val<-if(all(is.na(y)))NA else which.min(y)
  return(val)
}
