#' Estimate standard error or coefficient of variation
#' 
#' Returns standard error around vector of data x.
#' 
#' @param x vector of data
#' @export
#' @examples
#' mydata <- c(1, 2, 1, 6, 7, 8, 10)
#' CoefVar(mydata)

CoefVar <- function(x) {
se <- sd(x)/sqrt(length(x))
return(se)
}