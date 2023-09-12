#' Corrected AIC for linear models
#' 
#' @description
#' Calculates corrected AIC for an `lm` linear model object.
#' 
#' @param model A fitted `lm` object.
#' @return The numeric value of of corrected AIC for the supplied linear model object.
#' 
#' @examples
#' #generate data
#' set.seed(9122023)
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' AICc(lm(y~x, data = data))
#' 
#' @export
AICc <- function(model){
  if(!inherits(model,"lm")){
    stop("Model supplied is not a linear model. A model of class lm must be supplied to calculate AICc.")
  }
  return(as.numeric(-2*logLik(model) + (2*model$rank*nobs(model))/(nobs(model)-model$rank-1)))
}