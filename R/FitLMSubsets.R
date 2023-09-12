#' Perform all subsets linear regression
#' 
#' @description
#' Perform linear regression on all subsets of covariates supplied. Produces an output suitable for use with the `StandICModelSelect` function.
#' 
#' @param response A character string specifying the name of the response variable.
#' @param data A dataframe containing a column corresponding to the response variable in addition to columns for each covariate of interest.
#' @param intercept A logical indicating whether an intercept term should be considered in models. Defaults to TRUE.
#' @param force_intercept A logical indicating whether to force an intercept term into all models if an intercept is desired. Defaults to TRUE.
#' 
#' @return A list of fitted linear models suitable for use with the `StandICModelSelect` function.
#' 
#' @examples
#' # example code
#' # generate some data
#' data <- data.frame(s = rnorm(200), t = rnorm(200))
#' data$y <- data$s + rnorm(200)
#' # perform all subsets regression
#' model_list <- FitLMSubsets(response = "y", data = data, intercept = TRUE, force_intercept = TRUE)
#' # perform model selection
#' model_select <- StandICModelSelect(model_list, IC = "AIC")
#' @export
FitLMSubsets <- function(response, data, intercept = TRUE, force_intercept = TRUE){
  if(!inherits(response, "character") || sum(names(data) == response) < 1){
    stop("Please supply a proper response argument, that being a character string containing the name of a column in the data.")
  }
  covar_names <- names(data)[! names(data) %in% response]
  
  all_fits <- lapply(seq_along(covar_names), function(x) combn(covar_names, x, function(z) 
    lm(reformulate(z, response, intercept = intercept), data = data), simplify = FALSE))
  if(intercept & !force_intercept){
    all_fits <- c(lapply(seq_along(covar_names), function(x) combn(covar_names, x, function(z) 
      lm(reformulate(z, response, intercept = FALSE), data = data), simplify = FALSE)), all_fits)
  }
  all_fits <- unlist(all_fits, recursive = FALSE)
  if(intercept){
    all_fits <- c(list(lm(as.formula(paste0(response,"~1")), data = data)), all_fits)
  }
  return(all_fits)
}