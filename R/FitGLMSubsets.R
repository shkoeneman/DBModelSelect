#' Perform all subsets regression for generalized linear models
#' 
#' @description
#' Fit a specified generalized linear model on all subsets of covariates supplied. Produces an output suitable for use with the `StandICModelSelect` function.
#' 
#' @param response A character string specifying the name of the response variable.
#' @param data A dataframe containing a column corresponding to the response variable in addition to columns for each covariate of interest.
#' @param family A family suitable for supplying to the `glm` function specifying the error distribution and link function.
#' @param intercept A logical indicating whether an intercept term should be considered in models. Defaults to TRUE.
#' @param force_intercept A logical indicating whether to force an intercept term into all models if an intercept is desired. Defaults to TRUE.
#' @param ... Additional arguments that may be supplied when calling `glm` to fit the models of interest.
#' 
#' @return A list of fitted linear models suitable for use with the `StandICModelSelect` function.
#' 
#' @examples
#' # example code
#' # generate some data
#' data <- data.frame(s = rnorm(200), t = rnorm(200))
#' data$y <- data$s + rnorm(200)
#' # perform all subsets regression
#' model_list <- FitGLMSubsets(response = "y", data = data, family = gaussian(),
#'   intercept = TRUE, force_intercept = TRUE)
#' # perform model selection
#' model_select <- StandICModelSelect(model_list, IC = "AIC")
#' @export
FitGLMSubsets <- function(response, data, family, intercept = TRUE, force_intercept = TRUE, ...){
  if(!inherits(response, "character") || sum(names(data) == response) < 1){
    stop("Please supply a proper response argument, that being a character string containing the name of a column in the data.")
  }
  covar_names <- names(data)[! names(data) %in% response]
  
  all_fits <- lapply(seq_along(covar_names), function(x) combn(covar_names, x, function(z) 
    glm(reformulate(z, response, intercept = intercept), data = data, family = family), simplify = FALSE))
  if(intercept & !force_intercept){
    all_fits <- c(lapply(seq_along(covar_names), function(x) combn(covar_names, x, function(z) 
      glm(reformulate(z, response, intercept = FALSE), data = data, family = family), simplify = FALSE)), all_fits)
  }
  all_fits <- unlist(all_fits, recursive = FALSE)
  if(intercept){
    all_fits <- c(list(glm(as.formula(paste0(response,"~1")), data = data, family = family)), all_fits)
  }
  return(all_fits)
}