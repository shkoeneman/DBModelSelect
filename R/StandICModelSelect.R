#' Model selection using standardized information criteria
#' 
#' @description
#' Perform model selection on a list of models using standardized information criteria.
#' 
#' @param x A list containing the fitted model objects on which to perform model selection. Model objects must have a \code{logLik} method defined for them.
#' @param IC A character string containing the base information criteria to use. Options are "AIC", "BIC", and "AICc" for linear models. Default option is `AIC`.
#' @param ref_model_index An integer with the index of the largest candidate model to use as the reference. If not supplied, defaults to the model with largest number of estimated coefficients in \code{x}.
#' @param sd_cutoff A numeric describing how many standard deviations to use when formulating a cutoff for model viability.
#' @param user_df An optional vector the same length as \code{x} where one can specify the degrees of freedom of each fitted model. If not supplied, the degrees of freedom for each model is calculated to be the number of estimated regression coefficients.
#' @param ... Additional arguments.
#' 
#' @return A list containing the final model selected in addition to standardized information criteria and difference in degrees of freedom for all candidate models.
#' 
#' @examples
#' # example code
#' # generate some data
#' s <- rnorm(200)
#' t <- rnorm(200)
#' y <- s + rnorm(200)
#' # formulate and fit models
#' model_list <- list(lm(y~1), lm(y~s), lm(y~t), lm(y~s+t))
#' # perform model selection
#' model_select <- StandICModelSelect(model_list, IC = "AIC")
#' # display best model
#' model_select$best_model
#' @export
StandICModelSelect <- function(x, IC = "AIC", ref_model_index = NULL, sd_cutoff = 2, user_df = NULL, ...){
  if(!(IC[1] %in% c("AIC", "BIC", "AICc"))){
    stop("Selected IC is not a valid option. Please select a valid IC.")
  }
  if(!inherits(x,"list") | length(x) == 0){
    stop("Supplied model list is empty or not a list. Please supply a valid model list.")
  }

  if(!is.null(user_df)){
    df_vec <- user_df 
  } else{
    df_vec <- sapply(x, FUN = function(t){return(length(coef(t)))})
  }
  IC_vec <- sapply(x, FUN = function(t){return(eval(parse(text = paste0(IC,"(t)"))))})
  
  ref_model_index <- ifelse(is.null(ref_model_index),which.max(df_vec),ref_model_index)
  tryCatch({invisible(logLik(x[[ref_model_index]]))},
           error = function(cond){message("Reference model object does not have logLik method.
                                          Please submit a valid model object.")}
  )
  
  ref_IC <- eval(parse(text = paste0(IC,"(x[[",ref_model_index,"]])")))
  ref_df <- length(coef(x[[ref_model_index]]))
  n <- nobs(x[[ref_model_index]])
  
  if(sum(df_vec >= ref_df) > 1){
    stop("Reference model is not the largest candidate model. Please specify the sole largest candidate model.")
  }
  
  if(IC == "AICc"){
    max_expect <- max(sqrt((ref_df-df_vec)/2) + 
                        ((2*(df_vec))*(n/(n-df_vec-1)) - 
                           2*ref_df*(n/(n-ref_df-1)))/sqrt(2*(ref_df-df_vec)), na.rm = TRUE )
  }
  
  stand_IC <- sapply(1:length(x), FUN = function(t){
    model <- x[[t]]
    df <- df_vec[t]
    IC_val <- eval(parse(text = paste0(IC,"(model)")))
    if(df == ref_df){
      return(
        switch(IC,
               AIC = 0,
               BIC = sqrt(1/2)*(1-log(n)),
               AICc = max_expect)
      )
    } else{
      return((IC_val- ref_IC)/sqrt(2*(ref_df - df)))
    }
  })
  cutoff <- switch(IC,
            AIC = 0 + sd_cutoff,
            BIC = sqrt(1/2)*(1-log(n)) + sd_cutoff,
            AICc = max_expect + sd_cutoff)
  meets_cutoff <- (stand_IC <= cutoff)
  best_model_index <- ((1:length(x))[meets_cutoff])[which(df_vec[meets_cutoff] == min(df_vec[meets_cutoff]))]
  best_model_index <- best_model_index[which.min(stand_IC[best_model_index])]
  
  out <- list(
    best_model_index = best_model_index,
    best_model = x[[best_model_index]],
    meets_cutoff = meets_cutoff,
    IC = IC,
    sd_cutoff = sd_cutoff,
    model_cutoff = cutoff,
    stand_IC = stand_IC,
    df_diff = ref_df - df_vec,
    ref_model_index = ref_model_index
  )
  
  class(out) <- "StandICModelSelect"
  
  return(out)
  
}

#' @rdname StandICModelSelect
#' @method print StandICModelSelect
#' @export
print.StandICModelSelect <- function(x, ...) {
  cat(paste0('Model Selection using Standardized ', x$IC, '\n',
      'Selected Model:'))
  print(x$best_model)
}


#' @rdname StandICModelSelect
#' @method plot StandICModelSelect
#' @export
plot.StandICModelSelect <- function(x, ...){
  with(x, {
    plot(df_diff, stand_IC, type = "p",
         ylab = paste0("Standardized ",IC), xlab = paste0("Difference in DOF"))
    abline(h = model_cutoff, col = "blue")
    points(df_diff[stand_IC < model_cutoff], stand_IC[stand_IC < model_cutoff],
           col = "green")
    points(df_diff[stand_IC >= model_cutoff], stand_IC[stand_IC > model_cutoff],
           col = "red")
  })
}