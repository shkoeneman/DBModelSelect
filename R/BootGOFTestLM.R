#' Bootstrap goodness-of-fit procedure for linear models
#' 
#' @description
#' Performs a bootstrap goodness-of-fit procedure to assess the fit of a normal linear regression model
#' 
#' @param model A fitted `lm` object.
#' @param data A dataframe used to fit `model`.
#' @param boot_iter An integer indicating number of bootstrap iterations to perform.
#' @param level Confidence level of the bootstrap interval used in the test.
#' @param return_dist A logical specifying whether to optionally return the bootstrap distribution. Defaults to FALSE.
#' @return A list containing the specification and results of the test.
#' 
#' @examples
#' # generate some data
#' set.seed(5122023)
#' data <- data.frame(s = rnorm(200), t = rnorm(200))
#' data$y <- data$s + rnorm(200)
#' # perform all subsets regression
#' model_list <- FitLMSubsets(response = "y", data = data, intercept = TRUE, force_intercept = FALSE)
#' # determine whether largest candidate model shows lack of fit
#' BootGOFTestLM(model_list[[length(model_list)]], data = data)
#' 
#' @export
BootGOFTestLM <- function(model, data, boot_iter = 1000, level = 0.95, return_dist = FALSE){
  if(!inherits(model,"lm")){
    stop("Model supplied is not a linear model. A model of class lm must be supplied.")
  }
  
  boot_dist <- sapply(1:boot_iter, FUN = function(x){
    boot_data <- data[sample(1:nrow(data), nrow(data), TRUE),]
    return(SandwichEstGOF(lm(formula(model), data = boot_data)))
  })
  boot_int <- unname(quantile(boot_dist, c((1-level)/2,(1-(1-level)/2))))
  
  out <- list(
    null_val = 2*nobs(model),
    level = level,
    boot_iter = boot_iter,
    boot_int = boot_int
  )
  if(return_dist){
    out$boot_dist <- boot_dist
  }
  class(out) <- "BootGOFTestLM"
  return(out)
}

#' @rdname BootGOFTestLM
#' @method print BootGOFTestLM
#' @export
print.BootGOFTestLM <- function(x, ...) {
  cat(paste0("    Bootstrap GOF Test for Linear Models \n \n",
             "null hypothesis: normal linear model does not exhibit lack of fit \n",
             "null value: ",x$null_val,"\n",
             100*x$level," percent bootstrap inverval: \n"))
  cat(x$boot_int)
  
}


SandwichEstGOF <- function(model){
  n <- nobs(model)
  y <- matrix(model.response(model.frame(model)), nrow = n, ncol = 1)
  X <- model.matrix(model)
  p <- ncol(X)
  B <- matrix(coef(model), nrow = p, ncol = 1)
  
  
  sighat2 <- as.numeric(t(y-(X%*%B)) %*% (y-(X%*%B))*(1/n))
  
  inv_info <- matrix(0, nrow = p+1, ncol = p+1)
  inv_info[1:p,1:p] <- (t(X)%*%X)/sighat2
  inv_info[1:p,p+1] <- t(t(y-X%*%B)%*%X/(sighat2^2))
  inv_info[p+1,1:p] <- t(y-X%*%B)%*%X/(sighat2^2)
  inv_info[p+1,p+1] <- -n/(2*sighat2^2) + t(y-X%*%B)%*%(y-X%*%B)/(sighat2^3)
  inv_info <- solve(inv_info)
  
  score_sum <- Reduce("+", lapply(1:n, FUN = function(x){
    score_term <- matrix(c((y[x] - (X[x,]%*%B))%*%t(X[x,])/sighat2,
                        (y[x] - (X[x,]%*%B))^2/(2*sighat2^2) - (1/(2*sighat2))),
                        nrow = p+1, ncol = 1)
    return(score_term%*%t(score_term))
  }))
  
  return(n^2*(1/(sighat2^2))*as.numeric((inv_info%*%score_sum%*%inv_info)[[p+1,p+1]]))
  
}