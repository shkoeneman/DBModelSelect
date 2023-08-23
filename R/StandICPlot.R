#' Plot standardized information criteria
#' 
#' @description
#' Create a basic plot of the results of a model selection using standardized information criteria.
#' 
#' @param model_select A list that was output using the `StandICModelSelect` function.
#' 
#' @examples
#' # example code
#' # perform model selection
#' model_select <- StandICModelSelect(model_list, IC = "AIC")
#' # plot results of model selection
#' StandICPlot(model_select)
#' @export
StandICPlot <- function(model_select){
  with(model_select, {
    plot(df_diff, stand_IC, type = "p",
         ylab = paste0("Standardized ",IC), xlab = paste0("Difference in DOF"))
    abline(h = model_cutoff, col = "blue")
    points(df_diff[stand_IC < model_cutoff], stand_IC[stand_IC < model_cutoff],
           col = "green")
    points(df_diff[stand_IC >= model_cutoff], stand_IC[stand_IC > model_cutoff],
           col = "red")
  })
}