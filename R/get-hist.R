#' get_hist
#'
#' Internal function used in plotting mlblob objects. Retries scoring history from validation given h2o model
#'
#' @param model h2o model from mlblob. No Default.
#' @return Validation history of the model defined by input
#' @export
get_hist <- function(model){
  metric <- model@allparameters$stopping_metric
  if(metric == "RMSLE") {
    metric <- "RMSE"
  } else {
    metric <- metric
  }
  model@model$scoring_history[,which(names(model@model$scoring_history) == paste("validation", tolower(metric), sep = "_"))]
}
