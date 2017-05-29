#' get_hist
#'
#' Internal function used in plotting mlblob objects. Retries scoring history from validation given h2o model
#'
#' @param x model from mlblob. No Default.
#' @return Validation history of the model defined by input
#' @export
get_hist <- function(x) {
  x@model$scoring_history$validation_rmse
}
