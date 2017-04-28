# small funtion needed for qplot==================
get_hist <- function(x) {
  x@model$scoring_history$validation_rmse
}