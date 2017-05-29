#' predict_blob
#'
#' Uses all selected models stored in mlblob object to make predictions
#'
#' @param test H2O frame object used as new data input.
#' @param selected_models List of H2O models to predict on new data.
#' @return List of predictions same as length selected_models
#' @export
predict_blob <- function(test, selected_models) {
  #cat("Predicting on New Data With Selected Models\n")
  predictions <- lapply(selected_models, h2o.predict, newdata = test)
  predictions
}
