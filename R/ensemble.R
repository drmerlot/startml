#' ensemble
#'
#' @param mlout Inputs an mlblob object containing multiple models from the result of the startml function
#' @param algorithm Character. Algorithm used to create the ensemble. Can be one of the algorimths from startml, or "mean," "median," or "vote." Default is mean.
#' @param keep_features Boolean. When TRUE, orginial input features are mixed with weak learner predictions to create the new input data set for the ensemble. When FALSE, only weak learner esimates are used. Default is FALSE.
#' @param percent_reduce Numeric. Indicates the rough percentage to reduce input dementions by. Default is NULL.
#' @param reduce_method Character. Indicates the dimentional reduction method to use. Can be "PCA" or "auto_encoder." When "auto_encoder" algorithm must be deeplearning, this can greatly increase the length of time nessesary to run an ensemble, but may result in a more stable model. Default is NULL.
#' @param grid_search Boolean. Use a grid search to find optimal hyper parameter when algorithm is "gbm" randomForest" or "deepleaning." Default is FALSE, this can greatly increase the ensemble training time but may result in a superior parameter.
#' @param runtime_secs Numeric. Number of seconds to run grid search when grid_search is TRUE. Default is 60 seconds.
#' @param validation Character. Type of validation to be used in ensemble, can be "random_holdout" or "xval" for cross validation. Default is xval.
#' @return mlblob object including ensemble model, ensemble input data set, and predictions on train, valid, test, and new_data sets if available.
#' @export
ensemble <- function(mlout,
                     algorithm = "mean",
                     validation = "xval",
                     percent_reduce = NULL,
                     reduce_method = NULL,
                     keep_features = FALSE,
                     grid_search = FLASE,
                     runtime_secs = 60) {
  ml_ensemble <- mlout
  if(algorithm == "mean") {
    weak_predictions <- h2o.cbind(mlout@predict_test)
    ensemble_test <- h2o.mean(weak_predictions, axis = 1, return_frame = TRUE)
    weak_predictions <- h2o.cbind(mlout@predict_newdata)
    ensemble_newdata <- h2o.mean(weak_predictions, axis = 1, return_frame = TRUE)
    ml_ensemble@ensemble_model[[1]] <- "Simple mean of weak learners"
    ml_ensemble@ensemble_test[[1]] <- ensemble_test
    ml_ensemble@ensemble_newdata[[1]] <- ensemble_newdata
  }
  ml_ensemble
}
