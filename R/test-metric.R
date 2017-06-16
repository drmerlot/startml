#' test_metric
#'
#' Caculate performance metrics from models on new data. Depends on Metrics package.
#' @param prediction_list List object of H2O frames containing predictions.
#' No Default.
#' @param test H2O frame object containing labeled data for model evaluation.
#' No Default.
#' @param eval_metric Character object one of logloss, MSE, RMSE, MAE, AUC, or mean_per_class_error.
#' @param y Character object of length 1 identifying the column name of the target variable. No Default.
#' @return List object same length as prediction_list containing performance of each model on test input with selected metric.
#' @export
# get test holdout metrics from models # depends on Metrics package for now
test_metric <- function(prediction_list, test, eval_metric, y) {
  if(eval_metric == "AUC" | eval_metric == "logloss") {
    predictions <- lapply(prediction_list, function(x)x[,3])
  } else {
    predictions <- lapply(prediction_list, function(x)x[,1])
  }
  actual <- test[,y]
  if(eval_metric == "logloss") {
    metric <- lapply(predictions, FUN = logLoss, actual = actual)
  } else if(eval_metric == "MSE") {
    metric <- lapply(predictions, FUN = mse, actual = actual)
  } else if(eval_metric == "RMSE") {
    metric <- lapply(predictions, FUN = rmse, actual = actual)
  } else if(eval_metric == "MAE") {
    metric <- lapply(predictions, FUN = mae, actual = actual)
  } else if(eval_metric == "AUC") {
    metric <- lapply(predictions, FUN = auc, actual = actual)
  } else if(eval_metric == "mean_per_class_error") {
    metric <- lapply(predictions, FUN = ce, actual = actual)
  }else if(eval_metric == "RMSLE") {
    metric <- lapply(predictions, FUN = rmsle, actual = actual)
  } else {
    stop("Choose an eval metric: logloss, MSE, RMSE, MAE, AUC, mean_per_class_error")
  }
  metric
}
