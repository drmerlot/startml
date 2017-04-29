#===============================================
# get test holdout metrics from models # depends on Metrics package for now
test_metric <- function(prediction_list, test, eval_metric, y) {
  predictions_dataframe <- lapply(prediction_list, as.data.frame)
  if(eval_metric == "AUC" | eval_metric == "logloss") {
    predictions <- lapply(predictions_dataframe, function(x)x[,3])
  } else {
    predictions <- lapply(predictions_dataframe, function(x)x[,1])
  }
  actual <- as.data.frame(test[,y])[,1]
  if(eval_metric == "logloss") {
    metric <- lapply(predictions, FUN = logLoss, actual = actual)
  } else if(eval_metric == "MSE") {
    metric <- lapply(predictions, FUN = mse, actual = actual)
  } else if(eval_metric == "RMSE") {
    metric <- lapply(predictions, FUN = mse, actual = actual)
  } else if(eval_metric == "MAE") {
    metric <- lapply(predictions, FUN = mae, actual = actual)
  } else if(eval_metric == "AUC") {
    metric <- lapply(predictions, FUN = auc, actual = actual)
  } else if(eval_metric == "mean_per_class_error") {
    metric <- lapply(predictions, FUN = ce, actual = actual)
  }else if(eval_metric == "RMSLE") {
    metric <- lapply(predictions, FUN = rmsle, actual = actual)
  } else {
    stop("Choose an eval metric: logloss, MSE, RMSE, MAE, AUC, mean_per_class_error = 'ce'")
  }
  metric
}
