#===============================================
# get validation metrics from models
start.validmetric <- function(model_list, eval_metric) {
  if(eval_metric == "logloss") {
    metric <- lapply(model_list, h2o.logloss, valid = TRUE)
  } else if(eval_metric == "MSE") {
    metric <- lapply(model_list, h2o.mse, valid = TRUE)
  } else if(eval_metric == "RMSE") {
    metric <- lapply(model_list, h2o.rmse, valid = TRUE)
  } else if(eval_metric == "MAE") {
    metric <- lapply(model_list, h2o.mae, valid = TRUE)
  } else if(eval_metric == "AUC") {
    metric <- lapply(model_list, h2o.auc, valid = TRUE)
  } else if(eval_metric == "mean_per_class_error") {
    metric <- lapply(model_list, h2o.mean_per_class_error, valid = TRUE)
  }else if(eval_metric == "RMSLE") {
    metric <- lapply(model_list, h2o.rmsle, valid = TRUE)
  } else {
    stop("Choose an eval metric: logloss, MSE, RMSE, MAE, AUC, mean_per_class_error")
  }
  metric
}
