#===============================================
# get validation metrics from models
start.validmetric <- function(model_list, eval_metric) {
  if(eval_metric == "logloss") {
    metric <- h2o.logloss(model_list, valid = TRUE)
  } else if(eval_metric == "MSE") {
    metric <- h2o.mse(model_list, valid = TRUE)
  } else if(eval_metric == "RMSE") {
    metric <- h2o.rmse(model_list, valid = TRUE)
  } else if(eval_metric == "MAE") {
    metric <- h2o.mae(model_list, valid = TRUE)
  } else if(eval_metric == "AUC") {
    metric <- h2o.auc(model_list, valid = TRUE)
  } else if(eval_metric == "mean_per_class_error") {
    metric <- h2o.mean_per_class_error(model_list, valid = TRUE)
  } else {
    stop("Choose and eval metric logloss, MSE, RMSE, MAE, AUC, mean_per_class_error")
  }
  metric
}
