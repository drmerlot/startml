#===========================================================
# get best models by performance and correlation threshold for ensemble
start.selectmodels <- function(model_list,
                               test,
                               eval_metric,
                               y_name,
                               eval_threshold = NULL,
                               correlation_threshold = NULL) {
  if(eval_metric == "AUC") {
    eval_fun <- function(a, b) {
      a >= b
    }
  } else {
    eval_fun <- function(a, b) {
      a <= b
    }
  }
  if(is.null(correlation_threshold)) {
    low_cor_models <- model_list
  } else {
  prediction_list <- start.predict(test, model_list)
  predictions <- h2o.cbind(prediction_list)
  correlations <- h2o.cor(predictions)
  colnames(correlations) <- seq(1:length(model_list))
  correlations[!lower.tri(correlations)] <- 0
  low_cor_models <- model_list[as.numeric(colnames(correlations[,!apply(correlations,2,
    function(x) any(x > correlation_threhold))]))]
  }
  if(length(low_cor_models) == 0){
    min_message <- min(correlations[correlations != 0])
    stop(paste("No models selected, minimum correlation available is", min_message))
  } else {
    if(is.null(eval_threshold)) {
      low_cor_models
    } else {
      metrics <- unlist(start.testmetric(prediction_list, test = test, y_name = y_name, eval_metric = eval_metric))
      keep_models <- low_cor_models[eval_fun(metrics, eval_threshold)]
    }
  }
  if(length(keep_models) == 0){
    stop(paste("No models selected, choose different eval_threshold"))
  } else {
  keep_models
  }
}


test_out <- start.selectmodels(model_list = models,
                               test = test,
                   eval_metric = "RMSE",
                    eval_threshold = .5,
                   y_name = "target",
                   correlation_threshold = 0.6)

