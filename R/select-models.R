#===========================================================
# get best models by performance and correlation threshold for ensemble
select_models <- function(model_list,
                               test,
                               eval_metric,
                               y,
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
  prediction_list <- predict_blob(test, model_list)
    if(eval_metric == "AUC" | eval_metric == "logloss") {
      predictions_subset <- lapply(prediction_list, function(x)x[,3])
      predictions <- h2o.cbind(predictions_subset)
    } else {
    predictions <- h2o.cbind(prediction_list)
    }
  correlations <- h2o.cor(predictions)
  colnames(correlations) <- seq(1:length(model_list))
  correlations[!lower.tri(correlations)] <- 0
  low_cor_models <- model_list[as.numeric(colnames(correlations[,!apply(correlations,2,
    function(x) any(x > correlation_threshold))]))]
  }
  if(length(low_cor_models) == 0){
    min_message <- min(correlations[correlations != 0])
    warning(paste("No models selected, minimum correlation available is",
                  min_message, "\nReturning models unconstrained by correlation\n"))
    low_cor_models <- model_list
  } else {
    if(is.null(eval_threshold)) {
      low_cor_models
    } else {
      if(!exists("prediction_list")) {
        prediction_list <- predict_blob(test, model_list)
      }
      metrics <- unlist(test_metric(prediction_list, test = test, y = y, eval_metric = eval_metric))
      keep_models <- low_cor_models[eval_fun(metrics, eval_threshold)]
      if(length(keep_models) == 0){
        warning("eval_threshold too optimistic, returning models unconstrained by performance")
        low_cor_models
      } else {
        keep_models
      }
    }
  }
}