#' select_models
#'
#' select_models identifies best models in mlblob object by performance and pearson correlation thresholds.
#' @param model_list List object of H2O model objects to be subbsetted by performance and correlation thresholds
#' No Default.
#' @param test H2O frame object containing labeled data for model evaluation.
#' No Default.
#' @param y Character object of length 1 identifying the column name of the target variable. No Default.
#' @param eval_metric Character object one of logloss, MSE, RMSE, MAE, AUC, or mean_per_class_error.
#' @param eval_threshold Numeric object defining the performance threshold models must meet to be used in prediction. Is minimum for maximization loss function (i.e., AUC) and maximum for minimization loss functions (logloss, MSE, etc). Default is NULL, returns models without performance consideration.
#' @param correlation_threshold Numeric object defining the maximum person correlation allowed in the group of resulting models. If two models show high correlation, the one with surperior performance will be kept and the other dropped. Value ranges from -1 to 1, default is NULL, returning models without correlation considered.
#' @return List object containing H2O model objects adhearing to threshold standards set in input arguments.
#' @export
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
  names(correlations) <- seq(1:length(model_list))
  correlations[!lower.tri(correlations)] <- 0
  low_cor_models <- model_list[as.numeric(colnames(correlations[,!apply(correlations,2,
    function(x) any(x > correlation_threshold))]))]
  }
  if(length(low_cor_models) == 0){
    min_message <- min(apply(correlations, 2, max))
    warning(paste("No models selected, minimum correlation available",
                  min_message, "\nReturning models unconstrained by correlation\n"))
    low_cor_models <- model_list
  } else {
    if(is.null(eval_threshold)) {
      return_models <- low_cor_models
    } else {
      prediction_list <- predict_blob(test, low_cor_models)
      metrics <- unlist(test_metric(prediction_list, test = test, y = y, eval_metric = eval_metric))
      keep_models <- low_cor_models[eval_fun(metrics, eval_threshold)]
      if(length(keep_models) == 0){
        warning("eval_threshold too optimistic, returning models unconstrained by performance")
        return_models <- low_cor_models
      } else {
        return_models <- keep_models
      }
    }
  }
  return_models
}

