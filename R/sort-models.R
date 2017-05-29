#' sort_models
#'
#' Returns a index data frame giving ranked placement best models in model_list. Internal function used for model selection.
#'
#' @param model_list List object of H2O frames containing h2o model objects.
#' @param eval_metric Character object one of logloss, MSE, RMSE, MAE, AUC, or mean_per_class_error.
#' No Default.
#' @return Data frame indexing validation model performance of models in model_list.
#' @export
sort_models <- function(model_list, eval_metric) {
  metrics <- valid_metric(model_list, eval_metric = eval_metric)
  ranking <- data.frame(mod = seq(1, length(model_list), 1), metric = unlist(metrics))
  colnames(ranking) <- c("model_list_num", eval_metric)
  row.names(ranking) <- NULL
  if(eval_metric == "AUC") {
    sorted <- ranking[order(ranking[,2], decreasing = TRUE),]
  } else {
    sorted <- ranking[order(ranking[,2], decreasing = FALSE),]
  }
  sorted
}
