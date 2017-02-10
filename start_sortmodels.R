#===============================================================================
# sort the loaded models. needs more fanciness
start.sortmodels <- function(model_list, eval_metric) {
  metrics <- start.validmetric(model_list, eval_metric = eval_metric)
  ranking <- data.frame(mod = seq(1, length(model_list), 1), metric = unlist(metrics))
  colnames(ranking) <- c('model_list_num', eval_metric)
  row.names(ranking) <- NULL
  if(eval_metric == 'AUC') {
    sorted <- ranking[order(ranking[,2], decreasing = TRUE),]
  } else {
    sorted <- ranking[order(ranking[,2], decreasing = FALSE),]
  }
  sorted
}
