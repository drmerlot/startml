#===============================================================================
# sort the loaded models. needs more fanciness
start.sortmodels <- function(model_list, x, eval_metric) {
  metrics <- lapply(model_list, start.validmetric, eval_metric = eval_metric)
  ranking <- data.frame(mod = seq(1, length(model_list),1), metric = unlist(metrics))
  colnames(ranking) <- c('model_list_num', eval_metric)
  row.names(ranking) <- NULL
  sorted <- ranking[order(ranking$logloss),]
  best_models <- start.bestmodels(sorted, model_list, x = x)
  best_models
}
