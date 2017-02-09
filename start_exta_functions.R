#=================================================
# Need to separate all functions to their own files


# make number, performance / correlation threshold would be best..
#use_num <- 1


#wd <- "/Users/grad/Desktop/auto"
#setwd(wd)

# rewrite above as a function .

start.loadmodels <- function(path) {
  all_model_files <- list.files(path, full.names = TRUE)
  all_models  <- sapply(all_model_files, function(m) h2o.loadModel(m))
  all_models
}

# test the function: gotta make this a function
#dl_path <- paste(wd, "/deeplearning_models", sep = "")
#rf_path <- paste(wd, "/rf_models", sep = "")
#gbm_path <- paste(wd, "/gbm_models", sep = "")

#model_paths <- list(dl_path, rf_path, gbm_path)

#all_models <- lapply(model_paths, start.loadmodels)

# get validatino metrics

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

# start.bestmodels <- function(metric, model_list, x) {
#   best_models <- model_list[sorted_models$model_list_num[1:x]]
#   best_models
# }

start.sortmodels <- function(model_list, x, eval_metric) {
  metrics <- lapply(model_list, start.validmetric, eval_metric)
  ranking <- data.frame(mod = seq(1, length(model_list),1), metric = unlist(metrics))
  colnames(ranking) <- c('model_list_num', eval_metric)
  row.names(ranking) <- NULL
  sorted <- ranking[order(ranking$logloss),]
  best_models <- start.bestmodels(sorted, model_list, x = x)
  best_models
}

#===============================================================================

#best_rf_models = sort_models(all_rf_models, 14)
#best_dl_models = sort_models(all_dl_models, 14)
#best_gbm_models = sort_models(all_gbm_models, 14)


# use lapply to walk through instead of above
#best_models <- lapply(all_models, start.sortmodels, x = use_num, eval_metric)

#test the call
#best_models <- lapply(all_models, start.sortmodels, x = use_num, eval_metric = "logloss")

# load_wrappers = function(wrappers_path){
#   source(paste(path, 'ensemble_store_wrappers.R', sep = '/'))
# }

# load all the wrappers, will only use the ones needed
#source(normalizePath(paste(wd, "/ensemble_store_wrappers.R", sep = "")))

