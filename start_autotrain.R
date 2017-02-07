#========================================================
### function to run all three ...
start.autotrain <- function(train,
                    y_name,
                    y_type,
                    algorithms = c("deeplearning", "randomForest", "gbm"),
                    eval_metirc = "AUTO",
                    validation_type = "SharedHoldout", # add RandomHoldout and cv
                    wd = getwd()) {

  model_paths <- NULL

  if(sum(as.numeric(algorithms %in% "deeplearning")) == 1) {
    start.dlgrid(train = train,
                 y_name = y_name,
                 y_type = y_type,
                 eval_metric = "AUTO")
    model_paths <- c(model_paths, paste(wd, "/dl_models", sep = ""))
  } else if(sum(as.numeric(algorithms %in% "randomForest")) == 1) {
    start.rfgrid(train = train,
                 y_name = y_name,
                 y_type = y_type,
                 eval_metric = "AUTO")
    model_paths <- c(model_paths, paste(wd, "/dl_models", sep = ""))
  } else if(sum(as.numeric(algorithms %in% "gbm")) == 1) {
    start.gbmgrid(train = train,
                  y_name = y_name,
                  y_type = y_type,
                  eval_metric = "AUTO")
    model_paths <- c(model_paths, paste(wd, "/gbm_models", sep = ""))
  } else {
    stop("Set algorithms to one or a combination of 'deeplearning', 'randomForest', 'gbm'")
  }

  all_models <- lapply(model_paths, start.loadmodels)

  # use lapply to walk through instead of above  ..
  use_num <- 1
  best_models <- lapply(all_models, sort_models, use_num)
  best_models

}
