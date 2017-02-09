#========================================================
### function to run all three ...
start.autotrain <- function(train,
                    y_name,
                    y_type,
                    algorithms = c("deeplearning", "randomForest", "gbm"),
                    eval_metric = "AUTO",
                    validation_type = "SharedHoldout", # add RandomHoldout and cv
                    split_seed = NULL,
                    wd = getwd()) {

  model_paths <- NULL

  if(sum(as.numeric(algorithms %in% "deeplearning")) == 1) {
    start.dlgrid(train = train,
                 y_name = y_name,
                 y_type = y_type,
                 eval_metric = eval_metric,
                 split_seed = split_seed)
    model_paths <- c(model_paths, paste(wd, "/dl_models", sep = ""))
  }
  if(sum(as.numeric(algorithms %in% "randomForest")) == 1) {
    start.rfgrid(train = train,
                 y_name = y_name,
                 y_type = y_type,
                 eval_metric = eval_metric,
                 split_seed = split_seed)
    model_paths <- c(model_paths, paste(wd, "/rf_models", sep = ""))
  }
  if(sum(as.numeric(algorithms %in% "gbm")) == 1) {
    start.gbmgrid(train = train,
                  y_name = y_name,
                  y_type = y_type,
                  eval_metric = eval_metric,
                  split_seed = split_seed)
    model_paths <- c(model_paths, paste(wd, "/gbm_models", sep = ""))
  }
  if(sum(as.numeric(algorithms %in% "gbm") + as.numeric(algorithms %in% "randomForest") +
         as.numeric(algorithms %in% "deeplearning") == 0)) {
    stop("Set algorithms to one or a combination of 'deeplearning', 'randomForest', 'gbm'")
  }

  all_models <- start.loadmodels(model_paths)
  all_models

}
