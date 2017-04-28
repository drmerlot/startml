#========================================================
### function to run all three ...
autotrain <- function(train,
                            valid, 
                            y,
                            x,
                            algorithms = c("deeplearning", "randomForest", "gbm"),
                            eval_metric = "AUTO",
                            validation_type = "SharedHoldout", # add RandomHoldout and cv
                            runtime_secs = 10,
                            wd = getwd()) {

  model_paths <- NULL

  if(sum(as.numeric(algorithms %in% "deeplearning")) == 1) {
    dl_autogrid(train = train,
                 valid = valid,
                 y = y,
                 x = x, 
                 eval_metric = eval_metric,
                 deeplearning_runtime_secs = runtime_secs)
    model_paths <- c(model_paths, paste(wd, "/dl_models", sep = ""))
  }
  if(sum(as.numeric(algorithms %in% "randomForest")) == 1) {
    rf_autogrid(train = train,
                 valid = valid,
                 y = y,
                 x = x, 
                 eval_metric = eval_metric,
                 rf_runtime_secs = runtime_secs)
    model_paths <- c(model_paths, paste(wd, "/rf_models", sep = ""))
  }
  if(sum(as.numeric(algorithms %in% "gbm")) == 1) {
    gbm_autogrid(train = train,
                  valid = valid,
                  y = y,
                  x = x, 
                  eval_metric = eval_metric,
                  gbm_runtime_secs = runtime_secs)
    model_paths <- c(model_paths, paste(wd, "/gbm_models", sep = ""))
  }
  if(sum(as.numeric(algorithms %in% "gbm") + as.numeric(algorithms %in% "randomForest") +
         as.numeric(algorithms %in% "deeplearning") == 0)) {
    stop("Set algorithms to one or a combination of 'deeplearning', 'randomForest', 'gbm'")
  }

  all_models <- load_models(model_paths)
  all_models
}