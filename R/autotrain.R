#' autotrain
#'
#' Autotrain implements H2O grid search to automatically build machine learning
#' models
#'
#' @param train H2O frame object containing labeled data for model training.
#' No Default.
#' @param valid H2O frame object containing labeled data for model validation.
#' No Default.
#' @param y Character object of length 1 identifying the column name of the target variable. No Default.
#' @param x Character object of length 1 or more identifying the column name(s) of the input variables. No Default.
#' @param algorithms Character object of length 3, 2, or 1, specifying which alrogrithms to automatically train. The autotrain function will run a separate grid search for each algorimth type. Choices are: "deeplearning", "randomForest", and "gbm" following the naming convention in H2O version 3. Defaults to c("deeplearning", "randomForest", "gbm").
#' @param eval_metric Character object defining evaluation metric for training. Defualt is "AUTO" and uses built-in H2O automatic choice for target data type.
#' @param validation_type Defines validation type for training models.  Defaults to "shared_holdout" indicating all model built with all algorithms share the same validation set. Currently, this is the only option in autotrain. Planned types include "random_holdout" where each model will get a unique randomized sample of labeled data for validation, and "xval" in which the cross validation functionality in H2O will be implemented in every model.
#' @param runtime_secs Character Object which sets the length of time each grid search will run. Defaults to 20, thus the default runtime is 20 sec * (length of algorimths) = 1 minute.
# @keywords gradient boosting, deep learning, random forest, gird serach optimization, automatic, training
#' @param wd Character object defining file path where resulting modeling will be saved. Defualts to current working directory.
#' @return List object containing H2O model objects
#' @export
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
