#' rf_autogrid
#'
#' rf_autogrid is a wrapper employing built-in settings to run grid search hyper parameter optimizations on the random forest algorithm.
#'
#' @param train H2O frame object containing labeled data for model training.
#' No Default.
#' @param valid H2O frame object containing labeled data for model validation.
#' No Default.
#' @param y Character object of length 1 identifying the column name of the target variable. No Default.
#' @param x Character object of length 1 or more identifying the column name(s) of the input variables. No Default.
#' @param folds Character object defining number of folds for xval. Default is NULL and currently is not implemented.
#' @param rf_runtime_secs Numeric object defining total number of seconds the hyper parameter grid search will run.
#' @param rf_stopping_rounds Numeric object defining maximum number of training rounds an individual deep learning model not improving will continue to run. Default is 10.
#' @param rf_stopping_tolerance Numeric object which sets the mimmum loss funciton improvement for a training iteration to be considered an improvement. Defulat is 1E-5.
#' @param rf_min_depth Numeric object which sets the minimum tree depth for all random forest models. Defaut is 1.
#' @param rf_max_depth Numeric object which sets the maximum tree depth for all random forest models. Defaut is 7.
#' @param grid_strategy Character object default and only current supported option is "randomDiscrete"
#' @param eval_metric Character object defining evaluation metric for training. Defualt is "AUTO" and uses built-in H2O automatic choice for target data type.
#' @param wd Character object defining file path where dl_models folder will be created and deep learning models saved. Defaults to current working directory.
#' @return List object containing H2O model objects. Additionally saves h2o models as re-loadable text files in wd/rf_models folder.
#' @export
rf_autogrid <- function(train,
                        valid,
                        y,
                        x,
                        eval_metric = "AUTO",
                        wd = getwd(),
                        folds = NULL,
                        rf_min_depth = 1,
                        rf_max_depth = 7,
                        rf_runtime_secs = 20,
                        rf_stopping_rounds = 10,
                        rf_stopping_tolerance = 1e-5,
                        grid_strategy = "RandomDiscrete") {

  cat("Training Random Forest Models\n")
### here for grid
  rf_parameter_search <- list(max_depth = seq(rf_min_depth, rf_max_depth, 1),
                             sample_rate = c(0.2, 0.4, 0.5, 0.7, 0.9),
                             col_sample_rate_per_tree = c(0.2, 0.4, 0.5, 0.9, 1),
                             col_sample_rate_change_per_level = c(0.2, 0.4, 0.5, 0.9, 1),
                             min_rows = 2^seq(0,log2(nrow(train))-1,1),
                             nbins = 2^seq(4,10,1),
                             nbins_cats = 2^seq(4,12,1),
                             min_split_improvement = c(0,1e-8,1e-6,1e-4),
                             histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin"))

  rf_search_criteria <- list(strategy = grid_strategy,
                            max_runtime_secs = rf_runtime_secs,
                            stopping_rounds = rf_stopping_rounds,
                            stopping_tolerance = rf_stopping_tolerance,
                            stopping_metric = eval_metric,
                            seed = 1234)

  # needs be removed first for iterating within same session
  h2o.rm("rf")
  rf_random_grid <- h2o.grid(hyper_params = rf_parameter_search,
                             search_criteria = rf_search_criteria,
                             algorithm = "randomForest",
                             grid_id = "rf",
                             x = x,
                             y = y,
                             training_frame = train,
                             validation_frame = valid,
                             ntrees = 4000, # must be changable
                             seed = 1234) # must change

  #================================================
  #rf_grid <- h2o.getGrid("rf")

  # write out the models to disk
  rf_path <- paste(wd, "/rf_models", sep = "")
  rf_model_files <- sapply(rf_random_grid@model_ids, function(m) h2o.saveModel(h2o.getModel(m), path = rf_path, force = TRUE))

  # print out, needs work
  cat(paste("Random Forest Models Saved To:\n", rf_path, "\n\n"))
  rf_random_grid
}
