#' gbm_autogrid
#'
#' gbm_autogrid is a wrapper employing built-in settings to run grid search hyper parameter optimizations on gradient boosted machine algorithm.
#'
#' @param train H2O frame object containing labeled data for model training.
#' No Default.
#' @param valid H2O frame object containing labeled data for model validation.
#' No Default.
#' @param y Character object of length 1 identifying the column name of the target variable. No Default.
#' @param x Character object of length 1 or more identifying the column name(s) of the input variables. No Default.
#' @param folds Character object defining number of folds for xval. Default is NULL and currently is not implemented.
#' @param gbm_runtime_secs Numeric object defining total number of seconds the hyper parameter grid search will run.
#' @param gbm_stopping_rounds Numeric object defining maximum number of training rounds an individual deep learning model not improving will continue to run. Default is 10.
#' @param gbm_stopping_tolerance Numeric object which sets the mimmum loss funciton improvement for a training iteration to be considered an improvement. Defulat is 1E-5.
#' @param gbm_min_depth Numeric object which sets the mimmum loss funciton improvement for a training iteration to be considered an improvement. Defulat is 1E-5.
#' @param gbm_max_depth Numeric object which sets the maximum tree depth for all gbm models. Defulat is 7.
#' @param grid_strategy Character object default and only current supported option is "randomDiscrete"
#' @param eval_metric Character object defining evaluation metric for training. Defualt is "AUTO" and uses built-in H2O automatic choice for target data type.
#' @param wd Character object defining file path where dl_models folder will be created and deep learning models saved. Defaults to current working directory.
#' @return List object containing H2O model objects. Additionally saves h2o models as re-loadable text files in wd/gbm_models folder.
#' @export
gbm_autogrid <- function(train,
                         valid,
                         y,
                         x,
                         eval_metric = "AUTO",
                         wd = getwd(),
                         folds = NULL,
                         gbm_min_depth = 1,
                         gbm_max_depth = 7,
                         gbm_runtime_secs = 10,
                         gbm_stopping_rounds = 10,
                         gbm_stopping_tolerance = 1e-5,
                         grid_strategy = "RandomDiscrete") {

  cat("Training Gradient Boosting Models\n")
  #============================================================
  # needs to be reviewed for smart values and changable...
  # score_tree_interval = c(2, 5, 10),
  gbm_parameter_search = list(
    max_depth = seq(gbm_min_depth, gbm_max_depth, 1),
    sample_rate = seq(0.2, 1, 0.01),
    col_sample_rate = seq(0.2,0.9,1),
    col_sample_rate_per_tree = seq(0.2, 0.9, 1),
    col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
    min_rows = 2^seq(0,log2(nrow(train))-1,1),
    nbins = 2^seq(4,10,1),
    nbins_cats = 2^seq(4,12,1),
    min_split_improvement = c(0,1e-8,1e-6,1e-4),
    learn_rate = c(0.1, 0.01, 0.001),
    learn_rate_annealing = c(0.1, 0.5, .99), ## check this for reasonableness
    histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
  )

  gbm_search_criteria = list(
    strategy = grid_strategy,
    max_runtime_secs = gbm_runtime_secs,
    stopping_rounds =  gbm_stopping_rounds,
    stopping_tolerance = gbm_stopping_tolerance,
    stopping_metric = eval_metric,
    seed = 1234 # needs to be changable
  )

  # needs be removed first for iterating within same session
  h2o.rm("gbm")
  gbm_random_grid <- h2o.grid(algorithm = "gbm",
                              grid_id = "gbm", # this causes failure on repreat runs, but automatic names give huge model ids
                              x = x,
                              y = y,
                              training_frame = train,
                              validation_frame = valid,
                              ntrees = 4000, # has to be adjustable
                              hyper_params = gbm_parameter_search,
                              search_criteria = gbm_search_criteria,
                              seed = 1234)
  #====================================
  #gbm_grid <- h2o.getGrid("gbm") # already returns grid

  # write out the models to disk
  gbm_path <- paste(wd, "/gbm_models", sep = "")
  gbm_model_files <- sapply(gbm_random_grid@model_ids, function(m) h2o.saveModel(h2o.getModel(m), path = gbm_path, force = TRUE))

  # print out
  cat(paste("gbm Models Saved To:\n", gbm_path, "\n\n"))
  gbm_random_grid
}
