#' dl_autogrid
#'
#' dl_autogrid is a wrapper employing built-in settings to run grid search hyper parameter optimizations on deep learning (dl_ algorithms)
#'
#' @param train H2O frame object containing labeled data for model training.
#' No Default.
#' @param valid H2O frame object containing labeled data for model validation.
#' No Default.
#' @param y Character object of length 1 identifying the column name of the target variable. No Default.
#' @param x Character object of length 1 or more identifying the column name(s) of the input variables. No Default.
#' @param folds Character object defining number of folds for xval. Default is NULL and currently is not implemented.
#' @param deeplearning_runtime_secs Numeric object defining total number of seconds the hyper parameter grid search will run.
#' @param deeplearning_stopping_rounds Numeric object defining maximum number of training rounds an individual deep learning model not improving will continue to run. Default is 10.
#' @param deeplearning_stopping_tolerance Numeric object which sets the mimmum loss funciton improvement for a training iteration to be considered an improvement. Defulat is 1E-5.
#' @param deeplearning_adaptive_rate Boolean, if TRUE ADELTA is used to control learning rate if FALSE than normal rate controls can be used.
#' @param grid_strategy Character object default and only current supported option is "randomDiscrete"
#' @param eval_metric Character object defining evaluation metric for training. Defualt is "AUTO" and uses built-in H2O automatic choice for target data type.
#' @param wd Character object defining file path where dl_models folder will be created and deep learning models saved. Defaults to current working directory.
#' @param grid_id Character. Set grid name in h2o platform. Defualts to "dl" only used in ensemble function.
#' @return List object containing H2O model objects. Additionally saves h2o models as re-loadable text files in wd/dl_models folder.
#' @export
dl_autogrid <- function(train,
                        valid,
                        y,
                        x,
                        eval_metric = "AUTO",
                        wd = getwd(),
                        folds = NULL,
                        deeplearning_runtime_secs = 10,
                        deeplearning_stopping_rounds = 10,
                        deeplearning_stopping_tolerance = 1e-5,
                        deeplearning_adaptive_rate = TRUE,
                        grid_strategy = "RandomDiscrete",
                        grid_id = "dl") {

  cat("Training Deep Learning Models\n")
  #==============================================
  dl_parameter_search <- list(rate= c(1e-9, 1e-8, 1e-7, 1e-6),
                              rate_annealing= c(1e-12, 1e-9, 1e-6),
                              momentum_start= c(0.8, 0.9),
                              momentum_stable= c(0.95, 0.99),
                              momentum_ramp= 1/seq(1e-12, 1e-9, 1e-6),
                              score_duty_cycle= c(0.02, 0.05, 0.1),
                              activation = c("RectifierWithDropout",
                                             "TanhWithDropout",
                                             "MaxoutWithDropout"),
                              hidden = list(c(200,200,200),
                                            c(512,512,512),
                                            c(32,32,32),
                                            c(64, 64, 64)),
                              input_dropout_ratio = c(0, 0.05, 0.1),
                              hidden_dropout_ratios = list(c(0, 0, 0),
                                                           c(0.1, 0.1, 0.1),
                                                           c(0.2, 0.2, 0.2),
                                                           c(0.5, 0.5, 0.5)),
                              l1= c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 0),
                              l2= c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 0),
                              max_w2= c(10, 20, 40),
                              epsilon = c(1e-10, 1e-8, 1e-6, 1e-4),
                              rho = c(0.97, 0.98, 0.98))

  #========================================================
  # set variable type for proper auto options
  if(deeplearning_adaptive_rate == TRUE) {
    hyper_params <- dl_parameter_search[seq(7,15)]
  }
  if(deeplearning_adaptive_rate == FALSE) {
    hyper_params <- dl_parameter_search[seq(1:13)]
  }

  dl_search_criteria = list(strategy = grid_strategy,
                            max_runtime_secs = deeplearning_runtime_secs,
                            stopping_rounds = deeplearning_stopping_rounds,
                            stopping_tolerance = deeplearning_stopping_tolerance,
                            seed = 1234) # needs to be changable

  # run the grid
  # needs be removed first for iterating within same session
  h2o.rm("dl")
  dl_random_grid <- h2o.grid(algorithm="deeplearning",
                             grid_id = grid_id,
                             training_frame=train,
                             validation_frame = valid,
                             x = x,
                             y = y,
                             standardize = TRUE,
                             epochs=1000, #needs to change
                             overwrite_with_best_model = TRUE,
                             adaptive_rate = deeplearning_adaptive_rate,
                             hyper_params = dl_parameter_search,
                             search_criteria = dl_search_criteria,
                             stopping_metric = eval_metric,
                             seed = 1234) # needs to be changable
  #====================================================
  #dl_grid <- h2o.getGrid("dl")

  # write out the models to disk
  dl_path <- paste(wd, "/", grid_id, "_models", sep = "")
  dl_model_files <- sapply(dl_random_grid@model_ids, function(m) h2o.saveModel(h2o.getModel(m), path = dl_path, force = TRUE))

  # print out alert
  cat(paste("Deep Learning Models Saved To:\n", dl_path, "\n\n"))
  dl_random_grid
}
