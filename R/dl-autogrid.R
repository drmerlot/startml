#===============================================================================
# Train Deep learning models
dl_autogrid <- function(train,
                         valid,
                         y,
                         x,
                         eval_metric = "AUTO",
                         wd = getwd(),
                         validation_type = "SharedHoldout",
                         percent_valid_holdout = 10,
                         percent_test_holdout = 10,
                         folds = NULL,
                         deeplearning_runtime_secs = 3600,
                         deeplearning_stopping_rounds = 10,
                         deeplearning_stopping_tolerance = 1e-5,
                         deeplearning_adaptive_rate = TRUE,
                         grid_strategy = "RandomDiscrete") {

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
                             grid_id = "dl", # makes repeat run impossible
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
  dl_path <- paste(wd, "/dl_models", sep = "")
  dl_model_files <- sapply(dl_random_grid@model_ids, function(m) h2o.saveModel(h2o.getModel(m), path = dl_path, force = TRUE))

  # print out alert
  cat(paste("Deep Learning Models Saved To:\n", dl_path, "\n\n"))
  dl_random_grid
}