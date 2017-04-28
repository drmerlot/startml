#==================================================================
## Train rf models
start.rfgrid <- function(train,
                         valid,
                         y,
                         x, 
                         eval_metric = "AUTO",
                         wd = getwd(),
                         validation_type = "SharedHoldout", #need to add the others
                         percent_valid_holdout = 20,
                         percent_test_holdout = 20,
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