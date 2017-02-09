#==================================================================
## Train rf models
start.rfgrid <- function(train,
                          y_name,
                          y_type,
                          eval_metric = "AUTO",
                          wd = getwd(),
                          validation_type = "SharedHoldout", #need to add the others
                          percent_holdout = 10,
                          folds = 3,
                          rf_min_depth = 1,
                          rf_max_depth = 7,
                          rf_runtime_secs = 20,
                          rf_stopping_rounds = 10,
                          rf_stopping_tolerance = 1e-5,
                          grid_strategy = "RandomDiscrete",
                          split_seed = NULL) {

  cat("Training Random Forest Models\n")
  # break the data for holdout validation
  if(is.null(split_seed)) {
    split_seed <- round(runif(1, -1000000, 1000000))
  }
  # need condition for other holdouts
  if(validation_type == "SharedHoldout") {
    splits <- h2o.splitFrame(train, 1 - (percent_holdout/100), seed = split_seed)
    train  <- h2o.assign(splits[[1]], "train.hex")
    valid  <- h2o.assign(splits[[2]], "valid.hex")
  }
  # define the target and predictors
  y <- y_name
  x <- setdiff(names(df1), y)

  # set variable type for proper auto options
  if(y_type == "discrete") {
    train[,y] <- as.factor(train[,y])
    valid[,y] <- as.factor(valid[,y])
    df1[,y] <- as.factor(df1[,y])
  } else {
    train[,y] <- as.numeric(train[,y])
    valid[,y] <- as.numeric(valid[,y])
    df1[,y] <- as.numeric(df1[,y])
  }

### here for grid
hyper_params = list(
  max_depth = seq(rf_min_depth, rf_max_depth, 1),
  sample_rate = c(0.2, 0.4, 0.5, 0.7, 0.9),
  col_sample_rate_per_tree = c(0.2, 0.4, 0.5, 0.9, 1),
  col_sample_rate_change_per_level = c(0.2, 0.4, 0.5, 0.9, 1),
  min_rows = 2^seq(0,log2(nrow(train))-1,1),
  nbins = 2^seq(4,10,1),
  nbins_cats = 2^seq(4,12,1),
  min_split_improvement = c(0,1e-8,1e-6,1e-4),
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
)

search_criteria = list(
  strategy = grid_strategy,
  max_runtime_secs = rf_runtime_secs,
  stopping_rounds = rf_stopping_rounds,
  stopping_tolerance = rf_stopping_tolerance,
  stopping_metric = eval_metric,
  seed = 1234
)

rf_grid_rand <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  algorithm = "randomForest",
  grid_id = "rf_grid_random",
  x = x,
  y = y,
  training_frame = train,
  validation_frame = valid,
  ntrees = 4000,
  seed = 1234
)


# get the grid and sort by logloss
rf_grid <- h2o.getGrid("rf_grid_random", decreasing=FALSE)


# write out the models to disk
rf_path <- paste(wd, "/rf_models", sep = "")
rf_model_files <- sapply(rf_grid@model_ids, function(m) h2o.saveModel(h2o.getModel(m), path = rf_path, force = TRUE))

# print out, needs work
cat(paste("Random Forest Models Saved To:\n", rf_path, "\n\n"))
rf_grid

}