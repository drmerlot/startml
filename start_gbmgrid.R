#==================================================================
### Train gbm models
start.gbmgrid <- function(train,
                          y_name,
                          y_type,
                          eval_metric = "AUTO",
                          validation_type = "SharedHoldout",
                          wd = getwd(),
                          percent_holdout = 0.1,
                          gbm_min_depth = 1,
                          gbm_max_depth = 7,
                          gbm_runtime_secs = 10,
                          gbm_stopping_rounds = 10,
                          gbm_stopping_tolerance = 1e-5,
                          grid_strategy = "RandomDiscrete",
                          split_seed = NULL) {

  cat("Training Gradient Boosting Models\n")
  # break the data for holdout validation
  splits <- h2o.splitFrame(train, 1 - percent_holdout, seed=1234)
  train  <- h2o.assign(splits[[1]],  "train.hex") # 80%
  valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%

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

  # needs to be reviewed for smart values ...
  # score_tree_interval = c(2, 5, 10),
  hyper_params = list(
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

  search_criteria = list(
    strategy = grid_strategy,
    max_runtime_secs = gbm_runtime_secs,
    stopping_rounds =  gbm_stopping_rounds,
    stopping_tolerance = gbm_stopping_tolerance,
    stopping_metric = eval_metric,
    seed = 1234
  )

  gbm_random_grid <- h2o.grid(
    algorithm = "gbm",
    grid_id = "gbm_grid_random",
    x = x,
    y = y,
    training_frame = train,
    validation_frame = valid,
    ntrees = 4000,
    hyper_params = hyper_params,
    search_criteria = search_criteria,
    seed = 1234
  )


  gbm_grid <- h2o.getGrid("gbm_grid_random")


  # write out the models to disk
  gbm_path <- paste(wd, "/gbm_models", sep = "")
  gbm_model_files <- sapply(gbm_grid@model_ids, function(m) h2o.saveModel(h2o.getModel(m), path = gbm_path, force = TRUE))

  # print out, needs work
  cat(paste("gbm Models Saved To:\n", gbm_path, "\n\n"))
  gbm_grid


}