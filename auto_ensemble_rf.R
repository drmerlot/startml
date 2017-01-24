## for random forest auto train . . 

wd <- "/Users/Andy/Desktop/numerai/1-11-17"
setwd(wd)
# set variables, these will turn into funtion inputs
train_file <- "/Users/Andy/Desktop/numerai/1-11-17/numerai_training_data.csv"
# start lifting!
# import train and test !!
df1 <- h2o.importFile(path = normalizePath(train_file))



autotrain.rf <- function(train, 
                          y_name,
                          y_type,
                          wd = getwd(), 
                          percent_train_holdout = 0.1,
                          rf_min_depth = 1, 
                          rf_max_depth = 7,
                          rf_runtime_secs = 20,
                          rf_stopping_rounds = 10,
                          rf_stopping_tolerance = 1e-5,
                          seeds = c(1234, 4321, 3245, 5432),
                          grid_strategy = "RandomDiscrete") {

  # break the data for holdout validation
  splits <- h2o.splitFrame(train, 1 - percent_train_holdout, seed=seeds[1])
  train  <- h2o.assign(splits[[1]], "train.hex") # 80%
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
  
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  
)

search_criteria = list(
  strategy = grid_strategy, 
  max_runtime_secs = rf_runtime_secs,
  stopping_rounds = rf_stopping_rounds,                
  stopping_tolerance = rf_stopping_tolerance,
  seed = seeds[3] 
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
cat(paste("Random Forest Models Saved To:\n", rf_path))
rf_grid

}


# test the function . .
rf <- autotrain.rf(train = df1, 
                   y_name = "target", 
                   y_type = "discrete")
  



