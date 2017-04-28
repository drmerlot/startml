#===================================================================
# more controls needed
start.ml <-  function(labeled_data, 
                      new_data,
                      y, 
                      x = NULL,
                      y_type,
                      algorithms = c("deeplearning", "randomForest", "gbm"),
                      eval_metric = "AUTO",
                      validation_type = "SharedHoldout", # add RandomHoldout and cv
                      percent_valid_holdout = 10,
                      percent_test_holdout = 10,
                      runtime_secs = 10,
                      split_seed = NULL,
                      number_top_models = NULL,
                      eval_threshold = 0.7,
                      correlation_threshold = 0.6,
                      return_dataframe = FALSE) {
  
  if(validation_type == "SharedHoldout" && is.null(split_seed)) {
    stop("Set 'split_seed' to any real number for common random sampling when validation = SharedHoldout")
  }

  # This needs to be replaced ==========================================
  # only works with shared holdout.
  # need condition for other holdout.
  if(validation_type == "SharedHoldout" | validation_type == "RandomHoldout") {
    splits <- h2o.splitFrame(labeled_data,
                             c((1 - ((percent_valid_holdout/100) + (percent_test_holdout/100))), 
                               (percent_test_holdout/100)), 
                               seed = split_seed)
    train  <- h2o.assign(splits[[1]], "train.hex")
    valid  <- h2o.assign(splits[[2]], "valid.hex")
    test  <- h2o.assign(splits[[3]], "test.hex")
  } else {
    splits <- h2o.splitFrame(train, 1 - (percent_test_holdout/100), seed = split_seed)
    train  <- h2o.assign(splits[[1]], "train.hex")
    test  <- h2o.assign(splits[[2]], "test.hex")
  }

  # define x as all others if not specified
  if(is.null(x)) {
    x <- setdiff(names(labeled_data), y)
  }

  # set variable type for proper auto options
  if(y_type == "discrete") {
    train[,y] <- as.factor(train[,y])
    valid[,y] <- as.factor(valid[,y])
    test[,y] <- as.factor(test[,y])
  } else {
    train[,y] <- as.numeric(train[,y])
    valid[,y] <- as.numeric(valid[,y])
    test[,y] <- as.numeric(test[,y])
  }
  
  all_models <- start.autotrain(train = train,
                                valid = valid, 
                                y = y,
                                x = x, 
                                algorithms = algorithms,
                                eval_metric = eval_metric,
                                runtime_secs = runtime_secs)
  # ===================================================================

  if(!is.null(number_top_models)) {
    cat("\nChoosing Top Performing Models on Validation")
    sorted_models <- start.sortmodels(all_models,
                                      eval_metric = eval_metric)
    selected_models <- start.topmodels(sorted_models,
                                       all_models,
                                       number_top_models = number_top_models)
  } else {
    cat("\nChoosing Models on Test based on Performance and Correlation Thresholds\n")
    selected_models <- start.selectmodels(model_list = all_models,
                                          test = test,
                                          eval_metric = eval_metric,
                                          eval_threshold = eval_threshold,
                                          y_name = y_name,
                                          correlation_threshold = correlation_threshold)
  }
    cat("\nSaving Train Predictions with Selected Models\n")
    train_predictions <- start.predict(test = train, selected_models)
    cat("\nSaving Valid Predictions with Selected Models\n")
    valid_predictions <- start.predict(test = valid, selected_models)
    cat("\nSaving Test Predictions with Selected Models\n")
    test_predictions <- start.predict(test = test, selected_models)
    cat("\nPredicting on New Data with Selected Models\n")
    newdata_predictions <- start.predict(test = new_data, selected_models)
  
  if(return_dataframe == FALSE) {
    # needs work.
    # make the index dataframe, trivially all 1s for shared holout
    index = data.frame(model_num = seq(1, length(selected_models)),
                       train_id = rep(1, length(selected_models)),
                       valid_id = rep(1, length(selected_models)),
                       test_id = rep(1, length(selected_models)))

    # build the output object of new class mlblob
    mlout <- new("mlblob",
                 models = selected_models,
                 labeled_data = list(labeled_data),
                 train = list(train),
                 valid = list(valid),
                 test = list(test),
                 new_data = list(new_data),
                 predict_train = train_predictions,
                 predict_valid = valid_predictions,
                 predict_test = test_predictions,
                 predict_newdata = newdata_predictions,
                 index = index,
                 y = y,
                 x = x)
    # =================================================
    mlout
  } else {
    stop("start.ml does not currently support auto conversion to standard r object")
  }
}