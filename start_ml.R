#===================================================================
# more controls needed
start.ml <- function(train, new_data,
                     y_name, y_type,
                     algorithms = c("deeplearning", "randomForest", "gbm"),
                     eval_metric = "AUC",
                     validation_type = "SharedHoldout", # add RandomHoldout and cv
                     split_seed = NULL,
                     top_models = NULL,
                     eval_threshold = 0.7,
                     correlation_threshold = 0.6,
                     return_dataframe = FALSE) {
  if(validation_type == "SharedHoldout" && is.null(split_seed)) {
    stop("Set 'split_seed' to any real number for common random sampling when validation = SharedHoldout")
  }

  all_models <- start.autotrain(train = train,
                                    y_name = y_name,
                                    y_type = y_type,
                                    algorithms = algorithms,
                                    eval_metric = eval_metric,
                                    split_seed = split_seed)
  # This needs to be replaced ==========================================
  # only works with shared holdout.
  # need condition for other holdout.
  # fix by creating an ml object class that deals with this.
  if(validation_type == "SharedHoldout" | validation_type == "RandomHoldout") {
    splits <- h2o.splitFrame(train,
                             c((1 - ((percent_valid_holdout/100) + (percent_test_holdout/100))), (percent_test_holdout/100)), seed = split_seed)
    train  <- h2o.assign(splits[[1]], "train.hex")
    valid  <- h2o.assign(splits[[2]], "valid.hex")
    test  <- h2o.assign(splits[[3]], "test.hex")
  } else {
    splits <- h2o.splitFrame(train, 1 - (percent_test_holdout/100), seed = split_seed)
    train  <- h2o.assign(splits[[1]], "train.hex")
    test  <- h2o.assign(splits[[2]], "test.hex")
  }
  # ===================================================================

  if(!is.null(top_models)) {
    sorted_models <- start.sortmodels(all_models,
                                      eval_metric = eval_metric)
    selected_models <- start.topmodels(sorted_models,
                                       all_models,
                                       number_top_models = number_top_models)
  } else {
  selected_models <- start.selectmodels(model_list = all_models,
                                        test = test, #!!! needs to be 'test' from split not new data
                                        eval_metric = eval_metric,
                                        eval_threshold = eval_threshold,
                                        y_name = y_name,
                                        correlation_threshold = correlation_threshold)
  }
  predictions <- start.predict(test = new_data, selected_models) # !!! this houses the actual test data
  if(return_dataframe == FALSE) {
    predictions
  } else {
    stop("start.ml does not currently support auto conversion to standard r object")
  }
  # =================================================
  # needs work.
  # make the index dataframe, trivially all 1s for shared holout
  index = data.frame(model_num = seq(1, length(model_list)),
                     train_id = rep(1, length(model_list)),
                     valid_id = rep(1, length(model_list)),
                     test_id = rep(1, length(model_list))
  )

  # build the output object of new class mlstack
  mlout <- new("mlstack",
             models = model_list,
             train = list(train),
             valid = list(valid),
             test = list(test),
             newdata = list(new_data),
             index = index)
  # =================================================
  mlout
}