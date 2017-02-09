#===================================================================
# many more controls needed
start.ml <- function(train, test,
                     y_name, y_type,
                     algorithms = c("deeplearning", "randomForest", "gbm"),
                     eval_metric = "AUTO",
                     validation_type = "SharedHoldout", # add RandomHoldout and cv
                     split_seed = NULL,
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

  sorted_models <- start.sortmodels(all_models, x = 1, eval_metric = eval_metric)
  selected_models <- start.selectmodels(sorted_models, all_models, x=1)
  predictions <- start.predict(test = test, selected_models)

  if(return_dataframe == FALSE) {
    predictions
  } else {
    stop("start.ml does not currently support auto conversion to standard r object")
    }
}