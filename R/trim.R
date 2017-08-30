#' temp trim

#'
#' @param mlout mlblob object from output of startml function.
#' @param eval_metric Character object defining evaluation metric for training. No default, use "logloss," "MSE," "RMSE," "MAE," "AUC," or "mean_per_class_error."
#' @param number_top_models Numeric object indicating number of top models to return. Defualt is null. If number entered is greater than number of model, whole model list is returned.
#' @param eval_threshold Numeric object defining the performance threshold models must meet to be used in prediction. Is minimum for maximization loss function (i.e., AUC) and maximum for minimization loss functions (logloss, MSE, etc). Default is NULL, returns models without performance consideration.
#' @param correlation_threshold Numeric object defining the maximum person correlation allowed in the group of resulting models. If two models show high correlation, the one with surperior performance will be kept and the other dropped. Value ranges from -1 to 1, default is NULL, returning models without correlation considered.
#' @param return_dataframe Boolean, if TRUE startml will attempt to return a data.frame of the resulting predictions for each new data row. This will only work if the resulting predictions from new data are small enough to be stored in the R workspace. Though, when working with smaller datasets, such as some competitions, this can be very convient. The same object is stored in the H2O space and can be accessed with the name set as the ouput of startml and manipulated with functions from the h2o R package. Default is FALSE.
#' @return mlblob object of selected models from original input.
#' Trim serves as an option to select only models which meet certain criteria after the startml function is run. Trim inputs the mlblob object from startml and outputs an mlblob object containing only the slected models.
#' @export

trim <- function(mlout,
                     eval_metric,
                     eval_threshold = NULL,
                     correlation_threshold = NULL,
                     number_top_models = NULL,
                     return_dataframe = FALSE) {

  # define the data sets, only works with shared holdout
  all_models <- mlout@models
  labeled_data <- mlout@labeled_data[[1]]
  train <- mlout@train[[1]]
  test <- mlout@test[[1]]
  valid <- mlout@valid[[1]]
  new_data <- mlout@new_data[[1]]
  y <- mlout@y
  x <- mlout@x
  #====================================================================
  # select the models
  if(!is.null(number_top_models)) {
    cat("\nChoosing Top Performing Models on Validation")
    sorted_models <- sort_models(all_models,
                                 eval_metric = eval_metric)
    selected_models <- top_models(sorted_models,
                                  all_models,
                                  number_top_models = number_top_models)
  } else {
    cat("\nChoosing Models on Test based on Performance and Correlation Thresholds\n")
    selected_models <- select_models(model_list = all_models,
                                     test = test,
                                     eval_metric = eval_metric,
                                     eval_threshold = eval_threshold,
                                     y = y,
                                     correlation_threshold = correlation_threshold)

  }
  ml_keep <- mlout
  ids <- sapply(mlout@models, get_ids)
  ids_split <- sapply(names(ids), strsplit, split = "/")
  ids_final <- sapply(ids_split, `[`, length(ids_split[[1]]))

  del_ids <- sapply(selected_models, get_ids)
  del_ids_split <- sapply(names(del_ids), strsplit, split = "/")
  del_ids_final <- sapply(del_ids_split, `[`, length(del_ids_split[[1]]))

  keep <- which(ids_final %in% del_ids_final)
  # keep the model and its results
  ml_keep@models <- ml_keep@models[keep]
  ml_keep@predict_train <- ml_keep@predict_train[keep]
  ml_keep@predict_valid <- ml_keep@predict_valid[keep]
  ml_keep@predict_test <- ml_keep@predict_test[keep]
  ml_keep@predict_newdata <- ml_keep@predict_newdata[keep]
  ml_keep



}
