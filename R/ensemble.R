#' ensemble
#'
#' @param mlout Inputs an mlblob object containing multiple models from the result of the startml function
#' @param algorithm Character. Algorithm used to create the ensemble. Can be one of the algorimths from startml, or "mean," "median," or "vote." Default is mean.
#' @param keep_features Boolean. When TRUE, orginial input features are mixed with weak learner predictions to create the new input data set for the ensemble. When FALSE, only weak learner esimates are used. Default is FALSE.
#' @param percent_reduce Numeric. Indicates the rough percentage to reduce input dementions by. Default is NULL.
#' @param reduce_method Character. Indicates the dimentional reduction method to use. Can be "PCA" or "auto_encoder." When "auto_encoder" algorithm must be deeplearning, this can greatly increase the length of time nessesary to run an ensemble, but may result in a more stable model. Default is NULL.
#' @param grid_search Boolean. Use a grid search to find optimal hyper parameter when algorithm is "gbm" randomForest" or "deepleaning." Default is FALSE, this can greatly increase the ensemble training time but may result in a superior parameter.
#' @param runtime_secs Numeric. Number of seconds to run grid search when grid_search is TRUE. Default is 60 seconds.
#' @param validation Character. Type of validation to be used in ensemble, can be "random_holdout" or "xval" for cross validation. Default is xval.
#' @return mlblob object including ensemble model, ensemble input data set, and predictions on train, valid, test, and new_data sets if available.
#' @export
ensemble <- function(mlout,
                     algorithm = "mean",
                     validation = "xval",
                     percent_reduce = NULL,
                     reduce_method = NULL,
                     keep_features = FALSE,
                     grid_search = FALSE,
                     runtime_secs = 60,
                     eval_metric = "AUTO") {
  ml_ensemble <- mlout
  if(algorithm == "mean") {
    weak_predictions <- h2o.cbind(mlout@predict_test)
    ensemble_test <- h2o.mean(weak_predictions, axis = 1, return_frame = TRUE)
    weak_predictions <- h2o.cbind(mlout@predict_newdata)
    ensemble_newdata <- h2o.mean(weak_predictions, axis = 1, return_frame = TRUE)
    ml_ensemble@ensemble_model[[1]] <- "Simple mean of weak learners"
    ml_ensemble@ensemble_test[[1]] <- ensemble_test
    ml_ensemble@ensemble_newdata[[1]] <- ensemble_newdata
  } else if(algorithm == "deeplearning") {
      ensemble_train <- h2o.cbind(mlout@predict_train)
      ensemble_valid <- h2o.cbind(mlout@predict_valid)
      ensemble_test <- h2o.cbind(mlout@predict_test)
      ensemble_newdata <- h2o.cbind(mlout@predict_newdata)
      if(!is.null(percent_reduce)) {
        ensemble_train_pca <- h2o.prcomp(ensemble_train,
                                        transform = "STANDARDIZE",
                                        k = round(length(mlout@x)*(1 - percent_reduce / 100)),
                                        impute_missing = TRUE)
        pca_inputs <- predict(ensemble_train_pca, ensemble_train, impute_missing = TRUE)
        pca_inputs[, mlout@y] <- mlout@train[[1]][, mlout@y]
        pca_train <- pca_inputs
        pca_inputs <- predict(ensemble_train_pca, ensemble_valid, impute_missing = TRUE)
        pca_inputs[, mlout@y] <- mlout@valid[[1]][, mlout@y]
        pca_valid <- pca_inputs
        pca_inputs <- predict(ensemble_train_pca, ensemble_test, impute_missing = TRUE)
        pca_inputs[, mlout@y] <- mlout@test[[1]][, mlout@y]
        pca_test <- pca_inputs
        pca_inputs <- predict(ensemble_train_pca, ensemble_newdata, impute_missing = TRUE)
        pca_newdata <- pca_inputs
      }
      if(keep_features == TRUE) {
        ensemble_train <- h2o.cbind(mlout@train, pca_train)
        ensemble_valid <- h2o.cbind(mlout@valid, pca_valid)
        ensemble_test <- h2o.cbind(mlout@test, pca_test)
        ensemble_newdata <- h2o.cbind(mlout@new_data, pca_newdata)
      }
      if(grid_search == TRUE){
        ensemble_y <- mlout@y
        ensemble_id <- mlout@label_id
        ensemble_x <- x <- setdiff(names(ensemble_train), y)
        ensemble_x <- ensemble_x[-which(ensemble_x == ensemble_id)]
        # run another grid search
        ens_grid <- dl_autogrid(train = ensemble_train,
                    valid = ensemble_valid,
                    y = ensemble_y,
                    x = ensemble_x,
                    eval_metric = eval_metric,
                    deeplearning_runtime_secs = runtime_secs,
                    grid_id = "ensemble")
        model_paths <- NULL
        model_paths <- c(model_paths, paste(wd, "/ensemble_models", sep = ""))
        all_models <- load_models(model_paths)
        #h2o.rm("ensemble") # might need somewhere
        top_models <- sort_models(all_models, eval_metric)
        top_model <- top_models(top_models, all_models, number_top_models = 1)[[1]]
        predict_test <- h2o.predict(top_model, ensemble_test)
        predict_newdata <- h2o.predict(top_model, ensemble_newdata)
      } else {
        stop("only supports grid search = TRUE for now")
      }

      ensemble_test <- predict_test
      ensemble_newdata <- predict_newdata
  }
  ml_ensemble@ensemble_model[[1]] <- top_model
  ml_ensemble@ensemble_test[[1]] <- ensemble_test
  ml_ensemble@ensemble_newdata[[1]] <- ensemble_newdata
  ml_ensemble
}
