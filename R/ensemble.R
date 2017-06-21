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
  h2o.rm("ensemble")
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
        cat("\nReducing dimention of weak learner prediction matrix by ~",percent_reduce, "%\n")
        ensemble_train_pca <- h2o.prcomp(ensemble_train,
                                        transform = "STANDARDIZE",
                                        k = round(h2o.ncol(ensemble_train)*((100 - percent_reduce)/100)),
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
        #==========================================================================
        # this one is pre-set to be more cautious
        # run another grid search
        cat("Training Deep Learning Models\n")
        deeplearning_adaptive_rate = TRUE
        deeplearning_stopping_rounds = 10
        deeplearning_stopping_tolerance = 1e-5
        grid_id <- "ensemble"
        #==============================================
        dl_parameter_search <- list(rate= c(1e-9, 1e-8, 1e-7, 1e-6),
                                    rate_annealing = c(1e-12, 1e-9, 1e-6),
                                    momentum_start = c(0.8, 0.9),
                                    momentum_stable = c(0.95, 0.99),
                                    momentum_ramp = 1/seq(1e-12, 1e-9, 1e-6),
                                    score_duty_cycle = c(0.02, 0.05, 0.1),
                                    activation = c("RectifierWithDropout",
                                                   "TanhWithDropout",
                                                   "MaxoutWithDropout"),
                                    hidden = list(c(200,200,200),
                                                  c(512,512,512),
                                                  c(64, 64, 64),
                                                  c(round(length(ensemble_x) + length(ensemble_x)*1.2),
                                                    round(length(ensemble_x) + length(ensemble_x)*2),
                                                    round(length(ensemble_x) + length(ensemble_x)*.5))),

                                    input_dropout_ratio = c(0, 0.05, 0.1),
                                    hidden_dropout_ratios = list(c(0.1, 0.1, 0.1),
                                                                 c(0.2, 0.2, 0.2),
                                                                 c(0.5, 0.5, 0.5)),
                                    l1= c(1e-5, 1e-4, 1e-3),
                                    l2= c(1e-5, 1e-4, 1e-3),
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

        dl_search_criteria = list(strategy = "RandomDiscrete",
                                  max_runtime_secs = runtime_secs,
                                  stopping_rounds = deeplearning_stopping_rounds,
                                  stopping_tolerance = deeplearning_stopping_tolerance,
                                  seed = 1234) # needs to be changable

        # run the grid
        # needs be removed first for iterating within same session
        h2o.rm("dl")
        dl_random_grid <- h2o.grid(algorithm = "deeplearning",
                                   grid_id = grid_id,
                                   training_frame = ensemble_train,
                                   validation_frame = ensemble_valid,
                                   x = ensemble_x,
                                   y = ensemble_y,
                                   standardize = TRUE,
                                   epochs=1000, #needs to change
                                   overwrite_with_best_model = TRUE,
                                   adaptive_rate = deeplearning_adaptive_rate,
                                   hyper_params = hyper_params,
                                   search_criteria = dl_search_criteria,
                                   stopping_metric = eval_metric,
                                   seed = 1234) # needs to be changable

        dl_path <- paste(wd, "/", grid_id, "_models", sep = "")
        dl_model_files <- sapply(dl_random_grid@model_ids, function(m) h2o.saveModel(h2o.getModel(m), path = dl_path, force = TRUE))

        # print out alert
        cat(paste("Deep Learning Models Saved To:\n", dl_path, "\n\n"))

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
