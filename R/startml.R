#' startml
#'
#' startml is designed to run automatic hyperparameter searches for deep leaning
#' gradient boosted machine, and random forest models. It selects best models, and combines
#' or ensembles them in hopes making good predictions from an ensemble or highly skilled single
#' model using just one function call. Machine learning algorithms are provided by h2o and
#' run on the h2o JVM platform outside of the R workspace. Thus, much of the functionalies in startml are
#' scalable. Currently, startml only supports regression and binary classification.
#'
#' @param labeled_data H2O frame object containing labeled data for model training.
#' No Default.
#' @param newdata H2O frame object containing unlabeled data for model predictions.
#' No Default.
#' @param y Character object of length 1 identifying the column name of the target variable. No Default.
#' @param y_type Character object of length 1 identifying the type of data the target variable is. Can be "continuous" or "discrete." Coninuous automatically creates regression models, and discrete automatically creates binomial models. Currently, startml only supports regression and binary classification.
#' @param x Character object of length 1 or more identifying the column name(s) of the input variables. Default NULL, uses all remaining variables in labeled_data as inputs. Newdata must contian all of these input column names.
#' @param label_id Character object of length 1 identifying the name of the column of observation IDs in labeled_data. If used, must match column of same name in newdata. startml will ignore this column as an input, but include it as an ID column in prediction outputs.
#' @param algorithms Character object of length 3, 2, or 1, specifying which alrogrithms to automatically train. The autotrain function will run a separate grid search for each algorimth type. Choices are: "deeplearning", "randomForest", and "gbm" following the naming convention in H2O version 3. Defaults to c("deeplearning", "randomForest", "gbm").
#' @param eval_metric Character object defining evaluation metric for training. Defualt is "AUTO" and uses built-in H2O automatic choice for target data type.
#' @param validation_type Defines validation type for training models.  Defaults to "shared_holdout" indicating all model built with all algorithms share the same validation set. Currently, this is the only option in autotrain. Planned types include "random_holdout" where each model will get a unique randomized sample of labeled data for validation, and "xval" in which the cross validation functionality in H2O will be implemented in every model.
#' @param percent_valid_holdout Numeric object of value 0 to 100. Sets the percent of the labeled data that will be used for holdout validation. Default is 10. Is ignored if validation_type = "xval." Currently startml only supports "shared_holdout" validation.
#' @param percent_test_holdout  Numeric object of value 0 to 100. Sets the percent of the labeled data that will be used for test holdout for model selection. Default is 10.
#' @param runtime_secs Character Object which sets the length of time each grid search will run. Defaults to 20, thus the default runtime is 20 sec * (length of algorimths) = 1 minute.
#' @param split_seed Random seed for splitting labeled data into train, validation, and test components. Currently, startml only supports random sampling splits, this argument sets the random seed for these splits, making the data set separation process reproducible. Since this is a "naive" random split, labeled data should be shuffled before hand.
#' @param wd Character object defining file path where resulting modeling will be saved. Defualts to current working directory.
#' @param trim Boolean. When TRUE, output is trimmed with eval_threshold, correlation_threshold, or number_top_models. When FALSE, all models are returned. Default FALSE.
#' @param number_top_models Numeric object indicating number of top models to return. Defualt is 10. If number entered is greater than number of model, whole model list is returned.
#' @param eval_threshold Numeric object defining the performance threshold models must meet to be used in prediction. Is minimum for maximization loss function (i.e., AUC) and maximum for minimization loss functions (logloss, MSE, etc). Default is NULL, returns models without performance consideration.
#' @param correlation_threshold Numeric object defining the maximum person correlation allowed in the group of resulting models. If two models show high correlation, the one with surperior performance will be kept and the other dropped. Value ranges from -1 to 1, default is NULL, returning models without correlation considered.
#' @param return_dataframe Boolean, if TRUE startml will attempt to return a data.frame of the resulting predictions for each new data row. This will only work if the resulting predictions from new data are small enough to be stored in the R workspace. Though, when working with smaller datasets, such as some competitions, this can be very convient. The same object is stored in the H2O space and can be accessed with the name set as the ouput of startml and manipulated with functions from the h2o R package. Default is FALSE.
#' @return Object of class mlblob using S4 type. mlblob objects contain all selected models, their predictions on train, validation, test, and new data, and can be plotted using plot() showing a summary of the model group.
#' Slots are:
#' models, a list of h2o model objects
#' labeled_data an h2o frame object equivalent to the input label_data input object.
# 'train = A list of h2o frame objects contianing the train component for each model from the labeled data split.
# 'valid = A list of h2o frame objects contianing the validation component for each model from the labeled data split if cross validation is not used.
# 'test = A list of h2o frame objects contianing the test component for each model from the labeled data split.
# 'new_data = an h2o frame object equivalent to the input new_data input object.
# 'predict_train = A list of h2o frame objects contianing all model predictions on the trainng data.
# 'predict_valid = A list of h2o frame objects contianing all model predictions on the validation data.
# 'predict_test = A list of h2o frame objects contianing all model predictions on the test data.
# 'predict_newdata = A list of h2o frame objects contianing all model predictions on the new, unlabeled data.
# 'index = A data.frame object containing summary information of the mlblob object.
# 'y = A character object containing the name of the target variable column in labeled data.
# 'x = A character object containing the names of all input varialbes to be used in model building. Both labeled data and new data must each contain all of the column names specificed in x. Default is NULL and uses all variables except y and any column name specified in label_id. Do not train models with an ID column as an input. Either remove it, or specifiy it in label_id.
#' @export
startml <-  function(labeled_data,
                      newdata,
                      y,
                      x = NULL,
                      label_id = NULL,
                      y_type,
                      algorithms = c("deeplearning", "randomForest", "gbm"),
                      eval_metric = "AUTO",
                      validation_type = "shared_holdout", # add RandomHoldout and cv
                      percent_valid_holdout = 10,
                      percent_test_holdout = 10,
                      runtime_secs = 10,
                      split_seed = NULL,
                      trim = FALSE,
                      number_top_models = NULL,
                      eval_threshold = NULL,
                      correlation_threshold = 0,
                      return_dataframe = FALSE,
                      wd = getwd()) {

  if(validation_type == "shared_holdout" && is.null(split_seed)) {
    stop("Set 'split_seed' to any real number for common random sampling when validation = SharedHoldout")
  }

  # This needs to be replaced ==========================================
  # only works with shared holdout.
  # need condition for other holdout.
  if(validation_type == "shared_holdout" | validation_type == "random_holdout") {
    splits <- h2o.splitFrame(labeled_data,
                             c((1 - ((percent_valid_holdout/100) + (percent_test_holdout/100))),
                               (percent_test_holdout/100)),
                               seed = split_seed)
    train  <- h2o.assign(splits[[1]], "train.hex")
    valid  <- h2o.assign(splits[[2]], "valid.hex")
    test  <- h2o.assign(splits[[3]], "test.hex")
  } else if(validation_type == "xval") {
    splits <- h2o.splitFrame(train, 1 - (percent_test_holdout/100), seed = split_seed)
    train  <- h2o.assign(splits[[1]], "train.hex")
    test  <- h2o.assign(splits[[2]], "test.hex")
  } else {
    stop("Choose 'shared_holdout', 'random_holdout', or 'xval' for validation_type")
  }

  # define x as all others if not specified
  if(is.null(x)) {
    x <- setdiff(names(labeled_data), y)
  }

  if(!is.null(label_id)) {
    if(sum(x %in% label_id) > 0) {
      x <- x[-which(x == label_id)]
    } else {
      x <- x
      }
  }

  #===============================================
  # commented out for now, works when startml
  #  is stand alone functin, does not as
  #  part of package. Potential solves: something
  #  to do with methods package or versions of
  #  dependencies
  # set variable type for proper auto options
  #if(y_type == "discrete") {
  #  train[,y] <- as.factor(train[,y])
  #  valid[,y] <- as.factor(valid[,y])
  #  test[,y] <- as.factor(test[,y])
  #} else {
  #  train[,y] <- as.numeric(train[,y])
  #  valid[,y] <- as.numeric(valid[,y])
  #  test[,y] <- as.numeric(test[,y])
  #}
  #===============================================

  #==============================================
  # other fix is considering removing y_type argument
  #  in favor of doing this during data prep outside
  #  startml.
  #==============================================

  all_models <- autotrain(train = train,
                                valid = valid,
                                y = y,
                                x = x,
                                algorithms = algorithms,
                                eval_metric = eval_metric,
                                runtime_secs = runtime_secs,
                                wd = wd)
  # ===================================================================
  if(trim == TRUE) {
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
  } else {
    selected_models <- all_models
  }
  #=================================================================
  # predict with the models or selected models.
  cat("\nSaving Train Predictions with Selected Models\n")
  train_predictions <- predict_blob(test = train, selected_models)
  cat("\nSaving Valid Predictions with Selected Models\n")
  valid_predictions <- predict_blob(test = valid, selected_models)
  cat("\nSaving Test Predictions with Selected Models\n")
  test_predictions <- predict_blob(test = test, selected_models)
  cat("\nPredicting on New Data with Selected Models\n")
  newdata_predictions <- predict_blob(test = new_data, selected_models)

  # needs work.
  # make the index dataframe, trivially all 1s for shared holout
  index = data.frame(beta = "Not finished yet")

  # =================================================
  if(return_dataframe == FALSE) {
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
                  ensemble_model = list("no ensemble in this mlblob"),
                  ensemble_train = list("no ensemble in this mlblob"),
                  ensemble_valid = list("no ensemble in this mlblob"),
                  ensemble_test = list("no ensemble in this mlblob"),
                  ensemble_newdata = list("no ensemble in this mlblob"),
                  performance = index,
                  y = y,
                  x = x,
                  output = data.frame(mlblob.output = "No R object Returned, set return_dataframe to TRUE"))
  } else {
    warning("Returning R object in currently in the works")
  }
  mlout
}
