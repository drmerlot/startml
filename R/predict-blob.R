#==========================================================
#predict with selected models from start_autotrain
predict_blob <- function(test, selected_models) {
  #cat("Predicting on New Data With Selected Models\n")
  predictions <- lapply(selected_models, h2o.predict, newdata = test)
  predictions
}


