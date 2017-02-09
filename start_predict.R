#==========================================================
#predict with selected models from start_autotrain
start.predict <- function(test, selected_models) {
  cat("Predicting on New Data With Selected Models\n")
  predctions_dl <- lapply(selected_models, h2o.predict, newdata = test)
  predictions
}


