#==========================================================
#predict with selected models from start_autotrain
start.predict <- function(test, best_models) {
  cat("Predicting on test set with glm models\n")
  predctions_glm <- lapply(best_models[[1]], h2o.predict, newdata = test)
  cat("Predicting on test set with deep learning models\n")
  predctions_dl <- lapply(best_models[[2]], h2o.predict, newdata = test)
  cat("Predicting on test set with random foreset models\n")
  predctions_rf <- lapply(best_models[[3]], h2o.predict, newdata = test)
  predictions <- list(predctions_glm, predctions_dl, predctions_rf)
  predictions
}


