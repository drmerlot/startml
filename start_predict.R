#============= predict with the random models.


start.predict <- function(test, best_models) {
  predctions_glm <- lapply(best_models[[1]], h2o.predict, newdata = test)
  predctions_deeplearning <- lapply(best_models[[2]], h2o.predict, newdata = test)
  predctions_rf <- lapply(best_models[[3]], h2o.predict, newdata = test)
  predictions <- list(predctions_glm, predctions_deeplearning, predctions_rf)
}

te <- start.predict(test, best_models)
te <- h2o.predict(best_models, test)
