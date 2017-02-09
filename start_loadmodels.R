#=======================
# load saved h2o models
start.loadmodels <- function(path) {
  all_model_files <- list.files(path, full.names = TRUE)
  all_models  <- sapply(all_model_files, function(m) h2o.loadModel(m))
  all_models
}