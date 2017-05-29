#' load_models
#'
#' Loads saved text file h2o models into R workspace and h2o instance
#' @param path Character object of file path to folder containing one or more saved h2o model files.
#' @return List object of h2o model objects
#' @export
load_models <- function(path) {
  all_model_files <- list.files(path, full.names = TRUE)
  all_models  <- sapply(all_model_files, function(m) h2o.loadModel(m))
  all_models
}
