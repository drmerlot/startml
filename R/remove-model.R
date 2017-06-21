#' remove
#'
#' Removes a specific model from an mlblob object. Useful when trim function leaves an unwanted model in the mlblob.
#' @param mlout mlblob object from startml
#' @param model_id Character. Model id of unwanted model as seen in the result of calling plot on the mlblob object.
#' @return mlblob object identical to input except for removal of unwanted model and its predictions.
#' @export

remove_model <- function(mlout, model_id) {
  ml_delete <- mlout
  ids <- sapply(mlout@models, get_ids)
  ids_split <- sapply(names(ids), strsplit, split = "/")
  ids_final <- sapply(ids_split, `[`, length(ids_split[[1]]))
  delete <- which(ids_final == model_id)
  # delete the model and its results
  ml_delete@models <- ml_delete@models[-delete]
  ml_delete@predict_train <- ml_delete@predict_train[-delete]
  ml_delete@predict_valid <- ml_delete@predict_valid[-delete]
  ml_delete@predict_test <- ml_delete@predict_test[-delete]
  ml_delete@predict_newdata <- ml_delete@predict_newdata[-delete]
  ml_delete
}
