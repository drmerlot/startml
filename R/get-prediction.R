#' get_prediction
#'
#' Retrieves newdata predictions from an exisiting mlblob object from a given model name
#'
#' @param model Character. Name of model including in mlblob object whos predictions will be extracted.
#' @param mlblob mlblob object from output of startml function. Defines the mlblob object from which the predictions will be extract. Must have preictions in newdata slot.
#' @return h2oFrame of predictions from model in named in input sourced from the prediction_newdata slot of the named mlblob object.
#' @export
get_prediction <- function(model, mlblob) {
  search_list <- names(mlblob@models)
  model_index <- which(grepl(model, search_list))
  prediction <- mlblob@predict_newdata[model_index]
  prediction
}
