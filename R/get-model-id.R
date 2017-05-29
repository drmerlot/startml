#' get_model_id
#'
#' Internal function used in plotting mlblob objects. Gets model id from h2o model object
#' @param x h2o model. No Default.
#' @return Character object Model id.
#' @export
get_model_id <- function(x) {
  model_id <- x@model_id
  model_id
}
