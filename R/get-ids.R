#' get_ids
#'
#' Internal function used in plotting mlblob objects. Gets list of model ids from mlblob object.
#'
#' @param x model from mlblob. No Default.
#' @return Validation history of the model defined by input
#' @export
get_ids <- function(x) {
  x@model$id
}
