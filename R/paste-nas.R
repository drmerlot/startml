#' paste-nas
#'
#' Internal function used in plotting mlblob objects. Handles nas.
#'
#' @param x Character or Numeric object No Defalut.
#' @param longest Length of character or numeric object to extend x to with nas. No Default.
#' @return Validation history of the model defined by input
#' @export
paste_nas <- function(x, longest) {
  x_na <- c(x, rep(NA, longest - length(x)))
  x_na
}
