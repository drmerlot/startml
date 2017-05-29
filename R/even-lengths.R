#' even_lengths
#'
#' Small, internal function needed for plotting. Not intended for direct use.
#'
#' @param train_rmse List object contiaing model performance numbers. No Default.
#' @export
even_lengths <- function(train_rmse) {
  max_length <- max(unlist(lapply(train_rmse, length)))
  train_hist <- lapply(train_rmse, paste_nas, longest = max_length)
  train_hist
}
