# small function needed for plotting
even_lengths <- function(train_rmse) {
  max_length <- max(unlist(lapply(train_rmse, length)))
  train_hist <- lapply(train_rmse, paste_nas, longest = max_length)
  train_hist
}