# Plot binomial models ==========================
# right now, uses model list,
# change this to ml object when object is defined
# this adds ggplot2 dependency making the future package
#   need gpl3 license.

# temporary place for extra functions ==================
get_hist <- function(x) {
  x@model$scoring_history$validation_rmse
}

get_ids <- function(x) {
  x@model$id
}


paste_nas <- function(x, longest) {
  x_na <- c(x, rep(NA, longest - length(x)))
  x_na
}
# =======================================

even_lengths <- function(train_rmse) {
  max_length <- max(unlist(lapply(train_rmse, length)))
  train_hist <- lapply(train_rmse, paste_nas, longest = max_length)
  train_hist
}

start.qplot <- function(mlout) {
  if(class(mlout)[1] == "mlblob") {
    if(class(mlout@models[[1]]) == "H2OBinomialModel") {
      stop("Does not yet support binomial model summary")
    } else if(class(mlout@models[[1]]) == "H2ORegressionModel") {
      train_rmse <- lapply(mlout@models, get_hist)
      train_hist <- even_lengths(train_rmse)
      hist_df <- as.data.frame(do.call('cbind', train_hist))
      ids <- sapply(mlout@models, get_ids)
      ids_split <- sapply(names(ids), strsplit, split = "/")
      ids_final <- sapply(ids_split, `[`, length(ids_split[[1]]))
      iter <- seq(0, longest - 1, by = 1)
      colnames(hist_df) <- ids_final
      hist_df$iteration <- iter
      hist_melted <- melt(hist_df, ncol(hist_df))
      p <- ggplot(hist_melted) +
        geom_line(aes(x = iteration, y = value, color = variable),
                  alpha = 0.7, size = 1.2)
      plot(p)
    }
  } else {
    ggpot2::qplot(mlout)
  }
}