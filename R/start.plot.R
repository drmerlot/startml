# Plot binomial models ==========================
# right now, uses model list,
# change this to ml object when object is defined
# this adds ggplot2 dependency making the future package
#   need gpl3 license.

# temporary place for extra functions ==================
get_hist <- function(x) {
  x@model$scoring_history$validation_logloss
}

get_ids <- function(x) {
  x@model$id
}


paste_nas <- function(x, longest) {
  x_na <- c(x, rep(NA, longest - length(x)))
  x_na
}
# =======================================

even_lengths <- function(train_logloss) {
  longest <- max(unlist(lapply(train_logloss, length)))
  train_hist <- lapply(train_logloss, paste_nas, longest = longest)
  train_hist
}

start.qplot <- function(mlout) {
  if(class(mlout)[1] == "mlblob") {
    if(class(mlout@models[[1]]) == "H2OBinomialModel") {
      stop("Does not yet support binomial model summary")
    } else if(class(mlout@models[[1]]) == "H2ORegressionModel") {
      train_logloss <- lapply(ml_out@models, get_hist)
      train_hist <- lapply(train_logloss, paste_nas, longest = longest)
      hist_df <- as.data.frame(do.call('cbind', train_hist))
      ids <- sapply(ml_out@models, get_ids)
      ids_split <- sapply(names(ids), strsplit, split = "/")
      ids_final <- sapply(ids_split, `[`, length(ids_split[[1]]))
      iter <- seq(0, longest - 1, by = 1)
      colnames(hist_df) <- ids_final
      hist_df$iteration <- iter
      hist_melted <- melt(hist_df, ncol(hist_df))
      p <- ggplot(hist_melted) +
        geom_line(aes(x = iteration, y = value, color = variable))
      plot(p)
    }
  } else {
    ggpot2::qplot(mlout)
  }
}