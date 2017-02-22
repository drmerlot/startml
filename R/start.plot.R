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

# finding the x y for text labels in traning histories
hist_text <- function(id_final, hist_melted) {
  sub <- hist_melted[hist_melted$variable %in% id_final,]
  lab_x <- max(sub$iteration)
  lab_y <- sub$value[which(sub$iteration == lab_x)]
  col_x <- rep(lab_x, nrow(sub))
  col_y <- rep(lab_y, nrow(sub))
  sub$lab_x <- col_x
  sub$lab_y <- col_y
  sub$lab_x[which(sub$iteration != lab_x)] <- NA
  sub$lab_y[which(sub$iteration != lab_x)] <- NA
  sub
}

rename_predictions <- function(prediction) {
  names(prediction) <-
  }

# =======================================

even_lengths <- function(train_rmse) {
  max_length <- max(unlist(lapply(train_rmse, length)))
  train_hist <- lapply(train_rmse, paste_nas, longest = max_length)
  train_hist
}

start.qplot <- function(mlout) { suppressWarnings(
  if(class(mlout)[1] == "mlblob") {
    if(class(mlout@models[[1]]) == "H2OBinomialModel") {
      stop("Does not yet support binomial model summary")
    } else if(class(mlout@models[[1]]) == "H2ORegressionModel") {
      train_rmse <- lapply(mlout@models, get_hist)
      longest <- max(unlist(lapply(train_rmse, length)))
      train_hist <- even_lengths(train_rmse)
      hist_df <- as.data.frame(do.call('cbind', train_hist))
      ids <- sapply(mlout@models, get_ids)
      ids_split <- sapply(names(ids), strsplit, split = "/")
      ids_final <- sapply(ids_split, `[`, length(ids_split[[1]]))
      iter <- seq(0, longest - 1, by = 1)
      colnames(hist_df) <- ids_final
      hist_df$iteration <- iter
      hist_melted <- melt(hist_df, ncol(hist_df))
      hist_melted <- hist_melted[-which(is.na(hist_melted$value)),]
      ids_final <- as.list(ids_final)
      hist_lab <- lapply(ids_final, hist_text, hist_melted = hist_melted)
      hist_all <- do.call('rbind', hist_lab)
      p_history <- ggplot(hist_all) +
        geom_line(aes(x = iteration, y = value, color = variable),
                  alpha = 0.5, size = 1.2) +
        geom_text(aes(label = variable, x = lab_x, colour = variable,
                      y = lab_y, hjust = "inward"),
                  alpha = 1, check_overlap = TRUE, size = 3) +
        guides(color = FALSE) +
        ggtitle("Training History of Models on Valid") +
        ylab("RMSE") +
        xlab("Iterations")
      # make the histograms
      y = mlout@y
      all_target <- as.data.frame(mlout@labeled_data[[1]][,y])[,1]
      max_length <- length(all_target)
      train_target <- c(as.data.frame(mlout@train[[1]][,y])[,1],
                        rep(NA, max_length - nrow(mlout@train[[1]][,y])))
      valid_target <- c(as.data.frame(mlout@valid[[1]][,y])[,1],
                        rep(NA, max_length - nrow(mlout@valid[[1]][,y])))
      test_target <- c(as.data.frame(mlout@test[[1]][,y])[,1],
                       rep(NA, max_length - nrow(mlout@test[[1]][,y])))
      target_df <- data.frame(all = all_target,
                              train = train_target,
                              valid = valid_target,
                              test = test_target)

      # now melt
      target_melted <- melt(target_df)
      # now plot
      p_target <- ggplot(target_melted) +
        geom_histogram(aes(x = value, y = ..density..), bins = 20) +
        geom_vline(data = ddply(target_melted , "variable",
                                summarize, wavg = mean(na.omit(value))),
                   aes(xintercept=wavg, color = "green")) +
        geom_vline(data = ddply(target_melted , "variable", summarize,
                                wavg = median(na.omit(value))),
                   aes(xintercept=wavg, color = "orange")) +
        geom_density(aes(value, color = "blue"), alpha = 0.8) +
        facet_wrap(~variable) +
        scale_color_manual(name = '', values = c("green" = "green",
                                                 "orange" = "orange",
                                                 "blue" = "blue"),
                           labels = c("Kernel", 'Mean','Median')) +
        xlab(y) +
        ylab("Density") +
        ggtitle(paste(y, "in Labeled Data Splits")) +
        theme(axis.text.x=element_text(angle = -45, hjust = 0))
      # make the xy plot ======================
      for(i in 1:length(mlout@predict_test)) {
        names(mlout@predict_test[[i]]) <- ids_final[[i]]
        }
      xy_df <- do.call(h2o.cbind, mlout@predict_test)
      xy_df$labeled <- mlout@test[[1]][,y]
      xy_melted <- melt(as.data.frame(xy_df), ncol(xy_df))
      p_xy <- ggplot(xy_melted) +
        geom_point(aes(x = labeled, y = value, color = variable), alpha = 0.5) +
        geom_point(aes(x = labeled, y = labeled), color = "black", alpha = 0.5) +
        guides(color = FALSE) +
        xlab(paste("Labeled",  y)) +
        ylab(paste("Predicted", y)) +
        ggtitle("Labels vs Predictions on Test")
      # the order plot ======================
      pred_melted <- melt(as.data.frame(xy_df), ncol(xy_df))
      p_order <- ggplot(pred_melted[order(pred_melted$labeled),]) +
        geom_point(aes(x = seq(1, nrow(pred_melted)), y = value,
                       color = variable), alpha = 0.6) +
        geom_point(aes(x = seq(1, nrow(pred_melted)), y = labeled),
                   size = .8) +
        scale_color_discrete(guide=FALSE) +
        ylab(y) +
        xlab(paste("Index: Ordered By Asending", y)) +
        ggtitle("Labels and Predictions on Test")
      # Plot everything on the grid
      grid.arrange(p_history, p_order, p_target, p_xy, ncol = 2, nrow = 2)
    }
  } else {
    ggpot2::qplot(mlout)
  }
)}

