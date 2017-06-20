#' plot
#'
#' Masked from graphics. Summary plot of mlblob object.
#'
#' @param mlout mlblob object from output of startml function.
#' @return None. Plots graphic to device.
#' @export
plot <- function(mlout) { suppressWarnings(
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
      #============================================================
      # make the xy plot ======================
      for(i in 1:length(mlout@predict_test)) {
        names(mlout@predict_test[[i]]) <- ids_final[[i]]
      }
      xy_df <- do.call(h2o.cbind, mlout@predict_test)
      xy_df$labeled <- mlout@test[[1]][,y]
      if(mlout@ensemble_model != "no ensemble in this mlblob") {
        ensemble_test <- mlout@ensemble_test[[1]]
        xy_df$ensemble <- ensemble_test
        pred_melted <- melt(as.data.frame(xy_df), c(ncol(xy_df) - 1, ncol(xy_df)))
        # the order plot
        p_order <- ggplot(pred_melted[order(pred_melted$labeled),]) +
          geom_point(aes(x = seq(1, nrow(pred_melted)), y = value,
                         color = variable), alpha = 0.6) +
          geom_point(aes(x = seq(1, nrow(pred_melted)), y = labeled, shape = "labeled"),
                     color = "black", size = 1) +
          geom_point(aes(x = seq(1, nrow(pred_melted)), y = ensemble, shape = "ensemble"),
                     color = "dark red", size = 1) +
          scale_shape_manual(name = "", values = c("labeled" = 18, "ensemble" = 15),
                            labels = c("Ensemble", "Labeled")) +
          scale_color_discrete(guide=FALSE) +
          guides(shape = guide_legend(override.aes = list(color = c("dark red", "black"),
                                                          size = 1.2))) +
          ylab(y) +
          xlab(paste("Index: Ordered By Asending", y)) +
          ggtitle("Labels and Predictions on Test")
      } else {
        pred_melted <- melt(as.data.frame(xy_df), ncol(xy_df))
        # the order plot
        p_order <- ggplot(pred_melted[order(pred_melted$labeled),]) +
          geom_point(aes(x = seq(1, nrow(pred_melted)), y = value,
                         color = variable), alpha = 0.6) +
          geom_point(aes(x = seq(1, nrow(pred_melted)), y = labeled, fill = "black"),
                     size = .8) +
          scale_fill_manual(name = "", values = c("black" = "black"), labels = c("Labeled\nValues")) +
          scale_color_discrete(guide=FALSE) +
          ylab(y) +
          xlab(paste("Index: Ordered By Asending", y)) +
          ggtitle("Labels and Predictions on Test")
      }
      # currently replaced with order plot
      #p_xy <- ggplot(xy_melted) +
      #  geom_point(aes(x = labeled, y = value, color = variable), alpha = 0.5) +
      #  geom_point(aes(x = labeled, y = labeled), color = "black", alpha = 0.5) +
      #  guides(color = FALSE) +
      #  xlab(paste("Labeled",  y)) +
      #  ylab(paste("Predicted", y)) +
      #  ggtitle("Labels vs Predictions on Test")
      #============================================================
      #=============================================================
      # get a bar chart of performance
      # save performance metics
      metrics <- unlist(test_metric(mlout@predict_test, test = mlout@test[[1]], y = mlout@y, eval_metric = mlout@models[[1]]@allparameters$stopping_metric))
      performance <- data.frame(model = unlist(ids_final), test_performance = metrics)
      # if there is an ensemble
      if(mlout@ensemble_model != "no ensemble in this mlblob") {
        ensemble_metric <- unlist(test_metric(mlout@ensemble_test, test = mlout@test[[1]], y = mlout@y, eval_metric = mlout@models[[1]]@allparameters$stopping_metric))
        ensemble_performance <- data.frame(model = "ensemble", test_performance = ensemble_metric)
        p_performance <- ggplot() +
          geom_bar(data = ensemble_performance, aes(x = reorder(model, test_performance),
                   y = test_performance), fill = "dark red", stat = "identity") +
          geom_bar(data = performance, aes(x = reorder(model, test_performance),
                   y = test_performance, fill = model), stat = "identity") +
          geom_hline(aes(yintercept = summary(metrics)[3][[1]], color = "black")) +
          ylab(mlout@models[[1]]@allparameters$stopping_metric) +
          xlab("") +
          scale_color_manual(name = "", values = c("black" = "black"), labels = c("Median\nPerformance")) +
          scale_fill_discrete(guide=FALSE) +
          ggtitle("Model Performance on Test") +
          theme(axis.text.x=element_text(angle = -45, hjust = 0))
      }
      else {
        p_performance <- ggplot(performance) +
          geom_bar(aes(x = reorder(model, test_performance), y = test_performance, fill = model),
                 stat = "identity") +
          geom_hline(aes(yintercept = summary(metrics)[3][[1]], color = "black")) +
          ylab(mlout@models[[1]]@allparameters$stopping_metric) +
          xlab("") +
          scale_color_manual(name = "", values = c("black" = "black"), labels = c("Median\nPerformance")) +
          scale_fill_discrete(guide=FALSE) +
          ggtitle("Model Performance on Test") +
          theme(axis.text.x=element_text(angle = -45, hjust = 0))
      }
      #print out the grid
      grid.arrange(p_history, p_order, p_target, p_performance, ncol = 2, nrow = 2)
    }
  } else {
    graphics::plot(mlout)
  }
)}
