#' plot_dlayer

#' Visualize hidden layer activations in a deep learning model with dimentionality reduction and variable labels
#' @param model H2O model object containing labeled data for model training.
#' No Default.
#' @param vis_data H2O frame object containing data to caculate layer activations.
#' No Default.
#' @param layer Numeric object of length 1 identifying the which hidden layer to visualize.
#' No Defalut.
#' @param label Character object of length 1 identifying the column name of the variable in vis_data to label visulaizaion points with. No Default.
#' @param dimentions Numeric object set to 2 or 3, 2 returns ggplot figure, 3 returns plotly html page
#' @param max_points Numeric object setting maximum number of observations in visualization. A number of rows equal to max_points from vis_data are sampled with out replacement for the visulaization.
#' @param tsne_iter Numberic object sets the number of iterations in t-tailed stochastic nearest neighbors dimentionality reduction operation. Defaults to 1000.
#' @param wd Character object defining file path where html interactive graphic will be saved if dimentions = 3. Defaults to current working directory.
#' @return None
#' @export
plot_dlayer <- function(model,
                        vis_data,
                        layer,
                        label,
                        dimentions = 2,
                        max_points = 1000,
                        tsne_iter = 500,
                        wd = getwd()) {
  samp <- sample(x = 1:nrow(vis_data), size = max_points)
  samp <- samp[order(samp)]
  view_label <- vis_data[samp, label]
  names(view_label) <- label
  dat <- vis_data[samp,]
  cat("Getting hidden layer values from DL model\n")
  dlayer <- as.data.frame(h2o.deepfeatures(model, dat, layer = layer))
  if(dimentions == 2) {
    cat("Starting tSNE dimentionality reduction\n")
    dat_tsne <- tsne(X = dlayer,
                     k = dimentions,
                     initial_dims = dim(dlayer)[2],
                     max_iter = tsne_iter)
    vis_label <- as.data.frame(view_label)[,1]
    dat_plot <- data.frame(dat_tsne, vis_label)
    names(dat_plot) <- c("dl.hl.1", "dl.hl.2", label)
    p <- ggplot(dat_plot) +
      geom_point(aes(x = dl.hl.1, y = dl.hl.2, color = vis_label),
                 alpha = 0.7) +
      xlab("tSNE Dim 1") +
      ylab("tSNE Dim 2") +
      ggtitle("tSNE Dimentions of DL model Hidden Layer") +
      guides(color = guide_legend(title = label)) +
      theme_classic(base_size =  12)
    p
  } else if(dimentions == 3) {
    cat("Starting tSNE dimentionality reduction\n")
    dat_tsne <- tsne(X = dlayer,
                     k = dimentions,
                     initial_dims = dim(dlayer)[2],
                     max_iter = tsne_iter)
    vis_label <- as.data.frame(view_label)[,1]

    dat_plot <- data.frame(dat_tsne, vis_label)
    names(dat_plot) <- c("dl.hl.1", "dl.hl.2", "dl.hl.3", label)
    p <- plot_ly(dat_plot, x = ~dl.hl.1, y = ~dl.hl.2, z = ~dl.hl.3, color = ~vis_label) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = "tSNE Dim 1"),
                          yaxis = list(title = "tSNE Dim 2"),
                          zaxis = list(title = "tSNE Dim 3")))
  cat(paste("3D plot is saved as html page:\n", wd, "/", "dl-hidden-layer-plot.html\n",
            "open it there with your browser. Currently not working in firefox.\n", sep = ""))
  htmltools::save_html(as_widget(p), "dl-hidden-layer-plot.html")
  } else {
    stop("Dimentions must be set to 2 or 3")
  }
}
