#==============================================
# visualize hidden layer in a deep learning model
plot_dlayer <- function(model, 
                        vis_data, 
                        layer, 
                        label, 
                        dimentions = 2, 
                        max_points = 1000,
                        tsne_iter = 1000) {
  samp <- sample(x = 1:nrow(vis_data), size = max_points)
  samp <- samp[order(samp)]
  view_label <- vis_data[samp, label]
  names(view_label) <- label
  dat <- vis_data[samp,]
  dlayer <- as.data.frame(h2o.deepfeatures(model, dat, layer = layer))
  dat_tsne <- tsne(X = dlayer, 
                   k = dimentions, 
                   initial_dims = dim(dlayer)[2], 
                   max_iter = tsne_iter)
  vis_label <- as.data.frame(view_label)[,1]
  dat_plot <- data.frame(dat_tsne, vis_label)
  if(dimentions == 2) {
    names(dat_plot) <- c("dl.hl.1", "dl.hl.2")
    p <- ggplot(dat_plot) + 
      geom_point(aes(x = dl.hl.1, y = dl.hl.2, color = vis_label)) +
      xlab("DL Hidden Layer Low Dim 1") +
      ylab("DL Hidden Layer Low Dim 2") + 
      ggtitle("tSNE Dimentions of DL model Hidden Layer") + 
      guides(color = guide_legend(title = label)) +
      theme_dark(base_size =  12)
  } else if(dimentions == 3) {
    stop("3D plot in the works. Requires Plotly")
  } else { 
    stop("Dimentions must be set to 2 or 3")  
  }
  p
}