# small function needed for qplot 
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