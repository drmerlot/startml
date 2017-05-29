#' get_frame
#'
#' get_frame Returns an r data frame having an id and prediction column from a model
#'
#' @param mlout startml mlblob object from startml function. No Default.
#' @param new_data_id Character object of length 1 identifying the name of id varialbe. No default.
#' @param output_models Character object of varying length identifying which model names to get prections from. No default.
#' @param csv Boolean, whether or not a csv file named "new_data_predictions_
#' @param wd Character object defining file path where dl_models folder will be created and deep learning models saved. Defaults to current working directory.
#' @return R data.frame object with an id column and a single column for each model's predcitions.
#' @export
get_frame <- function(mlout, new_data_id = id, output_models, csv = TRUE, wd) {
  h2o_list <- mlout@predict_newdata
  r_list <- lapply(h2o_list, FUN = as.data.frame)
  preds_all_models <- do.call(cbind, r_list)
  name_list <- unlist(lapply(mlout@models, FUN = get_model_id))
  names(preds_all_models) <- name_list
  new_data_id <- as.data.frame(test$Id)[,1]
  new_data_predictions <- data.frame(id = new_data_id,
                                     preds_all_models)
  id_column_name <- "Id" # change this to id argument
  prediction_column_name <- y
  print_out <- new_data_predictions[,c(1,which(names(new_data_predictions) %in% output_models))]
  names(print_out) <- c(id_column_name, prediction_column_name)
  if(csv == TRUE) {
    write.csv(print_out, paste(wd, "/new_data_predictions_", output_model, ".csv", sep = ""))
  }
  print_out
}
