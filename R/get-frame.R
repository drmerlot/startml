#=======================================
# get an r data frame out of the model by model name
# return data.frame given id and model
get_frame <- function(mlout, new_data_id = id, output_models, wd) {
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
  write.csv(print_out, paste(wd, "/new_data_predictions_", output_model, ".csv", sep = ""))
}