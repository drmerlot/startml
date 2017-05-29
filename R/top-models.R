#' top_models
#'
#' Selects best n models by performance threshold
#'
#' @param sorted_models Data frame object indexing all models sorted by decreasing performance. No Default.
#' @param model_list List object of H2O frames containing h2o model objects.
#' No Default.
#' @param number_top_models Numeric object indicating number of top models to return. Defualt is 10. If number entered is greater than number of model, whole model list is returned.
#' @return List object same length as number_top_models containing top performing h2o models from model_list.
#' @export
top_models <- function(sorted_models, model_list, number_top_models = 10) {
  all_top_models <- model_list[sorted_models$model_list_num[1:number_top_models]]
  all_top_models
}
