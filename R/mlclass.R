#' mlblob
#' S4 object designed to keep track of all models and data
# needs work
#' @export
setClass("mlblob", slots = c(models = "list",
                             labeled_data = "list",
                             train = "list",
                             valid = "list",
                             test = "list",
                             new_data = "list",
                             predict_train = "list",
                             predict_valid = "list",
                             predict_test = "list",
                             predict_newdata = "list",
                             ensemble_model = "list",
                             ensemble_train = "list",
                             ensemble_valid = "list",
                             ensemble_test = "list",
                             ensemble_newdata = "list",
                             performance = "data.frame",
                             y = "character",
                             x = "character",
                             label_id = "character",
                             output = "data.frame"))
