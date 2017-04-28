# trying to set a class
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
                         index = "data.frame",
                         y = "character",
                         x = "character"))


# doesn't work  ==============================
# setGeneric("fill_models", function (x) standardGeneric("fill_models"))
# setMethod("fill_models", c(x="ml"), function (x) x@models)
#
#
# setGeneric("fill_models<-", function (x, ...) standardGeneric("fill_models<-"))
# setMethod("fill_models<-", c(x="ml"), function (x, value) {
#   x@models <- value
#   #if (validObject(x)) return(x)
# })

# fill_models(out) <- model_list

# end doesn't work ==============================


