# trying to set a class
setClass("ml", slots = c(models = "list",
                         train = "list",
                         valid = "list",
                         test = "list",
                         newdata = "list",
                         predict_train = "list",
                         predict_valid = "list",
                         predict_test = "list",
                         predict_newdata = "list",
                         index = "data.frame"
                         ))

