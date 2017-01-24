### function to run all three ...
auto.train <- function(train,
                    y_name,
                    y_type,
                    wd = getwd()) {
  setwd(wd)
  autotrain.rf(train = train,
               y_name = y_name,
               y_type = y_type)

  autotrain.gbm(train = train,
               y_name = y_name,
               y_type = y_type)

  autotrain.deeplearning(train = train,
                y_name = y_name,
                y_type = y_type)


}



















