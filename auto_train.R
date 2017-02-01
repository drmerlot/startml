### function to run all three ...
#========================================================
start.autotrain <- function(train,
                    y_name,
                    y_type,
                    wd = getwd()) {
  setwd(wd)
  start.rf(train = train,
               y_name = y_name,
               y_type = y_type)

  start.gbm(train = train,
               y_name = y_name,
               y_type = y_type)

  start.deeplearning(train = train,
                y_name = y_name,
                y_type = y_type)
}



















