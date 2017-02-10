#============================================================
## the prepare function

# load libraries
library(h2o)
#library(cvAUC)
#library(Metrics)
#library(h2oEnsemble)


## start decent size (more ram than needed for this)
h2o.shutdown(prompt = FALSE)
h2o.init(nthreads=2, max_mem_size="6G")
h2o.removeAll()

load_files <- as.list(list.files(path = "~/Desktop/auto/start.ml/",
                                 pattern = "start*", full.names = TRUE))
load_files <- load_files[-which(load_files %in% "/Users/grad/Desktop/auto/start.ml//start_setup.R")]

getfiles <- lapply(load_files, source)

# extra
wd <- "~/Desktop/auto"
setwd(wd)

train_file <- "~/Desktop/auto/numerai_training_data.csv"
test_file <- "~/Desktop/auto/numerai_tournament_data.csv"
y_name <- "target"
y_type <- "discrete" # or "continous"

#============================================================
# the load data function.
start.loaddata <- function(train_file) {
  df1 <- h2o.importFile(path = normalizePath(train_file))
  df1
}

# test
df1 <- start.loaddata(train_file)
test <- start.loaddata(test_file)


# run the ml file on binary classification.
mlout <- start.ml(train = df1,
                  test = test,
                  y_name = "target",
                  y_type = "discrete",
                  eval_metric = "logloss",
                  validation_type = "SharedHoldout",
                  split_seed = 1234
                  )

# for testing
# run the ml file on binary classification.
models <- start.autotrain(train = df1,
                  y_name = "target",
                  y_type = "discrete",
                  eval_metric = "logloss",
                  validation_type = "SharedHoldout",
                  split_seed = 1234
)


#============================================================
# the load data function.
# now try a regression ......
# test on kaggle housing prices ...
h2o.removeAll()

load_files <- as.list(list.files(path = "~/Desktop/auto/start.ml/",
                                 pattern = "start*", full.names = TRUE))
load_files <- load_files[-which(load_files %in% "/Users/grad/Desktop/auto/start.ml//start_setup.R")]

getfiles <- lapply(load_files, source)

# extra
wd <- "~/Desktop/auto"
setwd(wd)

train_file <- "~/Desktop/auto/train.csv"
test_file <- "~/Desktop/auto/test.csv"


start.loaddata <- function(train_file) {
  df1 <- h2o.importFile(path = normalizePath(train_file))
  df1
}

# test
df1 <- start.loaddata(train_file)
test <- start.loaddata(test_file)


# test on regression data
# run the ml file on binary classification.
mlout <- start.ml(train = df1,
                  test = test,
                  y_name = "SalePrice",
                  y_type = "continuous",
                  eval_metric = "RMSE",
                  number_top_models = 5,
                  validation_type = "SharedHoldout",
                  split_seed = 1234
)

# for now to get different metrics working
model_list <- start.autotrain(train = df1,
                  #test = test,
                  y_name = "SalePrice",
                  y_type = "continuous",
                  eval_metric = "RMSLE",
                  validation_type = "SharedHoldout",
                  split_seed = 1234
)

eval_metric = "RMSE"

metric <- start.validmetric(model_list, eval_metric = "RMSE")

sorted_models <- start.sortmodels(model_list, eval_metric = eval_metric)

selected_models <- start.selectmodels(sorted_models, all_models, x=1)


predictions <- start.predict(test = test, selected_models)
















