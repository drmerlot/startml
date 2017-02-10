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

load_files <- as.list(list.files(path = wd <- "C:/Users/Andy/Desktop/auto/start.ml/",
                                 pattern = "start*", full.names = TRUE))
load_files <- load_files[-which(load_files %in% "C:/Users/Andy/Desktop/auto/start.ml/start_setup.R")]

getfiles <- lapply(load_files, source)

# extra
wd <- "C:/Users/Andy/Desktop/auto"
setwd(wd)

train_file <- "numerai_training_data.csv"
test_file <- "numerai_tournament_data.csv"
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

load_files <- as.list(list.files(path = "C:/Users/Andy/Desktop/auto/start.ml/",
                                 pattern = "start*", full.names = TRUE))
load_files <- load_files[-which(load_files %in% "C:/Users/Andy/Desktop/auto/start.ml/start_setup.R")]

getfiles <- lapply(load_files, source)

# extra
wd <- "C:/Users/Andy/Desktop/auto"
setwd(wd)

train_file <- "train.csv"
test_file <- "test.csv"


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
                  eval_metric = "MSE",
                  validation_type = "SharedHoldout",
                  runtime_secs = 1200, 
                  split_seed = 1234
)


metric <- start.validmetric(model_list, eval_metric = "RMSLE")

sorted_models <- start.sortmodels(model_list, eval_metric = eval_metric)

selected_models <- start.selectmodels(sorted_models, model_list, number_top_models = 334)


predictions <- start.predict(test = test, selected_models)

# test 
# make a data frame
r_data <- lapply(predictions, as.data.frame)
pred_df <- do.call('cbind', r_data)
basic_bag <- rowMeans(pred_df)
r_test <- as.data.frame(test)
id <- r_test$Id
output <- data.frame(Id = id, SalePrice = basic_bag)

write.csv(output, "test_sub_2.csv", row.names = FALSE, quote = FALSE)

# view all models 

validations <- start.predict(valid, selected_models)
r_val <- lapply(validations, as.data.frame)
valid_df <- do.call('cbind', r_val)

#!! some correlation check
corr <- cor(valid_df) 
# uncorrelateds are 140 and 300

cor(valid_df[,1], valid_df[,80])

uncor <- c(1, 80)

val_bag <- rowMeans(valid_df[, uncor])

performance <- data.frame(valid_df[,seq(1, nrow(valid_df) - 1)], val_mean = val_bag, as.data.frame(valid$SalePrice))
performance <- data.frame(valid_df[, uncor], val_mean = val_bag, as.data.frame(valid$SalePrice))


library(ggplot2)
library(reshape2)
library(magrittr)

m_per <- melt(performance, c(ncol(performance) - 1,  ncol(performance)))

# probably deleate
#m_per$variable <- as.numeric(m_per$variable)

m_per[order(m_per$SalePrice),] %>% 
  ggplot() + 
  geom_point(aes(x = seq(1, nrow(m_per)), y = value, color = variable), alpha = 0.3) +
  geom_point(aes(x = seq(1, nrow(m_per)), y = SalePrice), col = "blue") + 
  geom_point(aes(x = seq(1, nrow(m_per)), y = val_mean),color = "black", alpha = 0.8, size = .5) +
  scale_color_discrete(guide=FALSE) + 
  theme_grey()

  #xlim(c(600,700)) + 
 # ylim(c(100000, 200000))













