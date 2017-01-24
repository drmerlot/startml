##############################
#
## h20 grid search for different model available in h2o.
# Searches for, and saves models
#
#############################

########################
#
# Load packages and start java framework ..
#
#######################

#
# set use of models by fitness threshold and

library(cvAUC)
library(h2o)
library(Metrics)
library(h2oEnsemble)

# start decent size (more ram than needed for this)
#h2o.init(nthreads=-1, max_mem_size="16G")
h2o.removeAll()

########################
#
# Load the nesseasry data in
#
#######################

wrappers_path <- wd

# make number, performance / correlation threshold would be best..
use_num <- 1

setwd(wd)

# df <- h2o.importFile(path = normalizePath("../1-11-17/numerai_training_data.csv"))
# test_real <- h2o.importFile(path = normalizePath("../1-11-17/numerai_tournament_data.csv"))
#
# # break the data
# splits <- h2o.splitFrame(df, 0.8, seed=321)
# train  <- h2o.assign(splits[[1]], "train.hex") # 80%
# valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%
#
# train = df
# # define the target
# y <- "target"
# x <- setdiff(names(train), y)
#
# # binomial distribution type
# train[,y] <- as.factor(train[,y])
# valid[,y] <- as.factor(valid[,y])
# df[,y] <- as.factor(df[,y])
#
# train_sm = train[1:2000,]

# temp before function
y_name <- 'target'
y_type <- "discrete"
percent_train_holdout = 0.1
seeds = c(1234, 4321, 3245, 5432)

train <- df1

# instead of that:
# break the data for holdout validation
splits <- h2o.splitFrame(train, 1 - percent_train_holdout, seed=seeds[1])
train  <- h2o.assign(splits[[1]], "train.hex") # 80%
valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%

# define the target and predictors
y <- y_name
x <- setdiff(names(df1), y)

# set variable type for proper auto options
if(y_type == "discrete") {
  train[,y] <- as.factor(train[,y])
  valid[,y] <- as.factor(valid[,y])
  df1[,y] <- as.factor(df1[,y])
} else {
  train[,y] <- as.numeric(train[,y])
  valid[,y] <- as.numeric(valid[,y])
  df1[,y] <- as.numeric(df1[,y])
}



########################
#
# Load the base models from stage 1
#  and their required wrappers.
#
#######################

wd <- "/Users/grad/Desktop/auto"
setwd(wd)

# get all the saved models and organize them by validation logloss

# leave at previous models for now..
#all_model_files = list.files(rf_path, full.names = TRUE)
#all_rf_models  <- sapply(all_model_files, function(m) h2o.loadModel(m))

#path = "C:/Users/Andy/Desktop/numerai/10-4-16/dl_mods_adjust"
#all_model_files = list.files(dl_path, full.names = TRUE)
#all_dl_models  <- sapply(all_model_files, function(m) h2o.loadModel(m))

#path = "C:/Users/Andy/Desktop/numerai/9-21-16/gbm_mods"
#all_model_files = list.files(gbm_path, full.names = TRUE)
#all_gbm_models  <- sapply(all_model_files, function(m) h2o.loadModel(m))


# rewrite above as a function .
autotrain.loadmodels <- function(path) {
  all_model_files <- list.files(path, full.names = TRUE)
  all_models  <- sapply(all_model_files, function(m) h2o.loadModel(m))
  all_models
}

# test the function:
dl_path <- paste(wd, "/deeplearning_models", sep = "")
rf_path <- paste(wd, "/rf_models", sep = "")
gbm_path <- paste(wd, "/gbm_models", sep = "")

model_paths <- list(dl_path, rf_path, gbm_path)

all_models <- lapply(model_paths, autotrain.loadmodels)

# logloss sorter
sort_model_list <- function(model_list) {
  logloss <- h2o.logloss(model_list, valid = TRUE)
}

get_best_models <- function(sorted_models, model_list, x) {
  best_models <- model_list[sorted_models$model_list_num[1:x]]
}

sort_models <- function(model_list, x) {
  metrics <- lapply(model_list, sort_model_list)
  ranking <- data.frame(mod = seq(1, length(model_list),1), metric = unlist(metrics))
  colnames(ranking) <- c('model_list_num', 'logloss')
  row.names(ranking) <- NULL
  sorted <- ranking[order(ranking$logloss),]
  best_models <- get_best_models(sorted, model_list, x)
  best_models
}


#best_rf_models = sort_models(all_rf_models, 14)
#best_dl_models = sort_models(all_dl_models, 14)
#best_gbm_models = sort_models(all_gbm_models, 14)


# use lapply to walk through instead of above  ..
best_models <- lapply(all_models, sort_models, use_num)



# load_wrappers = function(wrappers_path){
#   source(paste(path, 'ensemble_store_wrappers.R', sep = '/'))
# }

# load all the wrappers, will only use the ones needed
source(normalizePath(paste(wd, "/ensemble_store_wrappers.R", sep = "")))



########################
#
# Full available learner set
#
#
#######################

# define the model wrappers to use
learner <- c("h2o.deeplearning.ens.1",
             "h2o.deeplearning.ens.2",
             "h2o.deeplearning.ens.3",
             "h2o.deeplearning.ens.4",
             "h2o.deeplearning.ens.5",
             "h2o.deeplearning.ens.6",
             "h2o.deeplearning.ens.7",
             "h2o.deeplearning.ens.8",
             "h2o.deeplearning.ens.9",
             "h2o.deeplearning.ens.10",
             "h2o.deeplearning.ens.11",
             "h2o.deeplearning.ens.12",
             "h2o.deeplearning.ens.13",
             "h2o.deeplearning.ens.14" )

             "h2o.gbm.ens.1",
             "h2o.gbm.ens.2",
             "h2o.gbm.ens.3",
             "h2o.gbm.ens.4",
             "h2o.gbm.ens.5",
             "h2o.gbm.ens.6",
             "h2o.gbm.ens.7",
             "h2o.gbm.ens.8",
             "h2o.gbm.ens.9",
             "h2o.gbm.ens.10",
             "h2o.gbm.ens.11",
             "h2o.gbm.ens.12",
             "h2o.gbm.ens.13",
             "h2o.gbm.ens.14",

             "h2o.randomforest.ens.1",
             #"h2o.randomforest.ens.2",
             #"h2o.randomforest.ens.3",
             #"h2o.randomforest.ens.4",

             "h2o.randomforest.ens.5" ,
             "h2o.randomforest.ens.6" ,
             "h2o.randomforest.ens.7" ,
             "h2o.randomforest.ens.8" ,
             "h2o.randomforest.ens.9" ,
             "h2o.randomforest.ens.10" ,
             #"h2o.randomforest.ens.11" ,
             "h2o.randomforest.ens.12" ,
             "h2o.randomforest.ens.13" ,
             "h2o.randomforest.ens.14"
)


########################
#
# set up the 1st ensemble with h2oEnsemble
#  with holdout set to check
#
#  STILL TESTING
#######################

# define the model wrappers to use
learner <- c(#"h2o.deeplearning.ens.1",
             "h2o.deeplearning.ens.2",
             "h2o.deeplearning.ens.3",
             "h2o.deeplearning.ens.4",
             "h2o.deeplearning.ens.5",
             "h2o.deeplearning.ens.6",
             "h2o.deeplearning.ens.7",
             "h2o.deeplearning.ens.8",
             "h2o.deeplearning.ens.9",
             "h2o.deeplearning.ens.10",
             "h2o.deeplearning.ens.11",
             "h2o.deeplearning.ens.12",
             "h2o.deeplearning.ens.13",
             "h2o.deeplearning.ens.14",

             "h2o.gbm.ens.1",
             "h2o.gbm.ens.2",
             "h2o.gbm.ens.3",
             "h2o.gbm.ens.4",
             "h2o.gbm.ens.5",
             "h2o.gbm.ens.6",
             "h2o.gbm.ens.7",

             "h2o.randomforest.ens.1",
             "h2o.randomforest.ens.2",
             "h2o.randomforest.ens.3"
             #"h2o.randomforest.ens.4"
             #"h2o.randomforest.ens.5"
)

# grab a subset of learners to hopefully get a better score!!!
# just to test
#h2o.glm.2 <- function(..., alpha = 0.5) h2o.glm.wrapper(..., alpha = alpha)


# make the learners for testing purposes..



# get some low correlation base learners (from dimention reduction.. )
learner <- low_cor_base_learners



# define the model wrappers to use
learner <- c("h2o.deeplearning.wrapper.1",
             "h2o.randomForest.wrapper.1",
             "h2o.gbm.wrapper.1"

)


metalearner <- "h2o.deeplearning"

# fit the new base learners ...
fit_dl <- h2o.ensemble(x = x,
                       y = y,
                       training_frame = train,
                       learner = learner,
                       metalearner = metalearner,
                       cvControl = list(V = 3))

learner <- "test.1"

learner <- c('h2o.deeplearning', 'h2o.deeplearning')

# fit the new base learners ...
fit_dl <- h2o.ensemble(x = x,
                       y = y,
                       training_frame = train,
                       learner = learner,
                       metalearner = metalearner,
                       cvControl = list(V = 3))

h2o.save_ensemble(fit_dl, path = "C:/Users/Andy/Desktop/numerai/10-4-16/ensemble_mod_dl", force = TRUE)
fit_lc = h2o.load_ensemble(path = "C:/Users/Andy/Desktop/numerai/10-4-16/ensemble_mod_dl")
# get the validation predictions
pred <- predict(fit_dl, train)
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
labels <- as.data.frame(train[,y])[,1]

# check  overall
meta_per = cvAUC::AUC(predictions = predictions, labels = labels)

#check the ensemble
L <- 14
L <- length(learner)
auc <- sapply(seq(L), function(l) cvAUC::AUC(predictions = as.data.frame(pred$basepred)[,l], labels = labels))
base_learn_per = data.frame(learner, auc)


### change the meta learner
#fit_dl = h2o.metalearn(fit_lc, metalearner = "h2o.deeplearning.meta")


# get the validation predictions
pred <- predict(fit_dl, test_real)
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
labels <- as.data.frame(train[,y])[,1]

# check  overall
auc
auc=meta = cvAUC::AUC(predictions = predictions, labels = labels)

#check the ensemble
L <- length(learner)
auc <- sapply(seq(L), function(l) cvAUC::AUC(predictions = as.data.frame(pred$basepred)[,l], labels = labels))
data.frame(learner, auc)


# predict on the actual test data
pred <- predict(fit_dl, test_real)
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)





########################
#
# Export csv file
#
#######################

p = data.frame(predictions)
id = as.data.frame(test_real$t_id)
print = data.frame(id, p)
colnames(print) = c('t_id', 'probability')
write.csv(print, "C:/Users/Andy/Desktop/numerai/10-4-16/submission_37.csv", row.names = F)

########################
#
# shutdown the h2o frame

########################
#
# set up the 2nd ensemble with h2oEnsemble
#   with all training data
#
#######################

# define the model wrappers to use
learner <- c("h2o.deeplearning.ens.1","h2o.gbm.ens.1",
             "h2o.randomforest.ens.1"
)

learner <- c("h2o.randomforest.ens.1"

)
# grab a subset of learners to hopefully get a better score!!!
# just to test
h2o.glm.2 <- function(..., alpha = 0.5) h2o.glm.wrapper(..., alpha = alpha)

# can try different meta learners
metalearner <- "h2o.glm.2"

# fit the new base learners ...
fit_full <- h2o.ensemble(x = x, y = y,
                         training_frame = df,
                         family = "binomial",
                         learner = learner,
                         metalearner = metalearner,
                         cvControl = list(V = 2))

# predict on the actual test data
pred <- predict(fit_full, test_real)
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)



########################
#
# Export csv file
#
#######################

p = data.frame(predictions)
id = as.data.frame(id_labels)
print = data.frame(id, p)
colnames(print) = c('t_id', 'probability')
write.csv(print, "C:/Users/Andy/Desktop/numerai/9-14-16/submission_26.csv", row.names = F)

########################
#
# shutdown the h2o frame
#
#######################

h2o.shutdown(prompt=FALSE)

########################
#
# End
#
#######################


