## the prepare function
# right now just loads the train set, this needs some work....

# load libraries
library(h2o)
library(cvAUC)
library(Metrics)
library(h2oEnsemble)


## start decent size (more ram than needed for this)
h2o.shutdown(prompt = FALSE)
h2o.init(nthreads=2, max_mem_size="6G")
h2o.removeAll()

# extra
wd <- "/Users/grad/Desktop/auto"
setwd(wd)

train_file <- "/Users/grad/Desktop/auto/numerai_training_data.csv"
test_file <- "/Users/grad/Desktop/auto/numerai_tournament_data.csv"
y_name <- "target"
y_type <- "discrete" # or "continous"

#========================================
# the function
autotrain.loaddata <- function(train_file) {
  df1 <- h2o.importFile(path = normalizePath(train_file))
  df1
}

# test
df1 <- autotrain.loaddata(train_file)



