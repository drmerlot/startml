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
wd <- ""
setwd(wd)

train_file <- "~/Desktop/auto/numerai_training_data.csv"
test_file <- ""
y_name <- "target"
y_type <- "discrete" # or "continous"

#========================================
# the load data function.
autotrain.loaddata <- function(train_file) {
  df1 <- h2o.importFile(path = normalizePath(train_file))
  df1
}

# test
df1 <- autotrain.loaddata(train_file)


