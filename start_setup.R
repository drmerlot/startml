#============================================================
## the prepare function

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


# run the ml file..
mlout <- start.ml(train = df1, test = test, y_name = "target", y_type = "discrete")


te <- start.autotrain(train = df1, y_name = 'target', y_type = 'discrete')
