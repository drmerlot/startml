#' regression_example

#' @return Plots example of regression on kaggle housing price data set to demonstrate the startml function

# @export
# !!! Needs to have the paths to demo data somehow globally defined and unrelated to current wd of user
# !!! also, example data needs some columns trimmed to avoid warnings based on insufficient train, valid, test partitioning (Its a pretty small dataset)
demo <- function() {
  h2o.init()
  cat('-----------------------------------------------------\n         *** Begin startml Demo ***\n')
  cat('-----------------------------------------------------\n\n\n')
  cat('...Reading in example data from Kaggle Ames Housing Prices Dataset...\n\n')
  cat('Loading Labeled_dataset\n')
  labeled_data <- h2o.importFile(path = normalizePath("data/train.csv"))
  cat('Loading New Data\n')
  newdata <- h2o.importFile(path = normalizePath("data/test.csv"))
  cat('\n\n\n')
  cat('...Start Model Building...\n\n\n')
  test_out <- startml(labeled_data = labeled_data,
                      newdata = newdata,
                      label_id = 'Id',
                      y = 'SalePrice',
                      split_seed = 1234,
                      runtime_secs = 10,
                      eval_metric = "RMSE"
  )

  # first look at all models
  cat('...Plotting summary of data distributions, training history, and model predictions\nand performance on test data...\n\n\n')
  plot(test_out)
  cat("\n\n\nTry it yourself by loading in the example data:\n
      > labeled_data <- h2o.importFile(path = normalizePath('data/train.csv'))\n
      > newdata <- h2o.importFile(path = normalizePath('data/test.csv'))\n
      > demo_out <- startml(labeled_data = labeled_data,\n
                 newdata = newdata,\n
                 label_id = 'Id',\n
                 y = 'SalePrice',\n
                 split_seed = 1234,\n
                 runtime_secs = 10,\n
                 eval_metric = 'RMSE')\n
      > plot(demo_out)\n\n\n")
  cat('-----------------------------------------------------\n         *** startml Demo Finshed ***\n')
  cat('-----------------------------------------------------\n\n\n')
}
