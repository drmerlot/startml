% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test-metric.R
\name{test_metric}
\alias{test_metric}
\title{test_metric}
\usage{
test_metric(prediction_list, test, eval_metric, y)
}
\arguments{
\item{prediction_list}{List object of H2O frames containing predictions.}

\item{test}{H2O frame object containing labeled data for model evaluation.}

\item{eval_metric}{Character object one of logloss, MSE, RMSE, MAE, AUC, or mean_per_class_error.}

\item{y}{Character object of length 1 identifying the column name of the target variable. No Default.}
}
\value{
List object same length as prediction_list containing performance of each model on test input with selected metric.
}
\description{
Caculate performance metrics from models on new data. Depends on Metrics package.
}
