#===========================================================
# get best models by performance and correlation threshold for ensemble or something
eval_metric = "RMSE"
eval_threshold = 0.5 # performance threshold
correlation_threshold = 0.7 # pearson correlation

start.selectmodels <- function(model_list,
                               test,
                               eval_metric,
                               eval_threshold = 0,
                               correlation_threhold = 1) {
  if(eval_metric == "AUC") {
    eval_fun <- function(a, b) {
      a >= b
    }
  } else {
    eval_fun <- function(a, b) {
      a <= b
      }
  }

  prediction_list <- start.predict(test, model_list)
  predictions <- h2o.cbind(predictions)
  correlations <- h2o.cor(predictions)

  metrics <- unlist(start.validmetric(model_list, eval_metric = eval_metric))

  selected_models
}



