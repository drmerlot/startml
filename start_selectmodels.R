#===========================================================
# get best models by performance and correlation threshold for ensemble or something

eval_metric = AUC
eval_threshold = 0.5 # performance threshold
correlation_threshold = 0.7 # pearson correlation

start.selectmodels <- function(model_list, test eval_threhold, correlation_threhold) {
  if(eval_metric == "AUC") {
    fun <- function(x, y) {
      x >= y
    }
  } else {
    fun <- function(x, y) {
      x <= y
      }
  }


  top_models <- model_list[sorted_models$model_list_num[1:number_top_models]]
  top_models
}