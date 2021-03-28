library("mlr3")
library(R6)
library(stringi)
source("./code/Automl.R")
source("./code/make_mlr3_learner.R")
library(data.table)
library(ggplot2)
set.seed(1234)

dataset_files = list.files("datasets", "*.csv", full.names = TRUE)
all_datasets = lapply(dataset_files, function(x) {
  data = data.table::fread(x, stringsAsFactors = TRUE)
  data[, Delay := as.factor(Delay)]
})
names(all_datasets)  = stri_match_first_regex(dataset_files, "[A-Z]+")
all_tasks = mapply(TaskClassif$new, id = names(all_datasets), backend = all_datasets, target = "Delay")
learner = make_mlr3_learner(AutomlCustom)

# learner$train(all_tasks[[1]], row_ids = 1:100)
# preds = learner$predict(all_tasks[[3]])

resampling = rsmp("cv", folds = 5)
measure = msr("classif.acc")
cv_measure = list()
#cv_measures = list()
cv_models = list()

untuned_rf = lrn('classif.ranger', num.trees = 10)
rf_cv_measure = list()

i = 1

for(task in all_tasks[1:3]){
  result = resample(task = task, learner = learner, resampling = resampling)
  cv_measure[task$format()] = result$aggregate(measure)
  # cv_measures[task$format()] = result$score(measure)$classif.acc
  # cv_models[task$format()] = result$learner$automl$model$format()
  learner$train(task)
  cv_models[task$format()] = learner$automl$model$format()
  
  # untuned as comparison 
  result_rf = resample(task = task, learner = untuned_rf, resampling = resampling)
  rf_cv_measure[task$format()] = result_rf$aggregate(measure)
  
  cat(as.character(i), "/", as.character(length(all_tasks[1:2])), " Datasets CV finished.", sep = "")
  i = i+1
}

df_eval = data.table(dataset = names(cv_models), acc_automl = unlist(cv_measure), acc_untunedrf = unlist(rf_cv_measure), learner = unlist(cv_models))
df_eval$learner = sapply(1:nrow(df_eval), function(x) strsplit(df_eval$learner, split=".", fixed = T)[[x]][4])
df_eval$learner = sapply(1:nrow(df_eval), function(x) strsplit(df_eval$learner, split=">", fixed = T)[[x]][1])

ggplot(df_eval, aes(x=acc_untunedrf, y=acc_automl, col=learner)) +
  geom_point() +
  geom_abline(slope=1, intercept=0)
