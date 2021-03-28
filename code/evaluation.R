library("mlr3")
library(R6)
library(stringi)
source("./code/Automl.R")
source("./code/make_mlr3_learner.R")
set.seed(1)

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

resampling = rsmp("cv", folds = 3)
measures = msr("classif.acc")
cv_measure = list()
cv_measures = list()
cv_models = list()
i = 1

for(task in all_tasks[1:2]){
  result = resample(task = task, learner = learner, resampling = resampling)
  cv_measure[task$format()] = result$aggregate(measure)
  cv_measures[task$format()] = result$score(measure)$classif.acc
  cv_models[task$format()] = result$learners
  cat(as.character(i), "/", as.character(length(all_tasks[1:2])), " Datasets CV finished.", sep = "")
  i = i+1
  
}