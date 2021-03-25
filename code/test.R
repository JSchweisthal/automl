# Technical test
library("mlr3")
library(R6)
library(stringi)
library(ps)
source("./code/Automl.R")
source("./code/make_mlr3_learner.R")


set.seed(1)
dataset_files = list.files("datasets", "*.csv", full.names = TRUE)
all_datasets = lapply(dataset_files, function(x) {
  data = data.table::fread(x, stringsAsFactors = TRUE)
  data[, Delay := as.factor(Delay)]
})
names(all_datasets)  = stri_match_first_regex(dataset_files, "[A-Z]+")

# Test if the basic class works correctly
automl = AutomlCustom$new()
automl$train(head(all_datasets[[1]], 100))
preds = automl$predict(all_datasets[[1]])
stopifnot(all(preds %in% c(0,1)))

# Test if it works with mlr3
all_tasks = mapply(TaskClassif$new, id = names(all_datasets), backend = all_datasets, target = "Delay")
learner = make_mlr3_learner(AutomlCustom)
# learner$encapsulate = c(predict = "evaluate")
# learner$fallback = lrn("classif.featureless")
learner$train(all_tasks[[1]], row_ids = 1:1000)
preds = learner$predict(all_tasks[[1]])# $response
stopifnot(all(preds %in% c(0,1)))

res = resample(all_tasks[[1]], learner, rsmp("cv", folds = 3))
stopifnot(is.numeric(res$aggregate()))



