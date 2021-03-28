library("mlr3")
library(R6)
library(stringi)
library(dplyr)
source("./code/Automl.R")
source("./code/make_mlr3_learner.R")
library(data.table)

set.seed(1234)

dataset_files = list.files("datasets", "*.csv", full.names = TRUE)
all_datasets = lapply(dataset_files, function(x) {
  data = data.table::fread(x, stringsAsFactors = TRUE)
  data[, Delay := as.factor(Delay)]
})
names(all_datasets)  = stri_match_first_regex(dataset_files, "[A-Z]+")
# merging all the datasets together
full_data <- bind_rows(all_datasets)
# create a task for all
task <- TaskClassif$new(
  id = "flight", backend = full_data,
  target = "Delay")
learner = make_mlr3_learner(AutomlCustom)
learner$train(task) # train it
default_model = learner 

# save it as the default model
save(default_model, file = "./results/default_model.RData")
