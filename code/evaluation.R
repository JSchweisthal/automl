library("mlr3")
library(R6)
library(stringi)
source("./code/Automl.R")
source("./code/make_mlr3_learner.R")
library(data.table)
library(ggplot2)
library(xtable)
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

for(task in all_tasks[11:20]){
  result = resample(task = task, learner = learner, resampling = resampling)
  cv_measure[task$format()] = result$aggregate(measure)
  # cv_measures[task$format()] = result$score(measure)$classif.acc
  # cv_models[task$format()] = result$learner$automl$model$format()
  learner$train(task)
  cv_models[task$format()] = learner$automl$model$format()
  
  # untuned as comparison 
  result_rf = resample(task = task, learner = untuned_rf, resampling = resampling)
  rf_cv_measure[task$format()] = result_rf$aggregate(measure)
  
  cat(as.character(i), "/", as.character(length(all_tasks)), " Datasets CV finished.", sep = "")
  print(i)
  i = i+1
}
# creating a data.table summerising the evaluation results
df_eval = data.table(dataset = names(cv_models), acc_automl = unlist(cv_measure), acc_untunedrf = unlist(rf_cv_measure), learner = unlist(cv_models))
df_eval$learner = sapply(1:nrow(df_eval), function(x) strsplit(df_eval$learner, split=".", fixed = T)[[x]][5])
df_eval$learner = sapply(1:nrow(df_eval), function(x) strsplit(df_eval$learner, split=">", fixed = T)[[x]][1])
# mean and standard deviation of the accouracy of the best learners over the dataframes
c(mean(df_eval$acc_automl), sqrt(var(df_eval$acc_automl)))
# how often are learners the best learners?
table(df_eval$learner)
# Latex table
xtable(df_eval)
# visualizing the accuracy of untuned ranger and automl learner
ggplot(df_eval, aes(x=acc_untunedrf, y=acc_automl, col=learner)) + theme_bw() +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  labs(title = "Generalized estimation accuracy") +
  xlab("Untuned random forest") +
  ylab("Learner of AutoML system") + 
  labs(col = "selected learner")

# saving the evaluations
save(file = "./results/cv_measure.RData", cv_measure)
save(file = "./results/cv_models.RData", cv_models)
save(rf_cv_measure, file ="./results/rf_cv_measure.RData")
save(df_eval, file = "./results/df_eval.RData")
write.csv(df_eval, "./results/df_eval.csv")


