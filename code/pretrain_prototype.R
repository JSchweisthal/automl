library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3filters)
library(paradox)
library(mlr3tuning)
# requireNamespace("lgr")

# specify methods and learners (hyper params to be optimized should be specified in loop)
measure_name = "classif.acc"
tuner_name = "random_search"
learner_names = c("classif.rpart", "classif.ranger")
# specify task
task <- TaskClassif$new(
  id = "flight", backend = data,
  target = "Delay")
measure <- msr(measure_name)
# resampling method
resampling <- rsmp("cv", folds = 5)
# initializing empty dataframe for storing best hyperparamter constellation and accuracy
performance = data.frame(matrix(ncol = 4, nrow = 0))
colnames(performance) = c("dataset", "learner", "measure", "parameters")

# specify datasets on which should be trained on
path = 'datasets/'
datasets = c('arrivals_ATL') # for demonstration purposes: just one dataset, later all
format = '.csv'

# iterate over datasets
for(dataset in datasets){
  data = data.table::fread(paste(path, dataset, format, sep = ""), stringsAsFactors = TRUE)
  data[, Delay := as.factor(Delay)]
  # iterate over learners
  for(learner_name in learner_names){
    # assign wanted hyperparams per dataset  
    if(learner_name == "classif.rpart"){
      params = list(
        ParamDbl$new("cp", lower = 0.001, upper = 0.1),
        ParamInt$new("minsplit", lower = 1, upper = 100)
      )
    } else if(learner_name == "classif.ranger"){
      params = list(
        ParamInt$new("num.trees", lower = 3, upper = 30),
        ParamInt$new("min.node.size", lower = 1, upper = 10)
      )
    }
    # assign learner
    learner <- lrn(learner_name, predict_type = "prob")
    # make parameter set & terminator
    tune_ps <- ParamSet$new(params)
    terminator <- trm("evals", n_evals = 5)
    # choose random search - why not grid search?
    tuner <- tnr(tuner_name)
    at <- AutoTuner$new(
      learner = learner,
      resampling = resampling,
      measure = measure,
      search_space = tune_ps,
      terminator = terminator,
      tuner = tuner
    )
    at$train(task)
    # save results in data.frame
    current_result = data.frame(dataset, learner_name, at$tuning_result$classif.acc, I(at$tuning_result$learner_param_vals))
    names(current_result) = names(performance)
    performance = rbind(performance, current_result)
    # # at$archive # maybe store also full archive (every hp constellation)?
  }
}
# data.frame to data.table for easier querying
performance = as.data.table(performance)
# store best learner
best_learner = performance[measure==max(performance$measure), learner]

print(performance)

# open questions:
# 1. HP-selection just on given data and then for new data just take best learner from given system (maybe with MDS or so for dataset similarity) 
# and just finetunig on new data or whole learner selection and HP optimizitaion process?
# 2. GE-Error-Estimation: Nested resampling for learner selection needed since measure is not correct or 'order' sufficient for selection? 
# 3. Nested resampling for evaluation of new dataset need or only normal CV on new dataset  sufficient since HP-selection maybe not necessary?

print(performance)
