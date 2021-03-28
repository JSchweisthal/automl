library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3filters)
library(paradox)
library(mlr3tuning)
library(fastDummies)
library(mlr3pipelines)
library(data.table)
library(mlrintermbo)
# requireNamespace("lgr")

# data
# specify datasets on which should be trained on
# path = 'datasets/'
# datasets = c('arrivals_ATL') # for demonstration purposes: just one dataset, later all
# format = '.csv'
# data_test = data.table::fread(paste(path, dataset, format, sep = ""), stringsAsFactors = TRUE)
# data_test[, Delay := as.factor(Delay)]

# specify methods and learners (hyper params to be optimized should be specified in loop)
train_helper = function(data){
  # measure
  measure_name = "classif.acc"
  # tuner
  tuner_name = "random_search"
  learner_names = c(
    "classif.rpart",
    "classif.ranger"#,
    #"classif.svm",
    ,
    "classif.xgboost"
    #, "classif.kknn"
    #, "classif.log_reg"
    , "classif.naive_bayes"
  )
  measure <- msr(measure_name)
  # resampling method
  resampling <- rsmp("cv", folds = 5)
  # initializing empty dataframe for storing best hyperparamter constellation and accuracy
  performance = data.frame(matrix(ncol = 3, nrow = 0))
  colnames(performance) = c( "learner", "measure", "parameters")
  # hyperparameters to tune for all learner
  param_list = list(
    "classif.rpart" = list(
      ParamDbl$new("cp", lower = 0.001, upper = 0.1),
      ParamInt$new("minsplit", lower = 1, upper = 100)
    ),
    "classif.ranger" = list(
      ParamInt$new("num.trees", lower = 3, upper = 30),
      ParamInt$new("min.node.size", lower = 1, upper = 10)
    ),
    "classif.xgboost" = list(
      ParamInt$new("classif.xgboost.max_depth", lower = 1, upper = 10),
      ParamDbl$new("classif.xgboost.eta", lower = 0.1, upper = 0.5)
    ),
    "classif.kknn" = list(
      ParamInt$new("k", lower = 3, upper = 10)
    )
  )
  
  for(learner_name in learner_names){
    # change data for xgboost
    data_task = copy(data)
    # if(learner_name %in% c("classif.svm", "classif.xgboost")){
    #   data_task = cbind(dummy_cols(data[, -"Delay"], remove_selected_columns = TRUE), data[, "Delay"])
    # }
    # task
    task <- TaskClassif$new(
      id = "flight", backend = data_task,
      target = "Delay")
    # assign wanted hyperparams per dataset
    params = param_list[[learner_name]]
    # assign learner
    learner = lrn(learner_name, predict_type = "prob")
    # build pipeline for xgboost since it cannot handle categorical input directly
    if(learner_name == "classif.xgboost"){
      fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
      pipe_boost = fencoder %>>% learner
      learner = GraphLearner$new(pipe_boost)
    }
    # make parameter set & terminator
    if(!(learner_name %in% c("classif.log_reg", "classif.naive_bayes"))){
      tune_ps <- ParamSet$new(params)
      terminator <- trm("evals", n_evals = 5)
      # choose random search
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
      current_result = data.frame(learner_name, at$tuning_result$classif.acc, I(at$tuning_result$learner_param_vals))
      names(current_result) = names(performance)
      performance = rbind(performance, current_result)
    }
    else{
      result = resample(task = task, learner = learner, resampling = resampling)
      current_result = data.frame(learner_name,  result$aggregate(measure), list(params=""))
      names(current_result) = names(performance)
      performance = rbind(performance, current_result)
    }
  }
  # data.frame to data.table for easier querying
  performance = as.data.table(performance)
  # store best learner
  best_learner = performance[measure==max(performance$measure)]
  best_learner = best_learner[1, ]
  print(paste("THE BEST LEARNER IS", as.character(best_learner$learner), sep = " "))
  # optimizing the hyperparameters of the best model
  learner = lrn(as.character(best_learner$learner), predict_type = "prob")
  if(!(best_learner$learner %in% c("classif.log_reg", "classif.naive_bayes", "classif.xgboost"))){
    learner$param_set$values = best_learner$parameters[[1]] # new default parameters are the already tuned ones
  }
  if(learner_name == "classif.xgboost"){
    fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
    pipe_boost = fencoder %>>% learner
    learner = GraphLearner$new(pipe_boost)
    learner$param_set$values = best_learner$parameters[[1]]
  }
  # need imputation for prediction
  learner = GraphLearner$new( #impgraph %>>% 
    po("fixfactors") %>>% 
      po("encode", method = "treatment",
         affect_columns = selector_type("factor")) %>>%  
      po("imputeoor") %>>%
      po(learner))
  params <- copy(param_list[[as.character(best_learner$learner)]])
  if(!(as.character(best_learner$learner) %in% c("classif.log_reg", "classif.naive_bayes"))){
    # create the parameter names for the po
    if (as.character(best_learner$learner) != "classif.xgboost") {
      names(params) = paste(as.character(best_learner$learner), sapply(seq_len(length(params)), function(y) params[[y]]$id), sep = ".")
      sapply(seq_len(length(params)), function(y) params[[y]]$id  <- names(params)[y])
    }
    tune_ps <- ParamSet$new(params = param_list[[as.character(best_learner$learner)]])
    # set the default from first tuning step of BO
    # for (name in sapply(seq_len(length(params)), function(x) params[[x]]$id)) {
    #   tune_ps$params[[name]]$default = learner$param_set$values[[grep(name, names(learner$param_set$values))]]
    #   tune_ps$params[[name]]$id = paste(as.character(best_learner$learner), tune_ps$params[[name]]$id, sep = ".")
    # }
    # tuner <- tnr("intermbo") throws an error from the genoid package
    # Since BO did not work: stay with random search, just evaluate more points
    terminator <- trm("evals", n_evals = 10)
    # choose random search 
    tuner <- tnr("random_search")
    at <- AutoTuner$new(
      learner = learner,
      resampling = resampling,
      measure = measure,
      search_space = tune_ps,
      terminator = terminator,
      tuner = tuner
    )
    at$train(task)
    at
  } else {
    learner$train(task)
    learner
  } 
}

# open questions:
# 1. HP-selection just on given data and then for new data just take best learner from given system (maybe with MDS or so for dataset similarity)
# and just finetunig on new data or whole learner selection and HP optimizitaion process?
# 2. GE-Error-Estimation: Nested resampling for learner selection needed since measure is not correct or 'order' sufficient for selection?
# 3. Nested resampling for evaluation of new dataset need or only normal CV on new dataset  sufficient since HP-selection maybe not necessary?
