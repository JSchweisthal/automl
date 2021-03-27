library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3filters)
library(paradox)
library(mlr3tuning)
library(fastDummies)
library(mlr3pipelines)
library(data.table)
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
  learner_names = c("classif.rpart", 
                    "classif.ranger"#,
                    #"classif.svm",
                    #,
                    #"classif.xgboost"
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

  for(learner_name in learner_names){
    # change data for xgboost
    data_task = copy(data)
    if(learner_name %in% c("classif.svm", "classif.xgboost")){
      data_task = cbind(dummy_cols(data[, -"Delay"], remove_selected_columns = TRUE), data[, "Delay"])
    }
    # task
    task <- TaskClassif$new(
      id = "flight", backend = data_task,
      target = "Delay")
    # assign wanted hyperparams per dataset
    if(learner_name == "classif.rpart"){
      params = list(
        ParamDbl$new("cp", lower = 0.001, upper = 0.1),
        ParamInt$new("minsplit", lower = 1, upper = 100)
      )
    }
    if(learner_name == "classif.ranger"){
      params = list(
        ParamInt$new("num.trees", lower = 3, upper = 30),
        ParamInt$new("min.node.size", lower = 1, upper = 10)
      )
    }
    if(learner_name == "classif.svm"){
      kernel = "radial"
      params = list(
        ParamDbl$new("gamma", lower = 1, upper = 10),
        ParamDbl$new("cost", lower = 0.1, upper = 10)
      )
    }
    if(learner_name == "classif.xgboost"){
      params = list(
        ParamInt$new("max_depth", lower = 1, upper = 10),
        ParamDbl$new("eta", lower = 0.1, upper = 0.5)
      )
    }
    if(learner_name == "classif.kknn"){
      params = list(
        ParamInt$new("k", lower = 3, upper = 10)
      )
    }
    # imputing missing values &
    # assign learner
    # for svm: learner <- lrn(learner_name, kernel = kernel, predict_type = "prob")
    # if(learner_name == "classif.svm"){
    #   graph = po("imputeoor") %>>% lrn(learner_name, kernel = kernel, type='C-classification', predict_type = "prob")
    # }
   
    learner = lrn(learner_name, predict_type = "prob")
    
    
    # graph = po("imputelearner")  %>>% lrn(learner_name, predict_type = "prob") # impute missing values
    # learner = GraphLearner$new(graph)
      #!
    
   
    # make parameter set & terminator
    if(!(learner_name %in% c("classif.log_reg", "classif.naive_bayes"))){
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
      current_result = data.frame(learner_name, at$tuning_result$classif.acc, I(at$tuning_result$learner_param_vals))
      names(current_result) = names(performance)
      performance = rbind(performance, current_result)
    }
    else{
      learner$train(task)
      current_result = data.frame(learner_name, learner$predict(task)$score(measure), list(params=""))
      names(current_result) = names(performance)
      performance = rbind(performance, current_result)
    }
      
    # # at$archive # maybe store also full archive (every hp constellation)?
  }
  

  # data.frame to data.table for easier querying
  performance = as.data.table(performance)
  # store best learner
  best_learner = performance[measure==max(performance$measure)]
  best_learner = best_learner[1, ]
  

  learner = lrn(best_learner$learner, predict_type = "prob")
  if(!(best_learner$learner %in% c("classif.log_reg", "classif.naive_bayes"))){
    learner$param_set$values = best_learner$parameters[[1]]
  }
  
  # imp_missind = po("missind")
  # imp_num = po("imputehist", param_vals = list(affect_columns = selector_type("numeric")))
  # imp_missind = po("missind", param_vals = list(affect_columns = NULL, which = "all"))
  # 
  # imp_fct = po("imputeoor",
  #              param_vals = list(affect_columns = selector_type("factor")))
  # graph = po("copy", 2) %>>%
  #   gunion(list(imp_missind, imp_num %>>% imp_fct)) %>>%
  #   po("featureunion")
  
  impgraph = list(
    po("imputesample"),
    po("missind")
  ) %>>% po("featureunion")
  
  # learner = GraphLearner$new(graph %>>% po(learner))
  learner = GraphLearner$new( #impgraph %>>% 
            po("fixfactors") %>>% 
            po("encode", method = "treatment",
            affect_columns = selector_type("factor")) %>>%  
            po(learner))
  learner$train(task)
  learner
}

# open questions:
# 1. HP-selection just on given data and then for new data just take best learner from given system (maybe with MDS or so for dataset similarity)
# and just finetunig on new data or whole learner selection and HP optimizitaion process?
# 2. GE-Error-Estimation: Nested resampling for learner selection needed since measure is not correct or 'order' sufficient for selection?
# 3. Nested resampling for evaluation of new dataset need or only normal CV on new dataset  sufficient since HP-selection maybe not necessary?
# task <- TaskClassif$new(
#   id = "flight", backend = data_test,
#   target = "Delay")
# test_train = train_helper(data_test)
# 
# test_train$predict(task)
# 
# test_train$predict_newdata(data_test[, -"Delay"])

# test_train$param_set
# test_train$importance()
# test_train$predict(task)

# library(mlrintermbo)
# tnr("intermbo")