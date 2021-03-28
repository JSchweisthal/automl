# Abstract base class
source('code/module_functions_BO.R')
load("./results/default_model.RData")
 
library(R6)
Automl = R6Class(
  "Automl",
  public = list(
    
    model = default_model,
    
    train = function(data) {
      stop("Not implemented.")
      invisible(self) # important!
    },
    
    predict = function(newdata) {
      stop("Not implemented.")
    }
  )
)

AutomlCustom = R6Class(
  "AutomlCustom",
  inherit = Automl,
  public = list(
    train = function(data) {
      self$model = train_helper(data)
# implementation here!
      invisible(self) # important!
    },
    
    predict = function(newdata) {
      #implementation here!
      ndata = newdata
      self$model$predict_newdata(ndata)$response # change newdata by data
    }
    
  )
  
)

