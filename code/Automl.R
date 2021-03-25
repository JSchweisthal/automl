# Abstract base class
source('code/module_functions.R')
library(R6)
Automl = R6Class(
  "Automl",
  public = list(
    
    model = NULL,
    
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
    # factors_train_data = NULL,
    
    train = function(data) {
      self$model = train_helper(data)
      
      # tokeep = which(sapply(data[, -"Delay"],is.factor))
      # self$factors_train_data = sapply((data[ , tokeep, with=FALSE]), levels)
      
      # implementation here!
      invisible(self) # important!
    },
    
    predict = function(newdata) {
      #implementation here!
      # data = newdata[newdata[, Airline %in% self$factors_train_data$Airline & AirportFrom %in% self$factors_train_data$AirportFrom]]
      self$model$predict_newdata(data)$response
     # rep(0, times = nrow(newdata))
    }
    
  )
  
)

