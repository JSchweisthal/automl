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
    levels_train_data = list(),
    
    train = function(data) {
      self$model = train_helper(data)
      
      # tokeep = which(sapply(data[, -"Delay"],is.factor))
      # self$factors_train_data = sapply((data[ , tokeep, with=FALSE]), levels)
      self$levels_train_data['Airline'] = names(sort(table((data$Airline)),decreasing=TRUE)[1])
      self$levels_train_data['AirportFrom'] = names(sort(table((data$AirportFrom)),decreasing=TRUE)[1])
      # implementation here!
      invisible(self) # important!
    },
    
    predict = function(newdata) {
      #implementation here!
      ndata = newdata
      ndata[!(Airline %in% self$factors_train_data$Airline), Airline := self$levels_train_data['Airline']]
      ndata[!(AirportFrom %in% self$factors_train_data$AirportFrom), AirportFrom := self$levels_train_data['AirportFrom']]
      self$model$predict_newdata(ndata)$response # change newdata by data
     # rep(0, times = nrow(newdata))
    }
    
  )
  
)

