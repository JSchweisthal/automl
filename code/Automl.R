# Abstract base class
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
    train = function(data) {
      # implementation here!
      invisible(self) # important!
    },
    
    predict = function(newdata) {
      #implementation here!
      rep(0, times = nrow(newdata))
    }
    
  )
  
)
