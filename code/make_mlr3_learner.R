library(mlr3)
#' @param automl: Automl `R6ClassGenerator` - not the constructed object!
#' @example
#'   lrn = make_mlr3_learner(Automl)
#'   res = resample(mytask, lrn, rsmp("holdout"))
#'   res$aggregate()

make_mlr3_learner = function(automl, ...) {
  checkmate::assert_class(automl, "R6ClassGenerator")
  automl = automl$new(...)
  checkmate::assert_r6(automl, "Automl", public = c("train", "predict"))
  class_name = paste0("LearnerClassif",  class(automl)[1])
  lrn = R6Class(
    class_name,
    
    inherit = LearnerClassif,
    
    public = list(
      automl = automl,
      
      initialize = function() {
        ps = ps()
        ps$values = list()
        super$initialize(
          id = paste0("classif.", class(automl)[1]),
          feature_types = c("numeric", "factor", "integer"),
          predict_types = c("response"),
          param_set = ps,
          properties = NULL
        )
      }
    ),
    
    private = list(
      
      .train = function(task) {
        # get parameters for training
        pars = self$param_set$get_values(tags = "train")
        
        data = task$data()
        
        # set column names to ensure consistency in fit and predict
        self$state$feature_names = task$feature_names
        
        # use the mlr3misc::invoke function (it's similar to do.call())
        mlr3misc::invoke(self$automl$train, data = data)
      },
      
      .predict = function(task) {
        # get parameters with tag "predict"
        pars = self$param_set$get_values(tags = "predict")
        newdata = task$data(cols = self$state$feature_names)
        pred = mlr3misc::invoke(self$automl$predict, newdata = newdata)
        list(response = pred)
      }
    )
  )
  return(lrn$new())
}
