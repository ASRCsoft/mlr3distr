
#' @export
LearnerRegrNGR = R6::R6Class(
  "LearnerRegrNGR",
  inherit = LearnerRegr,
  public = list(
    initialize = function() {
      param_set = paradox::ps(
        cp             = paradox::p_dbl(0, 1, default = 0.01, tags = "train"),
        maxcompete     = paradox::p_int(0L, default = 4L, tags = "train"),
        maxdepth       = paradox::p_int(1L, 30L, default = 30L, tags = "train"),
        maxsurrogate   = paradox::p_int(0L, default = 5L, tags = "train"),
        minbucket      = paradox::p_int(1L, tags = "train"),
        minsplit       = paradox::p_int(1L, default = 20L, tags = "train"),
        surrogatestyle = paradox::p_int(0L, 1L, default = 0L, tags = "train"),
        usesurrogate   = paradox::p_int(0L, 2L, default = 2L, tags = "train"),
        xval           = paradox::p_int(0L, default = 10L, tags = "train")
      )
      param_set$set_values(xval = 10L)
      super$initialize(
        id = "regr.ngr",
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        predict_types = "distr",
        packages = "crch",
        param_set = param_set,
        properties = c("weights", "missings"),
        label = "Non-homogeneous Gaussian Regression"
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      if ("weights" %in% task$properties) {
        pv$weights = task$weights$weight
      }
      # crch::crch(y ~ . | ., data = cbind(x, y = y), method = 'boosting', type = 'crps',
      #      maxit = 1000, nu = 0.05, mstop = 'aic')
      # invoke(
      #   crch::crch,
      #   formula = task$formula(),
      #   data = task$data(),
      #   .args = pv
      # )
      if ('formula' %in% names(task$extra_args)) {
        formula = task$extra_args$formula
      } else {
        formula = task$formula()
      }
      crch::crch(formula, data = task$data(), method = 'boosting',
                 type = 'crps', maxit = 1000, nu = 0.05, mstop = 'aic')
    },
    .predict = function(task) {
      pv = self$param_set$get_values(tags = "predict")
      # ensure same column order in train and predict
      # newdata = mlr3extralearners:::ordered_features(task, self)
      # response = mlr3misc::invoke(predict, self$model, newdata = task$data(), .args = pv)
      # need to wrap in a `VectorDistribution` (see `LearnerRegr`)
      params = mlr3misc::invoke(predict, self$model, newdata = task$data(),
                                .args = c(type = 'parameter', pv))
      names(params) = c('mean', 'sd')
      distrs = distr6::VectorDistribution$new(distribution = "Normal",
                                              params = params)
      list(distr = distrs)
    }
  )
)

register_learner("regr.ngr", LearnerRegrNGR)
