
#' @export
LearnerRegrDRF = R6::R6Class(
  "LearnerRegrDRF",
  inherit = LearnerRegr,
  public = list(
    initialize = function() {
      param_set = paradox::ps(
          num.trees                    = paradox::p_int(1L, default = 500L, tags = c("train", "predict", "hotstart")),
          # num.features ?
          sample.fraction              = paradox::p_dbl(0L, 1L, tags = "train"),
          mtry                         = paradox::p_int(lower = 1L, special_vals = list(NULL), tags = "train"),
          mtry.ratio                   = paradox::p_dbl(lower = 0, upper = 1, tags = "train"),
          min.node.size                = paradox::p_int(1L, default = 15L, special_vals = list(NULL), tags = "train"),
          honesty                      = paradox::p_lgl(default = TRUE, tags = "train"),
          honesty.fraction             = paradox::p_dbl(lower = 0, upper = 1, default = .5, tags = "train"),
          honesty.prune.leaves         = paradox::p_lgl(default = TRUE, tags = "train"),
          alpha                        = paradox::p_dbl(lower = 0, upper = .25, default = .05, tags = "train"),
          ci.group.size                = paradox::p_int(default = 2L, tags = "train"),
          num.threads                  = paradox::p_int(lower = 1L, special_vals = list(NULL), tags = "train")
      )
      super$initialize(
        id = "regr.drf",
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        predict_types = "distr",
        packages = "drf",
        param_set = param_set,
        properties = c("weights", "missings"),
        label = "Non-homogeneous Gaussian Regression"
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      pv = mlr3learners:::convert_ratio(pv, "mtry", "mtry.ratio", length(task$feature_names))
      if ("weights" %in% task$properties) {
        pv$sample.weights = task$weights$weight
      }
      # mlr3misc::invoke(
      #     drf::drf,
      #     X = task$data(cols = task$feature_names),
      #     Y = task$data(cols = task$target_names),
      #     .args = pv
      #     )
      # drf::drf(X = task$data(cols = task$feature_names),
      #          Y = task$data(cols = task$target_names))
      args = list(X = task$data(cols = task$feature_names),
                  Y = task$data(cols = task$target_names))
      args = c(args, pv)
      tryCatch({
        do.call(drf::drf, args)
      }, error = function(e) {
        print('Error message:')
        print(as.character(e))
        warning(as.character(e))
      })
    },
    .predict = function(task) {
      pv = self$param_set$get_values(tags = "predict")
      newdata = task$data(cols = task$feature_names)
      n_pred = nrow(newdata)
      if (inherits(self$model, 'character')) {
        # print('Model is an error message:')
        # print(self$model)
        train_params = self$param_set$get_values(tags = "train")
        param_txt = paste(capture.output(print(train_params)), collapse=' ')
        warning_txt = paste0('Model is an error message. Parameters: ', param_txt)
        warning(warning_txt)
        # for now just return bad predictions
        params = data.frame(mean = rep(0, nrow(newdata)),
                            sd = sqrt(.Machine$double.xmax))
        distrs = distr6::VectorDistribution$new(distribution = "Normal",
                                                params = params)
        return(list(distr = distrs))
      }
      means = predict(self$model, newdata = newdata, functional = 'mean')
      sds = predict(self$model, newdata = newdata, functional = 'sd')
      # need to wrap in a `VectorDistribution` (see `LearnerRegr`)
      params = data.frame(mean = means, sd = sds)
      distrs = distr6::VectorDistribution$new(distribution = "Normal",
                                              params = params)
      list(distr = distrs)
    }
  )
)

register_learner("regr.drf", LearnerRegrDRF)
