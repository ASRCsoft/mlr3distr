
#' @export
LearnerTorchModuleDistr = R6::R6Class("LearnerTorchModuleDistr",
  inherit = mlr3torch::LearnerTorchModule,
  public = list(
      packages = 'torch',
      initialize = function(module_generator = NULL, param_set = NULL,
                            ingress_tokens = NULL, task_type, properties = NULL,
                            optimizer = NULL, loss = NULL, callbacks = list(),
                            packages = character(0), feature_types = NULL,
                            predict_types = NULL) {
        super$initialize(module_generator = module_generator,
                         param_set = param_set, ingress_tokens = ingress_tokens,
                         task_type = task_type, properties = properties,
                         optimizer = optimizer, loss = loss,
                         callbacks = callbacks, packages = packages,
                         feature_types = feature_types,
                         predict_types = predict_types)
      }
  ),
  private = list(
      .encode_prediction = function(predict_tensor, task) {
        # see `encode_prediction_default` for example
        if (self$predict_type == "response") {
          list(response = as.numeric(predict_tensor[, 1]))
        } else if (self$predict_type == "distr") {
          predict_mat = predict_tensor %>%
            as.numeric %>%
            matrix(ncol = 2, dimnames = list(NULL, c('mean', 'sd')))
          # check the numbers for problems
          if (any(is.na(predict_mat))) {
            # print(tensor_mat)
            # stop('NA tensor values')
            warning('NA tensor values will be replaced with zero (mean) and max double (sd)')
          }
          # for some reason `VectorDistribution` won't accept double.xmax?
          max_double = sqrt(.Machine$double.xmax)
          params = predict_mat %>%
            as.data.frame %>%
            # deal with bad numbers
            transform(mean = replace(mean, is.na(mean), 0),
                      sd = replace(sd, is.na(sd), .Machine$double.xmax)) %>%
            # original number is log sd
            transform(sd = exp(sd)) %>%
            # deal with too large numbers
            transform(sd = replace(sd, is.infinite(sd), max_double)) %>%
            transform(sd = replace(sd, sd > max_double, max_double))
          tryCatch({
            distrs = distr6::VectorDistribution$new(distribution = "Normal",
                                                    params = params)
          }, error = function(e) {
            print(summary(params))
            print(apply(params, 2, range, na.rm = TRUE))
            print(apply(params, 2, function(x) table(is.na(x))))
            stop(e)
          })
          list(distr = distrs, response = params$mean)
        } else {
          stopf("Invalid predict_type for task_type 'regr'.")
        }
      }
  )
)

register_learner("regr.moduledistr", LearnerTorchModuleDistr)
