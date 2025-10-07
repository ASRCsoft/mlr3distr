
#' @export
MeasureRegrCRPS = R6::R6Class(
    "MeasureRegrCRPS",
    inherit = MeasureRegr,
    public = list(
        initialize = function() {
          super$initialize(
                    id = "regr.crps",
                    packages = 'scoringRules',
                    range = c(0, Inf),
                    minimize = TRUE,
                    predict_type = 'distr',
                    label = "Mean CRPS"
                )
        }
    ),
    private = list(
        .score = function(prediction, ...) {
          # Note that the predictions will be out of order if used after
          # `Prediction$filter()`. See
          # https://github.com/mlr-org/mlr3/issues/1400
          dist_types = unique(prediction$distr$modelTable$Distribution)
          if (length(dist_types) > 1) stop('All predictions must use the same distribution type')
          
          # first, get the type of distribution or sampling
          dist_type = prediction$distr$modelTable$Distribution[1]

          # then, use the corresponding CRPS method
          if (dist_type == 'Normal') {
            # check for wrong order
            resp1 = prediction$response
            resp2 = unlist(prediction$distr$getParameterValue('mean'))
            in_order = all(resp1 == resp2)
            if (!in_order) stop('Inconsistent responses, likely due to `filter` bug')
            mean = unlist(prediction$distr$getParameterValue('mean'))
            sd = unlist(prediction$distr$getParameterValue('sd'))
            pred_crps = scoringRules::crps_norm(prediction$truth, mean = mean, sd = sd)
          } else if (dist_type == 'Empirical') {
            sample_mat = prediction$distr$wrappedModels() %>%
              lapply(function(x) x$parameters()$values$data$samples) %>%
              do.call(rbind, .)
            pred_crps = scoringRules::crps_sample(prediction$truth, sample_mat)
          } else {
            stop(paste0('Distribution type ', dist_type, ' not implemented'))
          }
          mean(pred_crps)
        }
    )
)

register_measure("regr.crps", MeasureRegrCRPS)
