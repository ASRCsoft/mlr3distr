
#' @export
PipeOpTargetTrafoScaleRangeDistr = R6::R6Class("PipeOpTargetTrafoScaleRangeDistr",
  inherit = mlr3pipelines::PipeOpTargetTrafoScaleRange,
  public = list(),
  private = list(
    .invert = function(prediction, predict_phase_state) {
      orig_means = unlist(prediction$distr$getParameterValue('mean'))
      orig_sds = unlist(prediction$distr$getParameterValue('sd'))
      new_means = (orig_means - self$state$offset) / self$state$scale
      new_sds = orig_sds / self$state$scale
      distrs = data.frame(mean = new_means, sd = new_sds) %>%
        distr6::VectorDistribution$new(distribution = "Normal",
                                       params = .)
      PredictionRegr$new(row_ids = prediction$row_ids,
                         truth = predict_phase_state$truth, distr = distrs)
    }
  )
)

register_pipeop("targettrafoscalerange_distr", PipeOpTargetTrafoScaleRangeDistr)
