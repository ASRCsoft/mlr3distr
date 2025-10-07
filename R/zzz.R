
# nolint start
#' @import checkmate
#' @import data.table
#' @import distr6
#' @import ggplot2
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @importFrom R6 R6Class
#' @importFrom utils data head tail
#' @importFrom stats model.matrix model.frame sd predict density median quantile setNames
#' @importFrom utils getFromNamespace
#' @importFrom mlr3pipelines po as_graph %>>% pipeline_greplicate gunion Graph ppl
"_PACKAGE"
# nolint end

# to silence RCMD check
utils::globalVariables(c(
  "ShortName", "ClassName", "missing", "task", "value", "variable", "y"
))

.onLoad = function(libname, pkgname) {
  register_mlr3()
  if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
    register_mlr3pipelines()
  }

  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
  setHook(packageEvent("mlr3pipelines", "onLoad"), function(...) register_mlr3pipelines(),
    action = "append")
}

.onUnload = function(libpath) {
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = map_chr(hooks[-1L], function(x) environment(x)$pkgname)
  setHook(event, hooks[pkgname != "mlr3distr"], action = "replace")

  event = packageEvent("mlr3pipelines", "onLoad")
  hooks = getHook(event)
  pkgname = map_chr(hooks[-1L], function(x) environment(x)$pkgname)
  setHook(event, hooks[pkgname != "mlr3distr"], action = "replace")

  # unregister
  walk(names(mlr3distr_learners), function(nm) mlr_learners$remove(nm))
  walk(names(mlr3distr_tasks), function(nm) mlr_tasks$remove(nm))
  walk(names(mlr3distr_measures), function(nm) mlr_measures$remove(nm))
  walk(names(mlr3distr_task_gens), function(nm) mlr_task_generators$remove(nm))
  walk(names(mlr3distr_pipeops), function(nm) mlr3pipelines::mlr_pipeops$remove(nm))
  walk(names(mlr3distr_graphs), function(nm) mlr3pipelines::mlr_graphs$remove(nm))
}

leanify_package()
