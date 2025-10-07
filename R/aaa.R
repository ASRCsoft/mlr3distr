# These elements need to be at the top of the Collate: order!

mlr3distr_learners = new.env()
mlr3distr_tasks = new.env()
mlr3distr_measures = new.env()
mlr3distr_task_gens = new.env()
mlr3distr_pipeops = new.env()
mlr3distr_graphs = new.env()

register_learner = function(name, constructor) {
  assert_class(constructor, "R6ClassGenerator")
  if (name %in% names(mlr3distr_learners)) stopf("learner %s registered twice", name)
  mlr3distr_learners[[name]] = constructor
}

register_task = function(name, constructor) {
  if (name %in% names(mlr3distr_tasks)) stopf("task %s registered twice", name)
  mlr3distr_tasks[[name]] = constructor
}

register_measure = function(name, constructor) {
  if (name %in% names(mlr3distr_measures)) stopf("measure %s registered twice", name)
  mlr3distr_measures[[name]] = constructor
}

register_task_generator = function(name, constructor) {
  if (name %in% names(mlr3distr_task_gens)) stopf("task generator %s registered twice", name)
  mlr3distr_task_gens[[name]] = constructor
}

register_pipeop = function(name, constructor) {
  if (name %in% names(mlr3distr_pipeops)) stopf("pipeop %s registered twice", name)
  mlr3distr_pipeops[[name]] = constructor
}

register_graph = function(name, constructor) {
  if (name %in% names(mlr3distr_graphs)) stopf("graph %s registered twice", name)
  mlr3distr_graphs[[name]] = constructor
}

register_mlr3 = function() {
  # tasks
  mlr_tasks = utils::getFromNamespace("mlr_tasks", ns = "mlr3")
  iwalk(as.list(mlr3distr_tasks), function(obj, name) mlr_tasks$add(name, obj)) # nolint

  # task generators
  mlr_task_gens = utils::getFromNamespace("mlr_task_generators", ns = "mlr3")
  iwalk(as.list(mlr3distr_task_gens), function(obj, name) mlr_task_gens$add(name, obj)) # nolint

  # learners
  mlr_learners = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  iwalk(as.list(mlr3distr_learners), function(obj, name) mlr_learners$add(name, obj)) # nolint

  # measures
  mlr_measures = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  iwalk(as.list(mlr3distr_measures), function(obj, name) mlr_measures$add(name, obj)) # nolint
}

register_mlr3pipelines = function() {
  mlr3pipelines::add_class_hierarchy_cache(c("PredictionSurv", "Prediction"))

  # pipeops
  mlr_pipeops = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  iwalk(as.list(mlr3distr_pipeops), function(obj, name) mlr_pipeops$add(name, obj)) # nolint

  # graphs
  mlr_graphs = utils::getFromNamespace("mlr_graphs", ns = "mlr3pipelines")
  iwalk(as.list(mlr3distr_graphs), function(obj, name) mlr_graphs$add(name, obj)) # nolint
}
