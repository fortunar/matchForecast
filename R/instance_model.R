# Wrapper that contains all the models for instance attributes
instance_model <- function() {
  return(
    structure(
      list(
        models = list()
      ),
      class = "instance_model"
    )
  )
}

add_model <- function(obj, name, model) {
  UseMethod("add_model", obj)
}

add_model.instance_model <- function(inst_model, name, model) {
  inst_model$models[[name]] <- model
  return(inst_model)
}

mean.instance_model <- function(inst_model) {
  return(lapply(inst_model$models, mean))
}

sample.instance_model <- function() {
  return(lapply(inst_model$models, sample))
}

var.instance_model <- function() {
  return(lapply(inst_model$models, var))
}
