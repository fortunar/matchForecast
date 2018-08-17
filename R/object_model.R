#' object_model
#'
#' Wrapper class that contains all of the models for object attributes.
object_model <- function() {
  return(
    structure(
      list(
        models = list()
      ),
      class = "object_model"
    )
  )
}

add_model <- function(obj, name, model)
{
  UseMethod("add_model", obj)
}

add_model.object_model <- function(obj_model, name, model) {
  obj_model$models[[name]] <- model
  return(obj_model)
}

mean.object_model <- function(obj_model) {
  return(lapply(obj_model$models, mean))
}

sample.object_model <- function(obj_model) {
  return(lapply(obj_model$models, sample))
}

var.object_model <- function(obj_model) {
  return(lapply(obj_model$models, var))
}

