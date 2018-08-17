#' input_model_bernoulli
#'
#' Bernoulli - Beta model. Fits \code{num_models} Bernoulli models to a numerical
#' vector (attribute).
#' @inheritParams input_model_poisson
#' @param prior List containing the conjugate prior distribution parameters (Beta in this
#'   case). List has to contain attributes "a" and "b" being the shape prior
#'   values.
#' @return List of \code{num_models} S3 classes of type 'bernoulli'
#'   The class 'bernoulli' has the following methods defined: mean, sample and var.
#' @family input models
input_model_bernoulli <- function(data, num_models, prior) {
  if (num_models == 1) {
    return(
      list(
        structure(
          list(
            p = sum(data) / length(data)
          ),
          class = "bernoulli"
        )
      )
    )
  }
  prior_a <- 0
  prior_b <- 0
  if (!is.null(prior)) {
    prior_a <- prior$a
    prior_b <- prior$b
  }
  probabilities <- rbeta(
    num_models,
    shape1 = sum(data) + prior_a,
    shape2 = length(data) - sum(data) + prior_b
  )

  fits <- rep(
    list(
      structure(
        list(
          p = NULL
        ),
        class = "bernoulli"
      )
    ),
    length(probabilities)
  )
  for (i in 1:length(probabilities)) {
    fits[[i]]$p <- probabilities[[i]]
  }
  return(fits)
}

mean.bernoulli <- function(model) {
  return(model$p)
}

sample.bernoulli <- function(model, num_samples) {
  return(rbinom(num_samples, 1, model$p))
}

var.bernoulli <- function(model) {
  return(model$p * (1 - model$p))
}
