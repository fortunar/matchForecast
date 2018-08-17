#' input_model_poisson
#'
#' Poisson - Gamma model. Fits \code{num_models} Poisson distributions to a numerical
#' vector (attribute).
#' @param data Numerical vector of data points.
#' @param num_models Number of distributions to fit.
#' @param prior List containing parameters of Gamma prior distribution for the
#' rate of Poisson distribution. List has to contain attributes "a" and "b" being the
#' shape and rate parameters.
#' @return List of \code{num_models} S3 classes of type 'poisson'
#'   The class 'poisson' has the following methods defined: mean, sample and var.
#' @family input models
input_model_poisson <- function(data, num_models, prior) {
  if (num_models == 1) {
    return(
      list(
        structure(
          list(
            rate = sum(data) / length(data)
          ),
          class = "poisson"
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
  rates <- rgamma(
    num_models,
    shape = sum(data) + prior_a,
    rate = length(data) + prior_b
  )
  fits <- rep(
    list(
      structure(
        list(
          rate = NULL
        ),
        class = "poisson"
      )
    ),
    length(rates)
  )
  for (i in 1:length(rates)) {
    fits[[i]]$rate <- rates[[i]]
  }
  return(fits)
}

mean.poisson <- function(model) {
  return(model$rate)
}

sample.poisson <- function(model, num_samples) {
  return(rpois(num_samples, model$rate))
}

var.poisson <- function(model) {
  return(model$rate)
}
