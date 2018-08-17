#' input_model_normal
#'
#' Normal - sample variance model. Fits \code{num_models} normal models with
#' variance being sample variance to a numerical vector (attribute).
#' @inheritParams input_model_poisson
#' @param prior List containing the parameters of the Normal prior distribution of
#' the mean. List has to contain entries "mu" and "var" being the mean and
#' variance parameters. For example: \code{list(mu = 10, var = 2)}.
#' @return List of \code{num_models} S3 classes of type 'normal'.
#'   The class 'normal' has the following methods defined: mean, sample and var.
#' @family input models
input_model_normal <- function(data, num_models, prior) {
  var_s <- var(data)
  if (num_models == 1) {
    return(
      list(
        structure(
          list(
            mean = mean(data),
            sd = if (length(data) == 1) 0 else sd(data)
          ),
          class = "normal"
        )
      )
    )
  } else if (is.na(var_s) || var_s == 0) {
    return(
      rep(
        list(
          structure(
            list(
              mean = mean(data),
              sd = 0
            ),
            class = "normal"
          )
        ),
        num_models
      )
    )
  }

  mu_0 <- 0
  var_0 <- Inf

  if (!is.null(prior)) {
    mu_0 <- prior$mu
    var_0 <- prior$var
  }

  mu_s <- mean(data)
  var_s <- var(data)
  n <- length(data)

  # Calculate posterior parameters
  mu_p <- (1 / var_0 * mu_0 + n / var_s * mu_s) / (1 / var_0 + n / var_s)
  var_p <- 1 / (1 / var_0 + n / var_s)

  means <- rnorm(num_models, mu_p, sqrt(var_p))

  fits <- rep(
    list(
      structure(
        list(
          mean = NULL,
          sd = sqrt(var_s)
        ),
        class = "normal"
      )
    ),
    length(means)
  )
  for (i in 1:length(means)) {
    fits[[i]]$mean <- means[[i]]
  }
  return(fits)
}

#' input_model_normal_ig
#'
#' Fits \code{num_samples} normal models according to normal - inverse gamma model.
#' @inheritParams input_model_poisson
#' @param prior List of two lists with keys 'mean' and 'var'. List corresponding to key
#' 'mean' needs to have entries 'mu' (mean of the prior mean distribution) and 'k'
#' (variance scaler). List corresponding to key 'var' needs to have entries 'a' and 'b'
#' corresponding to rate and scale of the inverse gamma prior for variance. For example:
#' \code{list(mean = list(mu = 10, k = 2), var = list(a = 5, b = 2))}.
#' @return List of \code{num_samples} S3 classes of type 'normal'.
#'   The class 'normal' has the following methods defined: mean, sample and var.
#' @family input models
input_model_normal_ig <- function(data, num_models, prior) {
  n <- length(data)

  mu_0 <- 0
  # this is defaulted to 1 to prevent NA's appearing when no prior is given
  k_0 <- 1

  a_0 <- 0
  b_0 <- 0

  if (!is.null(prior)) {
    # mean prior parameters
    mu_0 <- prior$mean$mu
    k_0 <- prior$mean$k

    # var prior parameters
    a_0 <- prior$var$a
    b_0 <- prior$var$b
  }

  mean_s <- mean(data)

  a_p <- a_0 + n / 2
  b_p <- b_0 + 1 / 2 * (sum((data - mean_s)) + (k_0 * n) / (k_0 + n) * (mean_s - mu_0))

  mu_p <- (k_0 * mu_0 + n * mean_s) / (k_0 + n)

  vars <- 1 / rgamma(num_models, shape = a_p, rate = b_p)

  fits <- rep(
    list(
      structure(
        list(
          mean = NULL,
          sd = NULL
        ),
        class = "normal"
      )
    ),
    num_models
  )
  for (i in 1:num_models) {
    fits[[i]]$sd <- sqrt(vars[[i]])
    var_p <- vars[[i]] / (k_0 + n)
    fits[[i]]$mean <- rnorm(1, mu_p, sqrt(var_p))
  }
  return(fits)
}


mean.normal <- function(model) {
  return(model$mean)
}

sample.normal <- function(model, num_samples) {
  return(rnorm(num_samples, model$mean, model$sd))
}

var.normal <- function(model) {
  return((model$sd^2))
}
