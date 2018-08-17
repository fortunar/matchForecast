#' input_model_normal_ig
#'
#' Normal - Inverse Gamma model. Jointly models mean and variance of a normal distribution.
#' @inheritParams input_model_poisson
#' @param prior List containing the parameters for Normal prior distribution of the mean
#' and Inverse Gamma prior distribution for the variance. For example:
#' \code{list(mean = list(mu = 10, k = 2), var = list(a = 5, b = 3)}.
#' @return List of \code{num_models} S3 classes of type 'normal'.
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

  a_p <- a_0 + n/2
  b_p <- b_0 + 1/2* (sum((data -mean_s)) +  (k_0*n)/(k_0 + n)*(mean_s - mu_0))

  mu_p <- (k_0*mu_0 + n*mean_s)/(k_0 + n)

  vars <- 1/rgamma(num_models, shape = a_p, rate = b_p)

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
    var_p <- vars[[i]]/(k_0 + n)
    fits[[i]]$mean <- rnorm(1, mu_p, sqrt(var_p))
  }
  return(fits)
}
