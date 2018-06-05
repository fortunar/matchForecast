#' input_model_mvnormal_iw
#'
#' Multivariate Normal - Inverse Wishart model. Fits \code{num_models} multivariate
#' normal distributions to a data frame according to multivariate normal - inverse Wishart model.
#' If prior is not passed to the function, classic multivariate normal model with sample covariance
#' is used.
#' @param data Data frame of events with attributes to model jointly.
#' @param num_models Number of distributions to fit.
#' @param prior List of lists containing priors for mean and covariance matrix. Should
#' contain entries 'mean' and 'sigma'. Inner list corresponding to entry 'mean'
#' should specify parameters for the multivariate normal prior of the mean.
#' It's entries should be 'mean' (vector) and 'sigma' (matrix) of the appropriate size.
#' List corresponding to the outer list key 'sigma' should specify parameters for the inverse Wishart
#' prior of the covariance matrix. Keys in this list should be 'a' (scalar)
#' and 'S' (matrix). For example:
#' \code{list(mean = list(mean = .., sigma = ..), sigma = list(a = .., S = ..))},
#' values are omitted for clarity.
#' @return List of \code{num_models} S3 classes of type 'mvnormal'.
#'   The class 'mvnormal' has the following methods defined: \code{mean} and
#'   \code{sample}. These both sample from the posterior distribution of the mean
#'   of the multivariate distribution fit.
#' @family input models
input_model_mvnormal_iw <- function(data, num_models, prior) {
  if (num_models == 1) {
    return(
      list(
        structure(
          list(
            mean = list(
              mean = colMeans(data),
              sigma = cov(data)
            )
          ),
          class = "mvnormal"
        )
      )
    )
  }

  n <- nrow(data)
  k <- ncol(data)
  sig_s <- cov(data)
  mu_s <- colMeans(data)
  if (is.null(prior)) {
    mu_p <- mu_s
    sig_p <- 1 / n * sig_s
    means <- rmvnorm(num_models, mu_p, sig_p)
    fits <- rep(
      list(
        structure(
          list(
            mean = list(
              mean = NULL,
              sigma = sig_p
            )
          ),
          class = "mvnormal"
        )
      ),
      num_models
    )
    for (i in 1:num_models) {
      fits[[i]]$mean$mean <- means[i, ]
    }
  }
  else {
    mu_0 <- if (!is.null(prior$mean$mean)) prior$mean$mean else rep(0, k)
    L_0 <- if (!is.null(prior$mean$sigma)) prior$mean$sigma else diag(k)
    a_0 <- if (!is.null(prior$sigma$a)) prior$sigma$a else k
    S_0 <- if (!is.null(prior$sigma$S)) prior$sigma$S else diag(k)

    L_0_inv <- solve(L_0)

    fits <- list()
    for (i in 1:num_models) {
      sig_i <- riwish(a_0 + n, S_0 + sig_s)
      sig_i_inv <- solve(sig_i)
      mu_p <- solve(L_0_inv + n * sig_i_inv) %*% (L_0_inv %*% mu_0 + n * sig_i_inv %*% mu_s)
      sig_p <- solve(L_0_inv + n * sig_i_inv)

      mean_p_s <- c(rmvnorm(1, mu_p, sig_p))
      names(mean_p_s) <- colnames(data)

      fits[[i]] <- structure(
        list(
          mean = list(
            mean = mean_p_s,
            sigma = sig_i
          )
        ),
        class = "mvnormal"
      )
    }
  }
  return(fits)
}

# For multivariate case the following methods operate on mean parameter of the
# multivariate normal distribution (mean of mean, sample of mean)
mean.mvnormal <- function(model) {
  return(model$mean$mean)
}

sample.mvnormal <- function(model, num_samples) {
  return(rmvnorm(num_samples, model$mean$mean, model$mean$sigma))
}
