#' weighting_gaussian
#'
#' TODO: decide whether to include this or not. Draws a sample from data using gaussian weighting.
weighting_gaussian <- function(data, sd) {
  len <- nrow(data)
  dt_new <- data.table::copy(data)
  index <- rnorm(len, mean = 0, sd = sd)
  for (i in 1:len) {
    sample <- index[[i]]
    if (abs(sample) > 2 * sd) {
      sample <- 2 * sd
    } else {
      sample <- abs(sample)
    }
    dt_new[i, ] <- data[len - floor((sample * (len - 1) / (2 * sd))), ]
  }
  return(dt_new)
}


#' weighting_last_n
#'
#' Only uses last n data points from data passed to the function.
weighting_last_n <- function(n) {
  return(function(data) {
    len <- nrow(data)
    dt_new <- data.table::copy(data)
    if (len - n + 1 > 0) {
      for (i in 1:len) {
        dt_new[i, ] <- data[sample((len - n + 1):len), ]
      }
      return(dt_new)
    }
    return(data)
  })
}
