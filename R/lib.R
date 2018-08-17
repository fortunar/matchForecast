# Library of helper methods used by the package

#' get_measurement_colnames
#'
#' Get column names of measurements (without suffix) from data.frame in
#' event-instance format.
get_measurement_colnames <- function(data) {
  cols <- NULL
  for (colname in colnames(data)) {
    if (substr(colname, 1, 2) != "ID") {
      split <- strsplit(colname, "_")[[1]]
      if (!is.na(suppressWarnings(as.numeric(split[[length(split)]])))) {
        cols <- c(cols, paste(split[1:(length(split) - 1)], collapse = '_'))
      }
    }
  }
  return(unique(cols))
}

#' get_id_colnames
#'
#' Get column names of id columns from data.frame in event-instance format.
get_id_colnames <- function(data) {
  cols <- NULL
  for (colname in colnames(data)) {
    if (colname != "ID" && substr(colname, 1, 2) == "ID") {
      cols <- c(cols, colname)
    }
  }
  return(cols)
}


#' get_id_colnames
#'
#' Get static columns (i.e. ones that are not modelled) from data.frame in
#' event-instance format. For example: y, TIME, ID ...
get_static_colnames <- function(data) {
  cols <- c()
  for (colname in colnames(data)) {
    if (colname == "ID" || is.na(suppressWarnings(as.numeric(substr_right(colname, 1))))) {
      cols <- c(cols, colname)
    }
  }
  return(cols)
}

#' substr_right
#'
#' Gets last n characters from a string.
substr_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}
