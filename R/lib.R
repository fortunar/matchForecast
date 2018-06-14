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

#' reformat_to_df
#'
#' Sets column names of the newly created data frame according to the package
#' structure. Also sets column types.
reformat_to_df <- function(data_modeled, data_specification) {
  ordered_column_names <- c(data_specification$cols_static)
  for (i in 1:length(data_specification$cols_ids)) {
    ordered_column_names <- c(ordered_column_names, paste("ID_", i, sep = ""))
    ordered_column_names <- c(ordered_column_names, paste(data_specification$cols_measurements, "_", i, sep = ""))
  }
  colnames(data_modeled) <- ordered_column_names

  for (col in data_specification$cols_measurements_suffixed) {
    data_modeled[[col]] <- as.numeric(as.character(data_modeled[[col]]))
  }

  data_modeled$y <- as.numeric(as.character(data_modeled$y))

  return(data_modeled)
}
