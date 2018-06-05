#' transform_data
#'
#' Transforms distributions fitted to attributes to feature vectors by applying
#' \code{row_transformation}. \code{row_transformation} acts on a single instance.
#'
#' @param row_transformation transformation to apply to each instance's model.
transform_data <- function(row_transformation) {
  return(function(data) {
    data_transformed <- list()
    for (i in 1:length(data)) {
      data_transformed_row <- list()
      for (attribute in names(data[[i]])) {
        if (substr(attribute, 1, 15) != "instance_model_") {
          # Copy static attributes
          data_transformed_row[[attribute]] <- data[[i]][[attribute]]
        } else {
          row_transformed <- row_transformation(data[[i]][[attribute]])
          for (attr_transformed in names(row_transformed)) {
            data_transformed_row[[
            paste(attr_transformed, "_", substr_right(attribute, 1), sep = "")
            ]] <- row_transformed[[attr_transformed]]
          }
        }
      }
      data_transformed[[i]] <- data_transformed_row
    }
    return(data_transformed)
  })
}

#' transformation_means
#'
#' Extracts means from all the distributions fits to the attributes of an instance.
transformation_means <- function(instance_model) {
  return(mean(instance_model))
}
