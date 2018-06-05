# Predict function to act on a single bagged model and a single sampled test data set obtained
# from instance models by applying a transformation function (e.g. means).
predict_fun <- function(model, data) {
  data$y <- NULL
  return(predict(model, newdata = data, type = "response"))
}

# Build 100 models for each of the instances in test set and
# call predict function from each of the bagged models from mf_model
# on each of the 100 datasets obtained by transformation function.
predict(
  mf_model,
  data_new = data_new,
  num_models = 100,
  predict_fun = predict_fun
)
