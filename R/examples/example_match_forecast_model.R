# Builds a logistic regression model on a single sampled train data set obtained
# from instance models by applying a transformation function (e.g. means).
get_model <- function(data) {
  return(glm(y ~ ., family = binomial(link = "logit"), data = data))
}

# Example of a prior specification (basketball example), for an attribute P3A,
# which is a Poisson variable and measures 3-point attempts. Conjugate prior
# for Poisson distribution is gamma distribution with parameters a - scale and
# b - rate
priors_example <- list(
  "San Antonio Spurs" =
    list(
      "P3A" = list(a = 241.2, b = 12.4),
      ...
    ),
  "Golden State Warriors" =
    list(
      "P3A" = list(a = 280.9, b = 13.2),
      ...
    ),
  ...
)

# Builds the Match Forecast Model
mf_model <- match_forecast_model(
  # Data frame in event-instance format
  data = data_train,
  # Parametric assumption about attributes
  input_model_specification = "poisson",
  # How many distributions to fit to each attribute
  num_models = 100,
  # How to build feature vectors from distributions
  transformation = "means",
  get_model = get_model,
  priors = priors_example
)
