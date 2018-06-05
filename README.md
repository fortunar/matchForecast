# About the package
The purpose of **matchForecast** package is to forecast sport match outcomes by performing bagging using two level modeling. The approach takes into account the uncertainty in the inputs to avoid overconfidence of ML models. Since the inputs to a match outcome prediction problem are often uncertain the approach can enhance the predictive performance of ML models.

# Which problems is it applicable to?
The package is applicable to problems where the attributes are uncertain (estimated). In other words, attributes of feature vectors are estimated based on some known prior observations (measurements). 

The use case will be described on the problem of forecasting basketball games. Prior to a match we want to forecast we have data of the team performance in past matches. These data include 2 point shots made, 3 point shots made, rebounds etc. For a new basketball match, we do not know how a team will perform, which is why we somehow need to estimate the attributes of the actual feature vectors that we feed into a ML algorithm. We might be tempted to take the mean values of performance data from past matches. However we obtain point estimates of the attributes which contain uncertainty. When we don't have a lot of prior games available mean estimates can lead to overconfidence and poor performance. This package models the data from past games with  distributions. Multiple predictive models are then bagged by training ML models on feature vectors obtained from fitted distributions.

Let's generalize the use cases. Problems that this package is applicable to can be described by the notion of events and instances. Outcome of new events is what we want to predict and we want the algorithm to learn based on prior events. Instances are objects with attributes and participate in events. At each event one or multiple instances can participate. This package assumes instances have the same types of measured attributes. The attributes of participating instances in a past event are known and so is the outcome. For a new event we do not know the attributes or the outcome. We would like to estimate the unknown attributes of instances participating in the new event based on attributes from previous known events and predict the outcome.

# Input data: event-instance format

The main input to this package are events in the form of a data frame. One event corresponds to one row in the data frame. Each event contains columns with measurements of features of participating instances. Each event can also contain a `TIME` attribute, which can be used to weight the importance of prior events for estimating the distributions.

Columns of event-instance formatted data frame include instance ID's in the format: `ID_<number>` and attributes in the format `<attr_name>_<number>`. `<number>` suffix connects instances to attributes. The suffixed number of corresponding instance and attributes should be the same. Column for the outcome (dependant variable) of the event should be denoted with `y`. 

An example of the columns of the data for basketball:


| TIME | y | ID_1 | ID_2 | P2M_1| P3M_1 | P2M_2 | P3M_2| ...|
|-------|---|--------|-------|---------|----------|----------|-----|---|


In the basketball case ID_1 refers to ID of the home team and P2M_1, P3M_1 to the attributes of the home team. ID_2 refers to the ID of away team and P2M_2, P3M_2 to its attributes. y is the outcome of the match.

## How do we model attributes?
We treat attributes of instances as random variables. Known measurements of an attribute are realizations of the corresponding random variable. For each random variable we compute multiple possible distributions using known prior measurements and a parametric assumption. Parametric assumption is our belief of how the variable is distributed. If a random variable is a count, a good guess is that it is Poisson distributed. Therefore in that case we can use the Poisson - Gamma Bayesian model to fit multiple Poisson distributions to the random variable. With supported Bayesian models we can also use priors. This package supports the following univariate Bayesian models out of the box: 

* Poisson - gamma model for count attributes
* Normal model with sample variance
* Normal - Inverse Gamma model for numeric attributes
* Bernoulli - Beta model for binary attributes

One multivariate Bayesian model is also supported: Multivariate Normal - Inverse Wishart model to jointly model numeric attributes.

User can also use custom models (see examples for more information).

## How do we use the distributions we get from modeling attributes?
The distributions we obtain from modeling attributes of instances are used to construct feature vectors that are fed into ML models. Any quantity derived from the distribution can be used: means, medians, quantiles etc. 

# Usage
## Install the package:
```R
devtools::install_github("fortunar/matchForecast")
```
Detailed documentation of all the parameters and use cases is available by installing the package by typing `?<name_of_function>`.

## Building the model - match_forecast_model()
```R
# Function for obtaining a single instance of a ML model. In this case it builds a 
# logistic regression model on data btained by applying a transformation 
# function (in this case "means") on the distributions fitted to attributes.
get_model <- function(data) {
  return(glm(y ~ ., family = binomial(link = "logit"), data = data))
}

# Example of a prior specification (basketball example), for an attribute P3A,
# which is a Poisson variable and measures 3-point attempts.
# Conjugate prior for Poisson distribution is the Gamma distribution.
# Parameters 'a' and 'b' in the list below are shape and rate parameters
# of gamma priors
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
  # Parametric assumption about attributes - in this case we assume
  # all attributes are distributed by Poisson distribution
  input_model_specification = 'poisson',
  # How many distributions to fit to each attribute (also equal
  # to the number of bagged models
  num_models = 100,
  # How to build feature vectors from distributions
  transformation = "means",
  get_model = get_model,
  priors = priors_example
)
```

## Predicting with match_forecast_model

```R
# Predict function to act on a single bagged model and a single sampled test data set obtained
# from instance models by applying a transformation function (e.g. means).
predict_fun <- function(model, data) {
  data$y <- NULL
  return(predict(model, newdata = data, type = "response"))
}

# This yields 100 * 30 predictions (for each bagged model and each transformed 
# feature vector from the fitted distributions). It returns is a list of lists 
# of predictions. Each prediction (list) contains fields:
# - predictions: output of predict_fun passed to this method
# - model: index of the bagged model (in this case 1-100)
# - num_of_sample: index of the fitted distributions (in this case 1-30)
results <- predict(
  # The model of type 'match_forecast_model'
  mf_model,
  # Test data (data to predict): needs to include only participating 
  # instance ID's
  data_new = data_new,
  # How many distributions to fit to each attribute of an instance 
  # in the test data
  num_models = 30,
  predict_fun = predict_fun
)

```

## More examples


### Modeling independent attributes with different parametric assumptions

In the example above, we modeled all attributes using Poisson - Gamma model by passing setting  `input_model_specification =  'poisson'`. If we want to use different parametric assumptions for attributes we need to pass a list to `input_model_specification` with keys being column names and values strings representing parametric assumptions.

Independent parametric assumptions and their corresponding strings:

 - Poisson - Gamma model: "poisson"
 - Normal model with sample variance: "normal"
 - Normal - Inverse Gamma model: "normal_ig"
 - Bernoulli - Beta model: "bernoulli"

Example code (just building the model):
```
# List of keys being attribute names (without instance suffix!)
# and values being strings corresponding to parametric assumptions
mixed_input_models <- list(
	# 2 point shots made
	P2M = "poisson"
	# 3 point shots made
	P3M = "normal"
	# Win ratio
	WR = "bernoulli"
)
mf_model <- match_forecast_model(
    data = data_train,
    input_model_specification = mixed_input_models,
    num_models = 100,
    transformation = "means",
    get_model = get_model
  )
```

### Modeling dependent attributes with multivariate normal - inverse Wishart model

Multivariate normal - inverse Wishart model jointly models all attributes of an instance.

Example code:

```
bbm <- match_forecast_model(
    data = data,
    input_model_specification = list(dependent = T, type = "mvnormal_iw"),
    num_models = 100,
    transformation = "means",
    get_model = get_model
)
```
## Advanced usage - custom models

Packages uses two levels of representation for instance models. An instance model is a model of one single instance (e.g. a team). It contains attribute models which model attributes of the instance. Attribute models can be univariate or multivariate.

Instance model structure is defined by the package. In order to make a custom model, one needs to create an instance of 'instance_model' and fill it with custom attribute models.

### Building a custom model

To write a custom model, a function needs to be passed in as the `input_model_specification`. As arguments it receives data from a single instance and `num_models` parameter. It should return a list of length `num_models` of objects of class 'instance_model'. One 'instance_model' represents a single model fit to the instance. It contains parameter `models`, which is a list of key-value pairs. Keys correspond to attributes and values to corresponding attribute models (e.g. of type "poisson").

Constructor for the class 'instance_model':
```
instance_model <- function() {
  return(
    structure(
      list(
        models = list()
      ),
      class = "instance_model"
    )
  )
}
# Methods defined for the class 'instance_model'
add_model <- function(obj, name, model)
{
  UseMethod("add_model", obj)
}

# Adds an attribute model with a specific name to the models attribute of 'instance_model'
add_model.instance_model <- function(inst_model, name, model) {
  inst_model$models[[name]] <- model
  return(inst_model)
}

# Get a vector of means of the attribute models
mean.instance_model <- function(inst_model) {
  return(lapply(inst_model$models, mean))
}

# Get a vector representing a sample from the attribute models
sample.instance_model <- function() {
  return(lapply(inst_model$models, sample))
}

# Get vector of variances of the attribute models
var.instance_model <- function() {
  return(lapply(inst_model$models, var))
}
```

To write a custom model one needs to create an instance of class 'instance_model' and fill its `models` parameter with custom attribute models. Each of the attribute models inside the `models` parameter should support `mean`, `var` and `sample` methods. For an example on how to write an attribute model refer to the source code of existing attribute models (Poisson - Gamma model, Normal model with sample variance etc.).

Example code for the Poisson - Gamma attribute model (already supported by the package):

```
# data parameter is a numerical vector of known attribute values
input_model_poisson <- function(data, num_models, prior) {
  if (num_models == 1) {
    return(
      list(
        structure(
          list(
            rate = sum(data)/length(data)
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
```
