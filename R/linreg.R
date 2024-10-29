#' Auxiliary function for getting X and y.
#'
#' @param formula an object of class "formula" .
#' @param data  a data.frame,
#'
#' @return list of X and y arrays.
get_X_y <- function(formula, data) {
  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]

  return(list(X = X, y = y))
}


# ----------------------------- Xy REGRESSORS -----------------------------

#' Base XyRegressor class
#'
#' Note: This class should not be used directly. Use derived classes instead.
#'
#' @field coef_ numeric.
XyRegressor <- setRefClass(
  "XyRegressor",
  fields = list(coef_ = "numeric")
)

XyRegressor$methods(

  predict = function(X) {
    "Predicts input data X"
    X %*% .self$coef_

    }
)

#' Ridge Regressor.
#'
#'
#' @field coef_ numeric.
#' @field lambda numeric.
#'
#' @import methods
#' @export RidgeRegressor
#' @exportClass RidgeRegressor
RidgeRegressor <- setRefClass(
  "RidgeRegressor",
  fields = list(coef_ = "numeric", lambda = "numeric"),
  contains = "XyRegressor"
  )


RidgeRegressor$methods(

  initialize = function(lambda = 1) {
    .self$lambda <- lambda
  },


  fit = function(X, y) {
    "Fits Ridge Regressor to X and y."
    .self$coef_ <- (
      as.vector(solve(t(X) %*% X + diag(.self$lambda, ncol(X))) %*% t(X) %*% y)
      )
  }
)


#' Linear Regressor.
#'
#'
#' @field coef_ numeric.
#'
#' @import methods
#' @export LinearRegressor
#' @exportClass LinearRegressor
LinearRegressor <- setRefClass(
  "LinearRegressor",
  fields = list(coef_ = "numeric"),
  contains = "XyRegressor"
)


LinearRegressor$methods(

  fit = function(X, y) {
    "Fits Linear Regressor to X and y."
    .self$coef_ <- as.vector(solve(t(X) %*% X) %*% t(X) %*% y)
  }
)


# ----------------------------- FORMULA REGRESSORS -----------------------------


#' Base FormulaRegressor class.
#'
#' Note: This class should not be used directly. Use derived classes instead.
#'
#' @field formula formula object
#' @field Xy list with X and y arrays
#' @field regressor a :class:`XyRegressor` object
FormulaRegressor <- setRefClass(
  "FormulaRegressor",
  fields = list(
    formula = "formula",
    Xy = "list",
    regressor = "XyRegressor",
    feature_names = "character",
    name = "ANY"
  )
)


FormulaRegressor$methods(

  initialize = function(formula, data) {
    .self$formula <- formula
    .self$Xy <- get_X_y(formula, data)
    .self$feature_names <- colnames(model.matrix(formula, data))
  },

  coef = function() {
    coef_ <- c(.self$regressor$coef_)
    setNames(coef_, .self$feature_names)
  },

  show = function() {

    # Create named coefficients vector.
    named_coefs <- c(.self$regressor$coef_)
    named_coefs <- setNames(named_coefs, .self$feature_names)

    cat("\nCall:\n")
    cat(class(.self), "(formula = ", deparse(.self$formula), ", data = ", .self$name, ")\n\n", sep = "")
    cat("Coefficients:\n")
    base::print(.self$coef())
  }
)


#' Performs Linear Regression model.
#'
#' @param formula
#' an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' The details of model specification are given under ‘Details’.
#' @param data
#' data frame, list or environment
#' (or object coercible by as.data.frame to a data frame) containing the
#' variables in the model. If not found in data, the variables are taken from
#' environment(formula),
#' @examples
#' lr <- linreg(Petal.Length~Species, data = iris)
#'
#' @import methods
#' @export linreg
#' @exportClass linreg
linreg <- setRefClass("linreg", contains = "FormulaRegressor")

linreg$methods(

  initialize = function(formula, data) {
    .self$name <- deparse(substitute(data))
    callSuper(formula = formula, data = data)
    .self$regressor <- LinearRegressor()
    .self$regressor$fit(X = .self$Xy$X, y = .self$Xy$y)

  }
)


#' Performs Ridge Regression model.
#'
#' @param formula
#' an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' The details of model specification are given under ‘Details’.
#' @param data
#' data frame, list or environment
#' (or object coercible by as.data.frame to a data frame) containing the
#' variables in the model. If not found in data, the variables are taken from
#' environment(formula),
#' @examples
#' data(iris)
#' ridge <- ridgereg(Petal.Length~Species, data = iris)
#'
#' @import methods
#' @export ridgereg
#' @exportClass ridgereg
ridgereg <- setRefClass("ridgereg", contains = "FormulaRegressor")

ridgereg$methods(

  initialize = function(formula, data, lambda = 1) {
    .self$name <- deparse(substitute(data))
    callSuper(formula = formula, data = data)
    .self$regressor <- RidgeRegressor(lambda = lambda)
    .self$regressor$fit(X = .self$Xy$X, y = .self$Xy$y)
  }
)


# ------------------------   Generic Functions --------------------------------

#' Predict method for Formula Regressors
#'
#' @param object. Object of class inheriting from "FormulaRegressor"
#' @param newdata. An optional data frame in which to look for variables with
#' which to predict. If omitted, the fitted values are used.
#'
#' @export
predict.FormulaRegressor <- function(object, newdata = NULL) {

  if (is.null(newdata)) {
    X <- object$Xy$X
  } else {
    X <- get_X_y(object$formula, newdata)$X
  }

  object$regressor$predict(X)
}

#' Extract Model Coefficients
#'
#' @param object Object of class inheriting from "FormulaRegressor"
#'
#' #' @export
coef.FormulaRegressor <- function(object, ...) {
  object$coef()
}


#---------------------visualize_airport_delays()--------------------------------
library(nycflights13)
library(dplyr)
library(ggplot2)

#' Visualize Airport Delays
#'
#' This function creates a plot visualizing the mean delay of flights 
#' for different airports, represented by their longitude and latitude.
#' The data is sourced from the `nycflights13` package, and the function 
#' utilizes `dplyr` for data manipulation and `ggplot2` for visualization.
#'
#' @export
visualize_airport_delays <- function() {
  mean_delays <- flights %>%
    filter(!is.na(dep_delay)) %>%                     
    group_by(dest) %>%                                
    summarise(mean_delay = mean(dep_delay, na.rm = TRUE))  
  
  delays_with_location <- mean_delays %>%
    inner_join(airports, by = c("dest" = "faa")) %>% 
    select(dest, mean_delay, lat, lon)                
  
  ggplot(delays_with_location, aes(x = lon, y = lat, color = mean_delay, size = mean_delay)) +
    geom_point(alpha = 0.7) +
    scale_color_gradient(low = "blue", high = "red") +  
    labs(
      title = "Mean Flight Delays by Airport",
      x = "Longitude",
      y = "Latitude",
      color = "Mean Delay (mins)",
      size = "Mean Delay"
    ) +
    theme_minimal()
}


visualize_airport_delays()



