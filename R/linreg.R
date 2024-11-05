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
  fields = list(coef_ = "numeric", scales_ = "list", lambda = "numeric"),
  contains = "XyRegressor"
  )


RidgeRegressor$methods(

  initialize = function(lambda = 1) {
    .self$lambda <- lambda
  },


  fit = function(X, y) {
    "Fits Ridge Regressor to X and y."

    n <- nrow(X); p <- ncol(X) - 1
    Inter <- 1

    Xm <- colMeans(X[, -Inter])
    ym <- mean(y)
    X <- X[, -Inter] - rep(Xm, rep(n, p))
    y <- y - ym

    Xscale <- drop(rep(1/n, n) %*% X^2)^0.5
    X <- X/rep(Xscale, rep(n, p))

    # Matrix formula for Ridge coefficients.
    coef <- as.vector(solve(t(X) %*% X + diag(.self$lambda, p)) %*% t(X) %*% y)


    scaledcoef <- t(as.matrix(coef / Xscale))
    inter <- ym - scaledcoef %*% Xm


    .self$coef_ <- c(inter, scaledcoef)
    .self$scales_ <- list(Xscale = Xscale, Xm = Xm, ym = ym)

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

  results = function() {
    X <- .self$Xy$X
    y <- .self$Xy$y
    B <- .self$regressor$coef_

    yhat <- .self$pred()
    residuals <- y - yhat
    degrees_of_freedom <- nrow(X) - ncol(X)
    residuals_variance <- as.numeric((t(residuals) %*% residuals) / degrees_of_freedom)
    B_variance <- residuals_variance * solve(t(X) %*% X)
    B_sd <- sqrt(diag(B_variance))
    t_values <- B / B_sd
    p_values <- 2 * pt(abs(t_values), degrees_of_freedom, lower.tail = FALSE)

    # Update `results` field.
    results <- list(
      B = B,
      yhat = yhat,
      residuals = residuals,
      degrees_of_freedom = degrees_of_freedom,
      residuals_variance = residuals_variance,
      B_variance = B_variance,
      B_se = sqrt(diag(B_variance)),
      t_values = t_values,
      p_values = p_values
    )

    return(results)

  },

  coef = function() {
    coef_ <- c(.self$regressor$coef_)
    setNames(coef_, .self$feature_names)
  },

  print = function() {
    .self$show()
    },

  pred = function(newdata = NULL) {

    if (is.null(newdata)) {
      X <- .self$Xy$X
    } else {
      X <- get_X_y(.self$formula, newdata)$X
    }

    .self$regressor$predict(X)

  },

  resid = function() {
    .self$results()$residuals

    },

  summary = function()  {

    digits <- 3
    results <- .self$results()

    rdf <- results$degrees_of_freedom
    rse <- sqrt(results$residuals_variance)


    # Generate summ data.frame.
    summ <- data.frame(
      A=results$B,
      B=results$B_se,
      C=results$t_values,
      D=results$p_values
    )
    colnames(summ) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

    # Add p-values.
    pv <- as.vector(summ[, "Pr(>|t|)"])
    Signif <- symnum(pv, corr = FALSE, na = FALSE,
                     cutpoints = c(0,  .001,.01,.05, .1, 1),
                     symbols   =  c("***","**","*","."," "))
    summ <- cbind(summ, Signif=format(Signif))

    base::print(summ)
    cat("Residual standard error:",
        format(signif(rse, digits)), "on", rdf, "degrees of freedom"
    )
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

#' Generic function `pred`
#'
#' @param x object
#' @param ... signature compatibility arguments
#' @export
#' @export
pred <- function(x, ...) UseMethod("pred")


#' Returns fitted values for :class:`linreg` object.
#'
#' @param x linreg object
#' @param ... signature compatibility arguments
#' @export
pred.FormulaRegressor <- function(x, ...) predict(x, ...)


#' Predict method for Formula Regressors
#'
#' @param object Object of class inheriting from "FormulaRegressor"
#' @param newdata An optional data frame in which to look for variables with
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
#' @param x Object of class inheriting from "FormulaRegressor"
#' @param ... signature compatibility arguments
#'
#' @export
coef.FormulaRegressor <- function(x, ...) x$coef()


#' Returns residuals for :class:`FormulaRegressor` object.
#'
#' @param x FormulaRegressor object
#' @param ... signature compatibility arguments
#' @export
residuals.FormulaRegressor <- function(x, ...) x$resid()


#' Returns summary for :class:`FormulaRegressor` object.
#'
#' @param object FormulaRegressor object
#' @param ... signature compatibility arguments
#' @export
summary.FormulaRegressor <- function(object, ...) object$summary()


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
