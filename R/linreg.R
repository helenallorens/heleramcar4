#' Generic function `pred`
#'
#' @param x object
#' @param ... signature compatibility arguments
#' @export
pred <- function(x, ...) UseMethod("pred")


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


  #' Predicts input data X.
  #'
  #' @param X Input data
  #' @return Predicted values
  #' @export
  predict = function(X) {
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

  #' @export
  initialize = function(lambda = 1) {
    .self$lambda <- lambda
  },

  #' Fits Ridge Regressor to X and y.
  #'
  #' @param X Design matrix
  #' @param y Target values
  #'
  #' @export
  fit = function(X, y) {
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


  #' Fits Linear Regressor to X and y.
  #'
  #' @param X Design matrix
  #' @param y Target values
  #'
  #' @export
  fit = function(X, y) {
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
#' @field regressor a :class:`Regressor` object
FormulaRegressor <- setRefClass(
  "FormulaRegressor",
  fields = list(
    formula = "formula", Xy = "list", datarg = "character",
    regressor = "Regressor"
    )
  )


FormulaRegressor$methods(

  initialize = function(formula, data) {

    .self$formula <- formula
    .self$Xy <- get_X_y(formula, data)
    .self$datarg <- deparse(substitute(data))

  },

  show = function() {

    cat("\nCall:\n")
    cat(class(.self), "(formula = ", deparse(.self$formula), ", data = ", .self$datarg, ")\n\n", sep = "")
    cat("Coefficients:\n")
    base::print(.self$regressor$coef_)

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

  #' @export
  initialize = function(formula, data) {

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

  #' @export
  initialize = function(formula, data, lambda = 1) {

    callSuper(formula = formula, data = data)
    .self$regressor <- RidgeRegressor(lambda = lambda)
    .self$regressor$fit(X = .self$Xy$X, y = .self$Xy$y)

  }

)





# --------------------------------------------------------------------------
# Adding methods for :class:`FormulaRegressor` to existing generic functions
# --------------------------------------------------------------------------

#' Returns residuals for :class:`linreg` object.
#'
#' @param x linreg object
#' @param ... signature compatibility arguments
#' @export
residuals.linreg <- function(x, ...) x$resid()

#' Returns fitted values for :class:`linreg` object.
#'
#' @param x linreg object
#' @param ... signature compatibility arguments
#' @export
pred.linreg <- function(x, ...) x$pred()

#' Returns estimated coefficients for :class:`linreg` object.
#'
#' @param x linreg object
#' @param ... signature compatibility arguments
#' @export
coef.linreg <- function(x, ...) x$coef()
