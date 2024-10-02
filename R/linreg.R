library(ggplot2)


#' Generic function `pred`
pred <- function(object, ...) UseMethod("pred")


#' Performs Linear Regression model.
#'
#' @param formula
#' an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' The details of model specification are given under ‘Details’.
#' @param data
#' n optional data frame, list or environment
#' (or object coercible by as.data.frame to a data frame) containing the
#' variables in the model. If not found in data, the variables are taken from
#' environment(formula),
#'
#' @return LinearRegression
#' @export
linreg <- function(formula, data) {
  lr <- LinearRegression$new(
    formula = formula,
    data = data,
    results = NULL
  )

  lr$fit()
  return(lr)
}

#' Linear Regression Results
#'
#' @field B numeric. Regressions coefficients
#' @field yhat matrix. The fitted values
#' @field residuals matrix. Residuals
#' @field residuals_variance numeric. The residuals variance
#' @field degrees_of_freedom numeric. Degrees of freedom
#' @field B_variance matrix. The variance of the regression coefficients
#' @field t_values numeric. The t-values for each coefficient
#' @field p_values numeric. The p-values for each coefficient
LinearRegressionResults <- setRefClass(
  "LinearRegressionResults",

  fields = list(
    B = "numeric",
    yhat = "matrix",
    residuals = "matrix",
    residuals_variance = "numeric",
    degrees_of_freedom = "numeric",
    B_variance = "matrix",
    B_se = "numeric",
    t_values = "numeric",
    p_values = "numeric"
  )
)


LinearRegression <- setRefClass(
  "LinearRegression",
  fields = list(
    formula = "formula",
    data = "data.frame",
    results = "ANY"
  )
)


LinearRegression$methods(

  fit = function() {

    # Define X, y.
    X <- model.matrix(formula, data)
    y <- data[[all.vars(formula)[1]]]

    # yhat = XB , where B is the vector of coefficients.
    B <- as.vector(solve(t(X) %*% X) %*% t(X) %*% y)
    yhat <- X %*% B

    # Results.
    residuals <- y - yhat
    degrees_of_freedom <- nrow(X) - ncol(X)
    residuals_variance <- as.numeric((t(residuals) %*% residuals) / degrees_of_freedom)
    B_variance <- residuals_variance * solve(t(X) %*% X)
    B_sd <- sqrt(diag(B_variance))
    t_values <- B / B_sd
    p_values <- 2 * pt(abs(t_values), degrees_of_freedom, lower.tail = FALSE)


    # Update `results` field.
    results <<- LinearRegressionResults$new(
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
  },

  show = function() {
    cat("Call: \n")
    print(.self$formula)
    cat("\n")
    cat("Coefficients: \n")
    print(get_named_coefs())
  },

  get_residuals = function() .self$results$residuals,

  get_predictions = function() .self$results$yhat,

  get_named_coefs = function() {
    B <- c(results$B)
    names <- colnames(model.matrix(formula, data))
    return(setNames(B, names))
  },

  get_summary = function()  {

    summary_ <- cbind(
      .self$results$B,
      .self$results$B_se,
      .self$results$t_values,
      .self$results$p_values
    )
    colnames(summary_) <- c("B", "B_se", "t_values", "p_values")
    return(summary_)
  },

  get_plot = function() {


    aes1 <- aes(x = .self$get_predictions(), y = .self$get_residuals())

    p1 <- (
      ggplot(data = data, mapping = aes1) +
      geom_point() +
      stat_summary(fun = median, geom = "line", color = "red")+
      labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")
      )

    print(p1)

    #aes2 <- aes(x= .self$get_predictions(), y = object$standard_residual)

    #p2 <- (
    #  ggplot(data = data, mapping = aes2) +
    #  geom_point() +
    #  stat_summary(fun = median, geom = "line", color = "red")+
    #  labs(title = "Scale-Location", x = "Fitted values", y = "Standardized residuals")
    #)
  }

)

# --------------------------------------------------------------------------
# Adding methods for :class:`LinearRegression` to existing generic functions
# --------------------------------------------------------------------------

#' Returns residuals for :class:`LinearRegression` object.
#' @export
residuals.LinearRegression <- function(object, ...) object$get_residuals()

#' Returns fitted values for :class:`LinearRegression` object.
#' @export
pred.LinearRegression <- function(object, ...) object$get_predictions()

#' Returns estimated coefficients for :class:`LinearRegression` object.
#' @export
coef.LinearRegression <- function(object, ...) object$get_named_coefs()

#' Returns summary for :class:`LinearRegression` object.
#' @export
summary.LinearRegression <- function(object, ...) object$get_summary()

#' Returns plot for :class:`LinearRegression` object.
#' @export
plot.LinearRegression <- function(object, ...) object$get_plot()
