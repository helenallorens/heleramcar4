library(ggplot2)

#' Performs Linear Regression model.
#'
#' @param formula
#' @param data
#'
#' @return LinearRegression
#' @export
#'
linreg <- function(formula, data) {
  lr <- LinearRegression$new(
    formula = formula,
    data = data,
    results = NULL,
    lm = NULL
    )

  lr$fit()
  return(lr)
}

#' Linear Regression Results
#'
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
    results = "ANY",
    lm = "ANY"
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

    # Save result for lm
    lm <<- lm(formula, data = data)

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

  get_named_coefs = function() {
    B <- c(results$B)
    names <- colnames(model.matrix(formula, data))
    return(setNames(B, names))
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

  get_plot = function() {}

)



# Assign :class:`LinearRegression` to generic functions.

residuals.LinearRegression <- function(x) x$get_residuals()

predict.LinearRegression <- function(x) x$get_predictions()

coef.LinearRegression <- function(x) x$get_named_coefs()

summary.LinearRegression <- function(x) x$get_summary()

plot.LinearRegression <- function(x) x$get_plot()








# Exemple d'ús:
# Defineix una fórmula i un conjunt de dades
data(iris)
lr <- linreg(Petal.Length~Species, data = iris)
