library(ggplot2)


#' Performs Linear Regression model.
#'
#' @param formula
#' @param data
#'
#' @return LinearRegression
#' @export
#'
#' @examples
#'
linreg <- function(formula, data) {

  lr <- LinearRegression$new(formula = formula, data = data)
  lr$fit()
  return(lr)

}

FittedParams <- setRefClass(
  "FittedParams",

  fields = list(
    coefficients = "numeric",
    fitted_values = "matrix",
    residuals = "matrix",
    df_residual = "numeric",
    sigma2 = "numeric",
    var_beta = "matrix",
    t_values = "numeric",
    p_values = "numeric"
    )
)


LinearRegression <- setRefClass(
  "LinearRegression",
  fields = list(formula = "formula", data = "data.frame", fitted_params = "FittedParams")
)



LinearRegression$methods(



    fit = function() {

      X <- model.matrix(formula, data)
      y <- data[[all.vars(formula)[1]]]

      beta <- as.vector(solve(t(X) %*% X) %*% t(X) %*% y)
      fitted_values <- X %*% beta
      residuals <- y - fitted_values
      df_residual <- nrow(X) - ncol(X)
      sigma2 <- as.numeric((t(residuals) %*% residuals) / df_residual)
      var_beta <- sigma2 * solve(t(X) %*% X)
      se_beta <- sqrt(diag(var_beta))
      t_values <- beta / se_beta
      p_values <- 2 * pt(abs(t_values), df_residual, lower.tail = FALSE)
      coefficients <- beta


      fitted_params <<- FittedParams$new(
        coefficients = coefficients,
        fitted_values = fitted_values,
        residuals = residuals,
        df_residual = df_residual,
        sigma2 = sigma2,
        var_beta = var_beta,
        t_values = t_values,
        p_values = p_values
      )
    },

    coef = function() {
      V <- c(lr$fitted_params$coefficients)
      names <- colnames(model.matrix(formula, data))
      return(setNames(V, names))
    },

   print = function() {
     cat("Coefficients: \n")
     return(coef())

   },

   show = function() {
     return(coef())
   }

)




# Exemple d'ús:
# Defineix una fórmula i un conjunt de dades
data(iris)
lr <- linreg(Petal.Length~Species, data = iris)
