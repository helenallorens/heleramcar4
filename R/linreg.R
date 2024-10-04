library(ggplot2)

#' Generic function `pred`
pred <- function(object, ...) UseMethod("pred")

#' linreg Results
#'
#' @field B numeric. Regressions coefficients
#' @field yhat matrix. The fitted values
#' @field residuals matrix. Residuals
#' @field residuals_variance numeric. The residuals variance
#' @field degrees_of_freedom numeric. Degrees of freedom
#' @field B_variance matrix. The variance of the regression coefficients
#' @field t_values numeric. The t-values for each coefficient
#' @field p_values numeric. The p-values for each coefficient
linregResults <- setRefClass(
  "linregResults",

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
#' @return linreg
#' @examples
#' data(iris)
#' lr <- linreg$new(Petal.Length~Species, data = iris)
#' @export
linreg <- setRefClass(
  "linreg",
  fields = list(
    formula = "formula",
    data = "data.frame",
    results = "linregResults",
    name = "ANY"
  )
)

linreg$methods(

  initialize = function(formula, data) {

    .self$formula <- formula
    .self$data <- data
    .self$name <- deparse(substitute(data))

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
    results <<- linregResults$new(
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

  print = function() .self$show(),

  show = function() {
    cat("\nCall:\n")
    cat("linreg(formula = ", deparse(.self$formula), ", data = ", .self$name, ")\n\n", sep = "")
    cat("Coefficients:\n")
    base::print(.self$coef())
  },


  resid = function() .self$results$residuals,

  pred = function() .self$results$yhat,

  coef = function() {
    B <- c(results$B)
    names <- colnames(model.matrix(formula, data))
    return(setNames(B, names))
  },

  summary = function()  {

    digits <- 3
    rdf <- .self$results$degrees_of_freedom
    rse <- sqrt(.self$results$residuals_variance)


    # Generate summ data.frame.
    summ <- data.frame(
      A=.self$results$B,
      B=.self$results$B_se,
      C=.self$results$t_values,
      D=.self$results$p_values
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

  plot = function() {
    aes1 <- aes(x = .self$pred(), y = .self$resid())
    p1 <- (
      ggplot(data = data, mapping = aes1) +
      geom_point() +
      stat_summary(fun = median, geom = "line", color = "red")+
      labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")
    )
    base::print(p1)
  }
)

# ----------------------------------------------------------------
# Adding methods for :class:`linreg` to existing generic functions
# ----------------------------------------------------------------

#' Returns residuals for :class:`linreg` object.
#' @export
residuals.linreg <- function(object, ...) object$resid()

#' Returns fitted values for :class:`linreg` object.
#' @export
pred.linreg <- function(object, ...) object$pred()

#' Returns estimated coefficients for :class:`linreg` object.
#' @export
coef.linreg <- function(object, ...) object$coef()

#' Returns summary for :class:`linreg` object.
#' @export
summary.linreg <- function(object, ...) object$summary()

#' Returns plot for :class:`linreg` object.
#' @export
plot.linreg <- function(object, ...) object$plot()
