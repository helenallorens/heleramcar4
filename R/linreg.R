linreg <- function(formula, data){
  
  x <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]
  beta <- as.vector(solve((t(x) %*% x)) %*% t(x) %*% y)
  fitted_values <- x %*% beta
  residuals <- y - fitted_values
  df <- nrow(x) - ncol(x)
  sigma2 <- as.numeric((t(residuals) %*% residuals)/df)
  var <- sigma2 * solve((t(x) %*% x))
  se_beta <- sqrt(diag(var))
  t <- beta/se_beta
  p_values <- 2 * pt(abs(t), df, lower.tail = FALSE)
  
  result <- list(
    coefficients = beta,
    fitted_values = fitted_values,
    residuals = residuals,
    df_residual = df,
    sigma2 = sigma2,
    var_beta = var,
    t_values = t,
    p_values = p_values
  )
  
  class(result) <- "linreg"
  
  return(result)
}

linreg(Petal.Length~Species, data = iris)
