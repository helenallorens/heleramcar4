# Carrega el paquet ggplot2
library(ggplot2)

# Defineix la classe linreg com una classe RC (Reference Class)
linreg <- setRefClass(
  "linreg",
  fields = list(
    formula = "formula",
    data = "data.frame",
    coefficients = "numeric",
    fitted_values = "numeric",
    residuals = "numeric",
    df_residual = "numeric",
    sigma2 = "numeric",
    var_beta = "matrix",
    t_values = "numeric",
    p_values = "numeric"
  ),

  methods = list(
    # Mètode per inicialitzar l'objecte
    initialize = function(formula, data) {
      x <- model.matrix(formula, data)
      y <- data[[all.vars(formula)[1]]]

      beta <- as.vector(solve(t(x) %*% x) %*% t(x) %*% y)
      fitted_values <<- x %*% beta
      residuals <<- y - fitted_values
      df_residual <<- nrow(x) - ncol(x)
      sigma2 <<- as.numeric((t(residuals) %*% residuals) / df_residual)
      var_beta <<- sigma2 * solve(t(x) %*% x)
      se_beta <- sqrt(diag(var_beta))
      t_values <<- beta / se_beta
      p_values <<- 2 * pt(abs(t_values), df_residual, lower.tail = FALSE)

      coefficients <<- beta
    },

    # Mètode print per imprimir els coeficients
    print = function() {
      cat("Coefficients:\n")
      coef_names <- names(coef())
      coef_table <- cbind(coef_names, coefficients)
      print(coef_table)
    },

    # Mètode per retornar els coeficients com a vector amb noms
    coef = function() {
      beta_names <- colnames(model.matrix(formula, data))
      setNames(coefficients, beta_names)
    },

    # Mètode per retornar els residus
    resid = function() {
      return(residuals)
    },

    # Mètode per retornar els valors predits
    pred = function() {
      return(fitted_values)
    },

    # Mètode per generar gràfiques
    plot = function() {
      # Gràfica de residus vs. valors ajustats
      p1 <- ggplot(data, aes(x = fitted_values, y = residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(x = "Fitted Values", y = "Residuals") +
        ggtitle("Residuals vs Fitted")

      # Gràfica de residus en funció de la mediana
      p2 <- ggplot(data, aes(sample = residuals)) +
        stat_qq() +
        stat_qq_line() +
        ggtitle("Normal Q-Q")

      # Mostra les dues gràfiques
      print(p1)
      print(p2)
    },

    # Mètode summary per imprimir el resum del model
    summary = function() {
      se_beta <- sqrt(diag(var_beta))
      coef_table <- cbind(coefficients, se_beta, t_values, p_values)
      colnames(coef_table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

      cat("Coefficients:\n")
      print(coef_table)
      cat("\nResidual standard error:", sqrt(sigma2), "on", df_residual, "degrees of freedom\n")
    }
  )
)

# Exemple d'ús:
# Defineix una fórmula i un conjunt de dades
data(mtcars)
model <- linreg$new(mpg ~ wt + hp, data = mtcars)

# Mostra el resum del model
model$summary()

# Imprimeix els coeficients
model$print()

# Retorna els coeficients
model$coef()

# Retorna els residus
model$resid()

# Retorna els valors predits
model$pred()

# Mostra les gràfiques
model$plot()

