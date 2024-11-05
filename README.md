<!-- badges: start -->
  [![R-CMD-check](https://github.com/helenallorens/heleramcar4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/helenallorens/heleramcar4/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# Linear and Ridge Regression RC Classes

This repository provides two Reference Classes in R: `linreg` and `ridgereg`, designed for easy implementation of linear and ridge regression models. The `linreg` class provides standard linear regression capabilities, while `ridgereg` offers regularized regression through ridge regression.

## Overview

- **`linreg`**: A linear regression class that estimates the relationship between variables using ordinary least squares.
- **`ridgereg`**: A ridge regression class that incorporates a regularization parameter to handle multicollinearity and improve model generalization.

## Installation

Clone this repository to get started:

```r
# Clone the repository
# Use the classes by sourcing them directly or adding them to your project.
```

## Usage

The primary classes are `linreg` for linear regression and `ridgereg` for ridge regression. Here’s how to initialize and use each:

### Linear Regression (`linreg`)

```r
# Example with iris dataset
lr <- linreg(Petal.Length ~ Species, data = iris)

# Access coefficients
lr$coef()

# Get predictions
pred(lr)

# Model summary
summary(lr)
```

### Ridge Regression (`ridgereg`)

```r
# Example with iris dataset and lambda = 1
ridge <- ridgereg(Petal.Length ~ Species, data = iris, lambda = 1)

# Access coefficients
ridge$coef()

# Get predictions
pred(ridge)

# Model summary
summary(ridge)
```


## License

This project is open-source and available under the [MIT License](LICENSE).
