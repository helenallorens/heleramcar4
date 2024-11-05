library(MASS)

test_that("ridreg and lm.ridge yield same results", {

  df <- data.frame(x2 = c(1, 2, 3, 4, 5), x1 = c(1, 2, 3, 4, 5), y = c(2, 4, 6, 8, 10))

  lmrid <- lm.ridge(y ~ x1 + x2, data = df, lambda = 1)
  rid <- ridgereg(y ~ x1 + x2, data = df, lambda = 1)

  expect_equal(
    as.numeric(coef(lmrid)),
    as.numeric(coef(rid))
  )
})
