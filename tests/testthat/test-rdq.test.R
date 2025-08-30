test_that("rdq.test input validation works", {
  n <- 200
  x <- cbind(rnorm(n), rnorm(n))
  y <- x[,1] + 0.5 * x[,2] + rnorm(n)
  d <- as.integer(x[,1] > 0)
  x0 <- 0
  z0 <- matrix(c(-1, 0, 1), ncol = 1)
  tau <- 0.5
  bdw <- 0.1
  alpha <- 0.05

  # Missing alpha
  expect_error(rdq.test(y, x, d, x0, z0, tau, bdw, bias = TRUE, alpha = NULL, type = 1),
               "Provide values for alpha")

  # Missing type
  expect_error(rdq.test(y, x, d, x0, z0, tau, bdw, bias = TRUE, alpha = 0.05, type = NULL),
               "option type cannot be empty")

  # bdw length mismatch
  expect_error(rdq.test(y, x, d, x0, z0, tau = c(0.25, 0.5), bdw = c(0.1, 0.2, 0.3),
                        bias = TRUE, alpha = 0.05, type = 1),
               "length of bdw")
})


test_that("rdq.test returns valid output structure", {
  n <- 200
  x <- cbind(rnorm(n), rnorm(n))
  y <- x[,1] + 0.5 * x[,2] + rnorm(n)
  y <- as.vector(y)
  d <- as.vector(x[,1] > 0)
  x0 <- 0
  z0 <- matrix(c(-1, 0, 1), ncol = 1)
  z0 <- c(-1,0,1)
  out <- rdq.test(y, x, d, x0, z0, tau = c(0.5,0.8), bdw = 0.3, bias = TRUE,
                  alpha = 0.05, type = c(1, 2))

  expect_s3_class(out, "test.qte")
  expect_named(out, c("statistic", "cr.value", "p.value", "type", "cov", "dg", "alpha"))
  expect_named(out$statistic, c("significance", "homogeneity"))
  expect_type(out$p.value$significance, "double")
})


test_that("rdq.test returns valid output structure for a single quantile level", {
  n <- 200
  x <- cbind(rnorm(n), rnorm(n))
  y <- x[,1] + 0.5 * x[,2] + rnorm(n)
  y <- as.vector(y)
  d <- as.vector(x[,1] > 0)
  x0 <- 0
  z0 <- matrix(c(-1, 0, 1), ncol = 1)
  z0 <- c(-1,0,1)
  out <- rdq.test(y, x, d, x0, z0, tau = c(0.5), bdw = 0.3, bias = TRUE,
                  alpha = 0.05, type = c(1, 2))

  expect_s3_class(out, "test.qte")
  expect_named(out, c("statistic", "cr.value", "p.value", "type", "cov", "dg", "alpha"))
  expect_named(out$statistic, c("significance", "homogeneity"))
  expect_type(out$p.value$significance, "double")
})

