test_that("rdq.bias returns bias and bhat with correct dimensions", {
  y <- rnorm(100)
  x <- matrix(runif(100), ncol=1)
  fx <- matrix(1, 100, 1)
  result <- rdq.bias(y, x, dz = 0, x0 = 0.5, z0 = NULL, taus = c(0.25, 0.5, 0.75), h.tau = rep(0.2, 3), h.tau2 = rep(0.2, 3), fx = fx, cov = 0)
  expect_named(result, c("bias", "bhat"))
  expect_equal(dim(result$bias), c(3, 1))
})

