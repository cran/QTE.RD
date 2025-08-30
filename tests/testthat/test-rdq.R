test_that("rdq works without covariates", {
  y <- rnorm(100)
  x <- matrix(runif(100), ncol=1)
  d <- ifelse(x > 0.5, 1, 0)
  result <- rdq(y, x, d, x0=0.5, tau=c(0.5), h.tau=0.5, cov=0)

  expect_true(all(c("qte", "qp.est", "qm.est", "bcoe.p", "bcoe.m") %in% names(result)))
})


test_that("rdq works with one covariate", {
  y <- rnorm(100)
  x <- cbind(runif(100), rnorm(100))
  d <- ifelse(x[, 1] > 0.5, 1, 0)
  z0 <- seq(-1, 1, length.out=3)
  result <- rdq(y, x, d, x0=0.5, z0=z0, tau=c(0.5), h.tau=0.5, cov=1)

  expect_equal(dim(result$qp.est), c(1, length(z0)))
})

