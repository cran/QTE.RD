test_that("rdq.sim returns correctly dimensioned arrays", {
  x <- matrix(runif(100), ncol = 1)
  d <- ifelse(x > 0.5, 1, 0)
  fxp <- fxm <- matrix(1, 100, 2)
  res <- rdq.sim(x, d, x0 = 0.5, z0 = NULL, dz = 0, cov = 0,
                 tt = c(0.25, 0.75), hh = rep(0.2, 2), hh2 = rep(0.3, 2),
                 fxp = fxp, fxm = fxm, n.sim = 10)
  expect_equal(dim(res$dcp), c(1, 2, 10))
  expect_equal(dim(res$drm), c(1, 2, 10))
})

