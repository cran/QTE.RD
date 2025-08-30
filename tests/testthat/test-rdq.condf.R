test_that("rdq.condf output is non-negative", {
  x <- matrix(runif(100), ncol = 1)
  #x <- runif(100)
  Q <- sort(runif(5))
  bcoe <- matrix(runif(5), nrow = 1)
  delta <- 0.01
  res <- rdq.condf(x, Q, bcoe, taus = c(0.3), taul = seq(0.1, 0.5, by = 0.1), delta, cov = 0)
  expect_true(all(res$ff >= 0))
})

