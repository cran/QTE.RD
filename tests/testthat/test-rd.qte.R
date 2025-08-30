test_that("rd.qte returns a qte object with no covariates, no bias", {
  set.seed(123)
  y <- rnorm(200)
  x <- matrix(runif(200), ncol=1)
  d <- ifelse(x > 0.5, 1, 0)
  result <- rd.qte(y, x, d, x0=0.5, tau=c(0.25, 0.5, 0.75), bdw=0.4, bias=0)

  expect_s3_class(result, "qte")
  expect_named(result, c("tau", "qte", "qp.est", "qm.est", "cov", "y", "x", "d", "x0", "z0", "bdw", "bias"), ignore.order = TRUE)
  expect_equal(result$cov, 0)
})


test_that("rd.qte handles bias correction", {
  y <- rnorm(200)
  x <- matrix(runif(200), ncol=1)
  d <- ifelse(x > 0.5, 1, 0)
  result <- rd.qte(y, x, d, x0=0.5, tau=c(0.5), bdw=0.3, bias=1)

  expect_true(all(c("qp.est", "qm.est", "qte") %in% names(result)))
})


test_that("rd.qte produces error for mismatched bandwidths and taus", {
  expect_error(
    rd.qte(rnorm(100), matrix(runif(100), ncol=1), ifelse(runif(100) > 0.5, 1, 0), x0=0.5,
           tau=c(0.1, 0.5), bdw=c(0.1, 0.2, 0.3), bias=0),
    "length of bdw should be one or equal to the length of tau"
  )
})

