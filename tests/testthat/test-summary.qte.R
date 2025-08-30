test_that("summary.qte produces summary structure", {
  y <- rnorm(100)
  x <- matrix(runif(100), ncol=1)
  d <- ifelse(x > 0.5, 1, 0)
  obj <- rd.qte(y, x, d, x0=0.5, tau=c(0.5), bdw=0.2, bias=0)
  s <- summary(obj, alpha=0.1)

  expect_s3_class(s, "summary.qte")
  expect_true("uband" %in% names(s))
})


test_that("summary.qte errors when alpha is missing", {
  y <- rnorm(100)
  x <- matrix(runif(100), ncol=1)
  d <- ifelse(x > 0.5, 1, 0)
  obj <- rd.qte(y, x, d, x0=0.5, tau=c(0.5), bdw=0.2, bias=0)

  expect_error(summary(obj, alpha=numeric(0)), "Provide alpha")
})


