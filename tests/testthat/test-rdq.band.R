test_that("rdq.band returns expected structure for minimal input", {
  set.seed(123)
  n <- 100
  x <- runif(n, -1, 1)
  d <- as.numeric(x >= 0)
  y <- 1 + 2*d + x + rnorm(n)
  result <- rdq.band(y=y, x=x, d=d, x0=0, tau=c(0.25, 0.5, 0.75), bdw=0.3, alpha=0.05)

  expect_s3_class(result, "band.qte")
  expect_named(result, c("qte", "qte.cor", "uband", "uband.robust",
                         "sig", "sig.r", "uband.p", "uband.m",
                         "uband.robust.p", "uband.robust.m", "tau", "alpha", "cov"), ignore.order = TRUE)
})


test_that("rdq.band throws error if alpha is missing", {
  expect_error(rdq.band(y = rnorm(10), x = rnorm(10), d = rep(0:1, 5),
                        x0 = 0, tau = 0.5, bdw = 0.3, alpha = NULL),
               "Provide alpha")
})


test_that("rdq.band throws error for mismatched bdw and tau lengths", {
  expect_error(rdq.band(y = rnorm(10), x = rnorm(10), d = rep(0:1, 5),
                        x0 = 0, tau = c(0.25, 0.5), bdw = c(0.3, 0.4, 0.5), alpha = 0.05),
               "The length of bdw should be one or equal to the length of tau")
})


test_that("rdq.band works with one covariate", {
  set.seed(3211)
  n <- 100
  x1 <- runif(n, -1, 1)
  x2 <- rnorm(n)
  d <- as.numeric(x1 >= 0)
  x <- cbind(x1, x2)
  y <- 1 + 2*d + 0.5*x2 + x1 + rnorm(n)
  z0 <- mean(x2)

  result <- rdq.band(y=y, x=x, d=d, x0=0, z0=z0, tau=c(0.25, 0.5, 0.75), bdw=0.5, alpha=0.05)

  expect_type(result$qte, "double")
  expect_length(result$tau, 3)
})


test_that("rdq.band handles NA values by removing them", {
  set.seed(42)
  n <- 100
  x <- runif(n, -1, 1)
  d <- as.numeric(x >= 0)
  y <- 1 + 2*d + x + rnorm(n)
  y[1] <- NA
  d[2] <- NA
  x[3] <- NA

  result <- rdq.band(y=y, x=x, d=d, x0=0, tau=0.5, bdw=0.3, alpha=0.1)
  expect_s3_class(result, "band.qte")
})


test_that("rdq.band works with one covariate and several evaluation points", {
  set.seed(3211)
  n <- 100
  x1 <- runif(n, -1, 1)
  x2 <- rnorm(n)
  d <- as.numeric(x1 >= 0)
  x <- cbind(x1, x2)
  y <- 1 + 2*d + 0.5*x2 + x1 + rnorm(n)
  z0 <- mean(x2) + c(-0.2,0)

  result <- rdq.band(y=y, x=x, d=d, x0=0, z0=z0, tau=c(0.25, 0.5, 0.75), bdw=0.5, alpha=0.05)

  expect_type(result$qte, "double")
  expect_equal(ncol(result$qte), length(z0))
})

