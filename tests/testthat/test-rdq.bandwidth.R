test_that("rdq.bandwidth input validation and fallback behavior works", {
  n = 500
  x = runif(n,min=-4,max=4)
  d = (x > 0)
  y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
  tlevel = seq(0.1,0.9,by=0.1)
  val <- seq(0.6, 1.2, by = 0.2)

  # cv=0 with provided hp
  out0 <- rdq.bandwidth(y, x, d, x0=0, z0=NULL, cv=0, val=val, hp=0.5)

  expect_s3_class(out0, "bw.qte")
  expect_true("opt.m" %in% names(out0))
  expect_true("opt.p" %in% names(out0))

  # Check truncation at max(val)
  maxval <- max(val)
  expect_true(all(out0$opt.m <= maxval))
  expect_true(all(out0$opt.p <= maxval))
})

test_that("rdq.bandwidth (with covariates) input validation and fallback behavior works", {
  n = 300
  x = runif(n,min=-4,max=4)
  d = (x > 0)
  z = sample(c(0,1),n,replace=TRUE)
  y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + d*z + rnorm(n)
  tlevel = seq(0.1,0.9,by=0.1)
  val <- seq(1, 3, by = 0.5)

  # cv=1
  out0 <- rdq.bandwidth(y=y,x=cbind(x,z),d=d,x0=0,z0=c(0,1),cv=1,val=val,bdy=1, p.order=1)

  expect_s3_class(out0, "bw.qte")
  expect_true("opt.m" %in% names(out0))
  expect_true("opt.p" %in% names(out0))

  # Check truncation at max(val)
  maxval <- max(val)
  expect_true(all(out0$opt.m <= maxval))
  expect_true(all(out0$opt.p <= maxval))
})




test_that("rdq.bandwidth input validation and fallback behavior works", {
  set.seed(9823)
  n <- 200
  x <- cbind(rnorm(n), rnorm(n))
  y <- x[,1] + 0.5 * x[,2] + rnorm(n)
  d <- as.integer(x[,1] > 0)
  x0 <- 0
  z0 <- matrix(c(-1, 0, 1), ncol = 1)
  z0 <- c(-1,0,1)
  #z0 <- -1
  val <- seq(0.5, 1.5, by = 0.5)

  # cv=0 with provided hp
  out0 <- rdq.bandwidth(y, x, d, x0, z0, cv = 0, val = val, hp = 0.5)
  expect_s3_class(out0, "bw.qte")
  expect_true("opt.m" %in% names(out0))
  expect_true("opt.p" %in% names(out0))

  # Check truncation at max(val)
  maxval <- max(val)
  expect_true(all(out0$opt.m <= maxval))
  expect_true(all(out0$opt.p <= maxval))
})


test_that("rdq.bandwidth returns valid structure for CV=1", {
  set.seed(123)
  n <- 200
  x <- cbind(rnorm(n), rnorm(n))
  y <- x[,1] + 0.5 * x[,2] + rnorm(n)
  d <- as.integer(x[,1] > 0)
  x0 <- 0
  z0 <- matrix(c(-1, 0, 1), ncol = 1)
  z0 <- c(-1,0,1)
  val <- seq(0.5, 1.5, by = 0.5)

  out1 <- rdq.bandwidth(y, x, d, x0, z0, cv = 1, val = val)

  expect_s3_class(out1, "bw.qte")
  expect_true(all(c("cv", "opt.m", "opt.p", "cov", "dg") %in% names(out1)))
})


