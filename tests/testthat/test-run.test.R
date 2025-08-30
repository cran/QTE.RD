test_that("run.test produces correct structure and types", {
  # Simulation setup

  taus <- c(0.25, 0.5, 0.75)
  hh <- 0.2
  alpha <- c(0.05, 0.1)
  m <- length(taus)               # number of quantiles
  dg <- 2              # number of groups
  n.sim <- 100
  n <- 500

  Qy.p <- matrix(rnorm(m * dg), m, dg)
  Qy.m <- matrix(rnorm(m * dg), m, dg)
  bias.p <- matrix(runif(m * dg, 0, 0.05), m, dg)
  bias.m <- matrix(runif(m * dg, 0, 0.05), m, dg)
  Dc.p <- array(rnorm(dg * m * n.sim), dim = c(dg, m, n.sim))
  Dc.m <- array(rnorm(dg * m * n.sim), dim = c(dg, m, n.sim))
  Dr.p <- array(rnorm(dg * m * n.sim), dim = c(dg, m, n.sim))
  Dr.m <- array(rnorm(dg * m * n.sim), dim = c(dg, m, n.sim))

  for (test.type in 1:4) { # set 'std.opt = 1'
    out <- run.test(n.sam = n, dz = 1, taus = taus, hh = hh,
                    Dc.p = Dc.p, Dc.m = Dc.m, Dr.p = Dr.p, Dr.m = Dr.m,
                    Qy.p = Qy.p, Qy.m = Qy.m, bias.p = bias.p, bias.m = bias.m,
                    cov = 1, bias = 1, alpha = alpha, n.sim = n.sim,
                    test.type = test.type, std.opt = 1)

    expect_type(out, "list")
    expect_named(out, c("test.stat", "cr.value", "p.val"))
    expect_length(out$test.stat, dg)
    expect_equal(dim(out$cr.value), c(dg, length(alpha)))
    expect_length(out$p.val, dg)
    expect_true(all(out$p.val >= n.sim^{-1}))  # p-values are bounded below
  }

  for (test.type in 1:4) { # set 'std.opt = 0'
    out2 <- run.test(n.sam = n, dz = 1, taus = taus, hh = hh,
                    Dc.p = Dc.p, Dc.m = Dc.m, Dr.p = Dr.p, Dr.m = Dr.m,
                    Qy.p = Qy.p, Qy.m = Qy.m, bias.p = bias.p, bias.m = bias.m,
                    cov = 1, bias = 1, alpha = alpha, n.sim = n.sim,
                    test.type = test.type, std.opt = 0)

    expect_type(out2, "list")
    expect_named(out2, c("test.stat", "cr.value", "p.val"))
    expect_length(out2$test.stat, dg)
    expect_equal(dim(out2$cr.value), c(dg, length(alpha)))
    expect_length(out2$p.val, dg)
    expect_true(all(out2$p.val >= n.sim^{-1}))  # p-values are bounded below
  }
})


test_that("run.test works with dz = 0 (no covariates)", {
  set.seed(456)
  m <- 2
  n.sim <- 500
  n <- 300

  Qy.p <- matrix(rnorm(m), m, 1)
  Qy.m <- matrix(rnorm(m), m, 1)
  bias.p <- matrix(runif(m, 0, 0.05), m, 1)
  bias.m <- matrix(runif(m, 0, 0.05), m, 1)
  Dc.p <- array(rnorm(m * n.sim), dim = c(m, n.sim))
  Dc.m <- array(rnorm(m * n.sim), dim = c(m, n.sim))
  Dr.p <- array(rnorm(m * n.sim), dim = c(m, n.sim))
  Dr.m <- array(rnorm(m * n.sim), dim = c(m, n.sim))

  taus <- c(0.4, 0.6)
  hh <- 0.15
  alpha <- c(0.05)

  out <- run.test(n.sam = n, dz = 0, taus = taus, hh = hh,
                  Dc.p = Dc.p, Dc.m = Dc.m, Dr.p = Dr.p, Dr.m = Dr.m,
                  Qy.p = Qy.p, Qy.m = Qy.m, bias.p = bias.p, bias.m = bias.m,
                  cov = 0, bias = 0, alpha = alpha, n.sim = n.sim,
                  test.type = 2, std.opt = 0)

  #expect_s3_class(out$cr.value, "matrix")
  expect_equal(length(out$p.val), 1)
})


test_that("run.test handles scalar vs vector cases in hh and std.opt", {
  # Same data as earlier
  set.seed(789)
  m <- 3; dg <- 1; n.sim <- 250; n <- 300

  Qy.p <- matrix(rnorm(m), m, 1)
  Qy.m <- matrix(rnorm(m), m, 1)
  bias.p <- matrix(runif(m, 0, 0.05), m, 1)
  bias.m <- matrix(runif(m, 0, 0.05), m, 1)
  Dc.p <- array(rnorm(m * n.sim), dim = c(1, m, n.sim))
  Dc.m <- array(rnorm(m * n.sim), dim = c(1, m, n.sim))
  Dr.p <- array(rnorm(m * n.sim), dim = c(1, m, n.sim))
  Dr.m <- array(rnorm(m * n.sim), dim = c(1, m, n.sim))

  taus <- c(0.25, 0.5, 0.75)
  hh <- rep(0.2, m)
  alpha <- 0.1

  # Should work even if hh is vector and std.opt = 1
  expect_silent({
    out <- run.test(n.sam = n, dz = 1, taus = taus, hh = hh,
                    Dc.p = Dc.p, Dc.m = Dc.m, Dr.p = Dr.p, Dr.m = Dr.m,
                    Qy.p = Qy.p, Qy.m = Qy.m, bias.p = bias.p, bias.m = bias.m,
                    cov = 1, bias = 0, alpha = alpha, n.sim = n.sim,
                    test.type = 1, std.opt = 1)
  })
})
