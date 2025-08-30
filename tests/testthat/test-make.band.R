make_test_data <- function(m, dg, n.sim) {
  array_dims <- c(dg, m, n.sim)
  list(
    Dc.p = array(rnorm(prod(array_dims)), dim = array_dims),
    Dc.m = array(rnorm(prod(array_dims)), dim = array_dims),
    Dr.p = array(rnorm(prod(array_dims)), dim = array_dims),
    Dr.m = array(rnorm(prod(array_dims)), dim = array_dims),
    Qy.p = matrix(rnorm(m * dg), nrow = m),
    Qy.m = matrix(rnorm(m * dg), nrow = m),
    bias.p = matrix(rnorm(m * dg), nrow = m),
    bias.m = matrix(rnorm(m * dg), nrow = m)
  )
}

test_that("make.band returns correct structure and dimensions", {
  m <- 5; dg <- 3; n.sim <- 200
  taus <- seq(0.1, 0.9, length.out = m)
  dat <- make_test_data(m, dg, n.sim)

  out <- make.band(n.sam = 100, dat$Dc.p, dat$Dc.m, dat$Dr.p, dat$Dr.m,
                   dz = 1, cov = 1, taus = taus, hh = 0.5,
                   Qy.p = dat$Qy.p, Qy.m = dat$Qy.m,
                   bias.p = dat$bias.p, bias.m = dat$bias.m,
                   alpha = 0.1, n.sim = n.sim)

  expect_type(out, "list")
  expect_equal(dim(out$qte), c(m, dg))
  expect_equal(dim(out$qte.r), c(m, dg))
  expect_equal(dim(out$uband), c(m, 2, dg))
  expect_equal(dim(out$uband.r), c(m, 2, dg))
  expect_equal(dim(out$s), c(m, dg))
  expect_equal(dim(out$s.r), c(m, dg))
})


test_that("make.band handles dz = 0 correctly", {
  m <- 4; dg <- 1; n.sim <- 100
  taus <- seq(0.1, 0.9, length.out = m)
  dat <- make_test_data(m, dg, n.sim)

  out <- make.band(n.sam = 50, dat$Dc.p, dat$Dc.m, dat$Dr.p, dat$Dr.m,
                   dz = 0, cov = 0, taus = taus, hh = 0.5,
                   Qy.p = dat$Qy.p, Qy.m = dat$Qy.m,
                   bias.p = dat$bias.p, bias.m = dat$bias.m,
                   alpha = 0.05, n.sim = n.sim)

  expect_equal(dim(out$qte), c(m, 1))
})

