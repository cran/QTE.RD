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

test_that("make.band.cq returns correct structure and dimensions", {
  m <- 6; dg <- 2; n.sim <- 100
  taus <- seq(0.1, 0.9, length.out = m)
  dat <- make_test_data(m, dg, n.sim)

  out <- make.band.cq(n.sam = 120, dat$Dc.p, dat$Dc.m, dat$Dr.p, dat$Dr.m,
                      dz = 1, cov = 1, taus = taus, hh = 0.3,
                      Qy.p = dat$Qy.p, Qy.m = dat$Qy.m,
                      bias.p = dat$bias.p, bias.m = dat$bias.m,
                      alpha = 0.1, n.sim = n.sim)

  expect_equal(dim(out$qp), c(m, dg))
  expect_equal(dim(out$qm), c(m, dg))
  expect_equal(dim(out$qp.r), c(m, dg))
  expect_equal(dim(out$qm.r), c(m, dg))
  expect_equal(dim(out$ubandp), c(m, 2, dg))
  expect_equal(dim(out$ubandp.r), c(m, 2, dg))
  expect_equal(dim(out$ubandm), c(m, 2, dg))
  expect_equal(dim(out$ubandm.r), c(m, 2, dg))
  expect_equal(dim(out$sp), c(m, dg))
  expect_equal(dim(out$sp.r), c(m, dg))
  expect_equal(dim(out$sm), c(m, dg))
  expect_equal(dim(out$sm.r), c(m, dg))
})
