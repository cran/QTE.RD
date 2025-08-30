make_mock_qte_uncond <- function() {
  tau <- seq(0.1, 0.9, length.out = 5)
  qte <- runif(length(tau))
  uband <- array(c(qte - 0.1, qte + 0.1), dim = c(length(tau), 2, 1))
  structure(list(qte = qte, uband = uband, tau = tau, cov = 0),
            class = "summary.qte")
}

make_mock_qte_group <- function(dg = 2) {
  tau <- seq(0.1, 0.9, length.out = 5)
  qte <- matrix(runif(length(tau) * dg), ncol = dg)
  uband <- array(0, dim = c(length(tau), 2, dg))
  for (i in 1:dg) {
    uband[, , i] <- cbind(qte[, i] - 0.1, qte[, i] + 0.1)
  }
  structure(list(qte = qte, uband = uband, tau = tau, cov = 1),
            class = "summary.qte")
}

make_mock_qte_conditional <- function(dg = 1) {
  tau <- seq(0.1, 0.9, length.out = 5)
  qp <- matrix(runif(length(tau) * dg), ncol = dg)
  qm <- matrix(runif(length(tau) * dg), ncol = dg)
  uband.p <- array(0, dim = c(length(tau), 2, dg))
  uband.m <- array(0, dim = c(length(tau), 2, dg))
  for (i in 1:dg) {
    uband.p[, , i] <- cbind(qp[, i] - 0.1, qp[, i] + 0.1)
    uband.m[, , i] <- cbind(qm[, i] - 0.1, qm[, i] + 0.1)
  }
  structure(list(qp = qp, qm = qm, uband.p = uband.p, uband.m = uband.m,
                 tau = tau, cov = ifelse(dg == 1, 0, 1)),
            class = "summary.qte")
}

test_that("ptype argument must be 1 or 2", {
  expect_error(plot.qte(make_mock_qte_uncond(), ptype = 3),
               "The option 'ptype' should be either 1 or 2.")
})

test_that("plot.qte runs silently for unconditional QTE, cov = 0", {
  expect_silent(plot.qte(make_mock_qte_uncond(), ptype = 1))
})

test_that("plot.qte runs silently for group QTE, cov = 1", {
  expect_silent(plot.qte(make_mock_qte_group(dg = 2), ptype = 1))
})

test_that("plot.qte runs silently for conditional QTE, cov = 0", {
  expect_silent(plot.qte(make_mock_qte_conditional(dg = 1), ptype = 2))
})

test_that("plot.qte runs silently for conditional QTE, cov = 1, dg = 3", {
  expect_silent(plot.qte(make_mock_qte_conditional(dg = 3), ptype = 2))
})

test_that("no bands plotted when class is not summary.qte", {
  obj <- make_mock_qte_uncond()
  class(obj) <- "qte"
  expect_silent(plot.qte(obj, ptype = 1))
})

test_that("group layouts render correctly for varying group sizes", {
  for (g in c(2, 3, 5, 9)) {
    expect_silent(plot.qte(make_mock_qte_group(dg = g), ptype = 1))
  }
})
