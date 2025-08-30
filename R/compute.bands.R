#' Uniform confidence bands for QTE
#' @description
#' \code{compute.bands} is used to construct uniform confidence bands.
#' The function \code{make.band} calls this function to construct uniform bands.
#' @keywords internal
#'
compute.bands <- function(G, Q, n.sim, n.sam, hh, alpha){
  m_len <- length(Q)
  if (m_len == 1) {
    shat <- sqrt(mean(G^2))
  } else {
    shat <- sqrt(rowMeans(G^2))
  }
  ra <- G / matrix(rep(shat, n.sim), ncol = n.sim)
  rs <- apply(abs(ra), 2, max)
  cp <- quantile(rs, probs = 1 - alpha)
  sigma <- shat/sqrt(n.sam * hh)
  band <- cbind(Q - cp * sigma, Q + cp * sigma)
  list(band = band, sigma = sigma)
}

