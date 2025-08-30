#' Conditional density estimation
#' @description
#' \code{rdq.condf} estimates conditional density functions by using the differencing method.
#'
#' @param x a vector (or a matrix) of covariates.
#' @param Q a vector of estimated conditional quantiles.
#' @param bcoe quantile regression coefficient estimates.
#' @param taus a vector of quantiles of interest.
#' @param taul a vector of quantiles used for the conditional density estimation.
#' It is needed to estimate the tail parts of conditional density functions more precisely.
#' @param delta bandwidths for estimating the conditional density.
#' @param cov either 0 or 1. Set `cov=1` if covariates are present in the model;
#' otherwise set `cov=0`.
#'
#' @return conditional density function estimates
#' @export
#'
#' @examples
#' n = 500
#' x = runif(n,min=-4,max=4)
#' d = (x > 0)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' tlevel = seq(0.1,0.9,by=0.1)
#' hh = rep(2,length(tlevel))
#'
#' ab = rdq(y=y,x=x,d=d,x0=0,z0=NULL,tau=tlevel,h.tau=hh,cov=0)
#' delta = 0.186
#' fe = rdq.condf(x=x,Q=ab$qp.est,bcoe=ab$bcoe.p,taus=0.5,taul=tlevel,delta=delta,cov=0)
#'
#'
rdq.condf <- function(x, Q, bcoe, taus, taul, delta, cov){
  x <- as.matrix(x)
  n <- nrow(x)
  m <- length(taus)
  if(cov==0){
    Qh <- sort(Q)
    Q.u <- approx(x=taul,y=Qh,xout=(taus+delta),method="linear",rule=2)$y
    Q.l <- approx(x=taul,y=Qh,xout=(taus-delta),method="linear",rule=2)$y
    ffa <- pmax(0,(2*delta)/(Q.u-Q.l-0.01))
    ff  <- rep(1,n) %o% ffa
  }
  if(cov==1){
    w <- as.matrix(x[,-1])
    ff <- array(0,c(n,m))
    for(i in 1:n){
      Qh <- sort(bcoe %*% c(1,w[i,]))
      Q.u <- approx(x=taul,y=Qh,xout=(taus+delta),method="linear",rule=2)$y
      Q.l <- approx(x=taul,y=Qh,xout=(taus-delta),method="linear",rule=2)$y
      ff[i,] <- pmax(0,(2*delta)/(Q.u-Q.l-0.01))
    }
  }
  return(list(ff = ff))
}
