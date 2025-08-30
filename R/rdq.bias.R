#' Bias estimation
#' @description
#' \code{rdq.bias} estimates the bias terms using the local quadratic quantile regression.
#'
#' @param y a numeric vector, the outcome variable.
#' @param x a vector (or a matrix) of covariates, the first column is the running variable.
#' @param dz the number of covariates.
#' @param x0 the cutoff point.
#' @param z0 the value of the covariates at which to evaluate the effects.
#' @param taus a vector of quantiles of interest.
#' @param h.tau the bandwidth values (specified for each quantile level), for estimating conditional quantiles.
#' @param h.tau2 the bandwidth values for the local quadratic quantile regression, for estimating the bias terms.
#' @param fx conditional density estimates.
#' @param cov either 0 or 1. Set `cov=1` if covariates are present in the model;
#' otherwise set `cov=0`.
#'
#' @return A list with elements:
#' \describe{
#' \item{bias}{the bias estimates.}
#' \item{b.hat}{the estimate of the \eqn{B_{v}(x,z,\tau)} term. See Qu, Yoon, and Perron (2024).}
#' }
#' @export
#'
#' @references Zhongjun Qu, Jungmo Yoon, Pierre Perron (2024), "Inference on Conditional Quantile
#' Processes in Partially Linear Models with Applications to the Impact of Unemployment Benefits,"
#' The Review of Economics and Statistics; \doi{10.1162/rest_a_01168}
#'
#' @examples
#' n = 500
#' x = runif(n,min=-4,max=4)
#' d = (x > 0)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' tlevel = seq(0.1,0.9,by=0.1)
#' tlevel2 = c(0.05,tlevel,0.95)
#' hh = rep(2,length(tlevel))
#' hh2 = rep(2,length(tlevel2))
#'
#' ab = rdq(y=y,x=x,d=d,x0=0,z0=NULL,tau=tlevel2,h.tau=hh2,cov=0)
#' delta = c(0.05,0.09,0.14,0.17,0.19,0.17,0.14,0.09,0.05)
#' hh = rep(2,length(tlevel))
#' fe = rdq.condf(x,Q=ab$qp.est,bcoe=ab$bcoe.p,taus=tlevel,taul=tlevel2,delta=delta,cov=0)
#' be = rdq.bias(y[d==1],x[d==1],dz=0,x0=0,z0=NULL,taus=tlevel,hh,hh,fx=fe$ff[(d==1),],cov=0)
#'
#'
rdq.bias <- function(y,x,dz,x0,z0,taus,h.tau,h.tau2,fx,cov){
  n <- length(y)
  m <- length(taus)
  if(cov==0){x <- as.vector(x); z <- x - x0; w <- NULL; dg <- 1}
  if(cov==1){z <- x[,1] - x0; w <- x[,-1]}
  if(cov==1 & dz==1){dg <- length(z0)}	# number of groups by covariates
  if(cov==1 & dz >1){dg <- nrow(z0)}
  dim.x  <- 2*(1+dz)
  dim.x2 <- 3*(1+dz)
  chs <- (dim.x+1):dim.x2
  b.hat <- array(0,c(m,dg))
  for(k in 1:m){
    wk  <- depa(z/h.tau[k])
    wk2 <- depa(z/h.tau2[k])
    if(cov==0){mdl <- y ~ z + I(z^2)}
    if(cov==1){mdl <- y ~ w + z + I(w*z) + I(z^2) + I(w*(z^2))}
    a1 <- rq(mdl, method="fn", tau = taus[k], weights=wk2)
    gam2 <- a1$coef[chs]
    zh <- z/h.tau[k]
    if(cov==0){
      wj <- cbind(1,zh)
      qj <- zh^2
      cj <- solve(crossprod(wj,(wk*wj)))[1,] %*% crossprod(wj,(wk*qj))
      b.hat[k,1] <- cj %*% gam2
    }
    if(cov==1){
      fx <- as.matrix(fx)
      wj <- cbind(1,w,zh,(w*zh))
      qj <- cbind((zh^2),(w*(zh^2)))
      sel <- 1:(1+dz)
      cj <- solve(crossprod((wj*fx[,k]),(wk*wj))) %*% crossprod((wj*fx[,k]),(wk*qj))
      bproj <- (cj %*% gam2)[sel]
      for(i in 1:dg){
        zz0 <- if (dz == 1) c(1, z0[i]) else c(1, z0[i, ])
        b.hat[k, i] <- zz0 %*% bproj
      }
    }
  }
  bias.e <- b.hat*(h.tau^2)		# point estimste
  return(list(bias = bias.e, bhat = b.hat))
}
