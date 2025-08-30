#' Estimate the QTE under the RDD
#' @description
#' \code{rdq} estimates QTE under the RDD with or without covariates. This function is used by \code{rd.qte} to generate QTE estimates.
#'
#' @param y a numeric vector, the outcome variable.
#' @param x a vector (or a matrix) of covariates, the first column is the running variable.
#' @param d a numeric vector, the treatment status.
#' @param x0 the cutoff point.
#' @param z0 the value of the covariates at which to evaluate the effects.
#' For example, if a female dummy is included, z0 = 1 may indicate the female subgroup.
#' @param tau a vector of quantiles of interest.
#' @param h.tau the bandwidth values (specified for each quantile level).
#' @param cov either 0 or 1. Set `cov=1` if covariates are present in the model;
#' otherwise set `cov=0`.
#'
#' @return A list with elements:
#' \describe{
#' \item{qte}{QTE estimates.}
#' \item{qp.est}{conditional quantile estimates on the right side of \eqn{x_{0}} (or for the D=1 group).}
#' \item{qm.est}{conditional quantile estimates on the left side of \eqn{x_{0}} (or for the D=0 group).}
#' \item{bcoe.p}{quantile regression coefficients on the right side of \eqn{x_{0}}.}
#' \item{bcoe.m}{quantile regression coefficients on the left side of \eqn{x_{0}}.}
#' }
#'
#' @export
#'
#' @import quantreg
#' @examples
#' # Without covariate
#' n = 500
#' x = runif(n,min=-4,max=4)
#' d = (x > 0)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' tlevel = seq(0.1,0.9,by=0.1)
#' hh = rep(2,length(tlevel))
#' rdq(y=y,x=x,d=d,x0=0,z0=NULL,tau=tlevel,h.tau=hh,cov=0)
#'
#' # (continued) With covariates
#' z = sample(c(0,1),n,replace=TRUE)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + d*z + rnorm(n)
#' rdq(y=y,x=cbind(x,z),d=d,x0=0,z0=c(0,1),tau=tlevel,h.tau=hh,cov=1)
#'
rdq <- function(y,x,d,x0,z0=NULL,tau,h.tau,cov){
  x <- as.matrix(x)
  m <- length(tau)
  z <- x[,1] - x0
  dz <- ncol(x)-1
  if(cov==0){w <- NULL; dg = 1}
  if(cov==1){w <- as.matrix(x[,-1])}
  if(cov==1 & dz==1){dg <- length(z0)}	# number of groups by covariates
  if(cov==1 & dz >1){dg <- nrow(z0)}
  ym <- y[d==0]; yp <- y[d==1]; zm <- z[d==0]; zp <- z[d==1]
  wm <- w[d==0,]; wp <- w[d==1,]
  # point estimation
  bcoe.p <- array(0,c(m,(1+dz))); bcoe.m <- bcoe.p
  for(i in 1:m){
    kp <- depa(zp/h.tau[i]); km = depa(zm/h.tau[i])
    if(cov==0){m1 <- yp ~ zp; m2 <- ym ~ zm}
    if(cov==1){m1 <- yp ~ zp + wp + I(zp*wp); m2 <- ym ~ zm + wm + I(zm*wm)}
    b1 <- try(rq(m1,method="fn",subset=(kp!=0),tau=tau[i],weights=kp))
    b2 <- try(rq(m2,method="fn",subset=(km!=0),tau=tau[i],weights=km))
    if(cov==0){bcoe.p[i,1] <- b1$coef[1]; bcoe.m[i,1] <- b2$coef[1]}
    if(cov==1){
      bcoe.p[i,] <- b1$coef[c(1,(2+1):(2+dz))]
      bcoe.m[i,] <- b2$coef[c(1,(2+1):(2+dz))]
    }
  }
  Qp <- array(0,c(m,dg)); Qm <- array(0,c(m,dg))
  if(cov==0){
    Qp[,1] <- bcoe.p[,1]
    Qm[,1] <- bcoe.m[,1]
  }
  if(cov==1 & dz==1){
    Qp <- outer(bcoe.p[,1], rep(1, dg)) + outer(bcoe.p[,2], z0)
    Qm <- outer(bcoe.m[,1], rep(1, dg)) + outer(bcoe.m[,2], z0)
  }
  else if(cov==1 & dz > 1){
    Zmat <- cbind(1, z0)
    Qp <- bcoe.p %*% t(Zmat)
    Qm <- bcoe.m %*% t(Zmat)
  }
  Qd <- Qp - Qm
  return(list(qte = Qd, qp.est = Qp, qm.est = Qm, bcoe.p = bcoe.p, bcoe.m = bcoe.m))
}
