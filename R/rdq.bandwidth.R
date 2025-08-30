#' Bandwidth estimation
#' @description
#' \code{rdq.bandwidth} implements two bandwidth selection rules and obtains the cross-validation (CV) bandwidth and the MSE optimal bandwidth.
#'
#' @usage rdq.bandwidth(y, x, d, x0, z0=NULL, cv, val, hp=NULL, pm.each=0, bdy=1, p.order=1, xl=0.5)
#'
#' @param y a numeric vector, the outcome variable.
#' @param x a vector (or a matrix) of covariates, the first column is the running variable.
#' @param d a numeric vector, the treatment status.
#' @param x0 the cutoff point.
#' @param z0 the value of the covariates at which to evaluate the effects.
#' @param cv either 0 or 1. When \emph{cv=1}, both the CV and MSE optimal bandwidths
#' are produced. When \emph{cv=0}, the MSE optimal bandwidth is produced.
#' @param val a set of candidate values for the CV bandwidth.
#' @param hp a pilot bandwidth to estimate nuisance parameters for the MSE optimal bandwidth.
#' It will be used only if \emph{cv=0}. If \emph{cv=1}, the CV bandwidth will be used
#' as the pilot bandwidth to compute the MSE optimal bandwidth.
#' @param pm.each either 0 or 1. When \emph{pm.each=1}, the CV bandwidths for each side of the cutoff will be obtained separately.
#' @param bdy either 0 or 1. When \emph{bdy=1}, the CV bandwidth uses the boundary point procdure.
#' @param p.order either 1 or 2. When \emph{p.order=1}, a local linear regression is used, and
#' when \emph{p.order=2}, a local quadratic regression is used.
#' @param xl if `xl=0.5`, the CV bandwidth use the 50% of observations closest to \eqn{x_0}.
#'
#' @return
#' A list with elements:
#' \describe{
#' \item{cv}{the selected CV bandwidth at the median.}
#' \item{opt.p}{the MSE optimal bandwidth at the median from the right side of \eqn{x_0}.}
#' \item{opt.m}{the MSE optimal bandwidth at the median from the left side of \eqn{x_0}.}
#' }
#' @export
#'
#' @references Zhongjun Qu, Jungmo Yoon, Pierre Perron (2024), "Inference on Conditional Quantile
#' Processes in Partially Linear Models with Applications to the Impact of Unemployment Benefits,"
#' The Review of Economics and Statistics; \doi{10.1162/rest_a_01168}
#' @references Zhongjun Qu and Jungmo Yoon (2019), "Uniform Inference on Quantile Effects
#' under Sharp Regression Discontinuity Designs," Journal of Business and Economic Statistics,
#' 37(4), 625–647; \doi{10.1080/07350015.2017.1407323}
#'
#' @examples
#' # Without covariate
#' n = 500
#' x = runif(n,min=-4,max=4)
#' d = (x > 0)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' tlevel = seq(0.1,0.9,by=0.1)
#' rdq.bandwidth(y=y,x=x,d=d,x0=0,z0=NULL,cv=1,val=(1:4))
#' rdq.bandwidth(y=y,x=x,d=d,x0=0,z0=NULL,cv=0,val=(1:4),hp=2)
#'
#' # (continued) With covariates
#' z = sample(c(0,1),n,replace=TRUE)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + d*z + rnorm(n)
#' rdq.bandwidth(y=y,x=cbind(x,z),d=d,x0=0,z0=c(0,1),cv=1,val=(1:4),bdy=1,p.order=1)
#'
rdq.bandwidth <- function(y,x,d,x0,z0=NULL,cv,val,hp=NULL,pm.each=0,bdy=1,p.order=1,xl=0.5){
  x <- as.matrix(x)
  dz <- ncol(x)-1
  cov <- if(dz == 0) 0 else 1
  mis <- apply(apply(cbind(y,x,d),2,is.na),1,max)
  y <- y[mis==0]; x <- as.matrix(x[mis==0,]); d <- d[mis==0]	# drop missing observations
  n <- length(y)
  if(cov==0){w <- NULL; dg <- 1}
  if(cov==1){w <- as.matrix(x[,-1])}
  if(cov==1 & dz==1){dg <- length(z0)}	# number of groups by covariates
  if(cov==1 & dz >1){dg <- nrow(z0)}
  n.sim <- ifelse((n<50000),300,100)
  if(cv==1){	# CV bandwidth
    if(pm.each==0){
      banda <- cv.bandwidth(y=y, x=x[,1], z=w, dz, x0, val, xl, order=p.order, bdy)
    }
    if(pm.each==1){
      bandp <- cv.bandwidth(y[d==1],x[(d==1),1],w[(d==1),],dz,x0, val, xl, order=p.order, bdy)
      bandm <- cv.bandwidth(y[d==0],x[(d==0),1],w[(d==0),],dz,x0, val, xl, order=p.order, bdy)
    }
  }
  if(cv==0){banda <- NULL}
  # Optimal bandwidth
  delta.m <- bandwidth.rq(0.5,length(y[d==0]),hs=FALSE)
  delta.p <- bandwidth.rq(0.5,length(y[d==1]),hs=FALSE)
  tt.m <- 0.5 + c(-delta.m,0,delta.m); tt.m = round(tt.m,2)
  tt.p <- 0.5 + c(-delta.p,0,delta.p); tt.p = round(tt.p,2)
  if(cv==1 & pm.each==0){hh <- banda$h.cv}
  if(cv==1 & pm.each==1){hh <- (bandp$h.cv+bandm$h.cv)/2}
  if(cv==0){hh <- hp}
  ab.p <- rdq(y,x,d,x0,z0,tau=tt.p,h.tau=rep(hh,length(tt.p)),cov)
  ab.m <- rdq(y,x,d,x0,z0,tau=tt.m,h.tau=rep(hh,length(tt.m)),cov)
  fp <- rdq.condf(x,Q=ab.p$qp.est,bcoe=ab.p$bcoe.p,taus=0.5,taul=tt.p,delta.p,cov)
  fm <- rdq.condf(x,Q=ab.m$qm.est,bcoe=ab.m$bcoe.m,taus=0.5,taul=tt.m,delta.m,cov)
  bp <- rdq.bias(y[d==1],x[(d==1),],dz,x0,z0,taus=0.5,hh,hh,fx=as.matrix(fp$ff[(d==1)]),cov)
  bm <- rdq.bias(y[d==0],x[(d==0),],dz,x0,z0,taus=0.5,hh,hh,fx=as.matrix(fm$ff[(d==0)]),cov)
  sim <- rdq.sim(x,d,x0,z0,dz,cov,tt=0.5,hh,hh,fxp=fp$ff,fxm=fm$ff,n.sim)
  if(cov==0){
    h.op.m <- ((mean(sim$dcm^2)/(4*(bm$bhat^2)))^{1/5})*(n^{-1/5})
    h.op.p <- ((mean(sim$dcp^2)/(4*(bp$bhat^2)))^{1/5})*(n^{-1/5})
  }
  if(cov==1){
    h.op.m <- array(0,c(dg,1))
    h.op.p <- array(0,c(dg,1))
    for(l in 1:dg){
      h.op.m[l] <- ((mean(sim$dcm[l,,]^2)/(4*(bm$bhat[l]^2)))^{1/5})*(n^{-1/5})
      h.op.p[l] <- ((mean(sim$dcp[l,,]^2)/(4*(bp$bhat[l]^2)))^{1/5})*(n^{-1/5})
    }
  }
  # save bandwidth values
  bandwidth <- NULL
  if(cv==1 & pm.each==0){bandwidth$cv <- banda$h.cv}
  if(cv==1 & pm.each==1){bandwidth$cv <- c(bandm$h.cv,bandp$h.cv)}
  # optimal bandwidth is truncated by the maximum candidate value
  h.op.m <- pmin(h.op.m,max(val)); h.op.p <- pmin(h.op.p,max(val))
  bandwidth$opt.m <- h.op.m
  bandwidth$opt.p <- h.op.p
  bandwidth$cov <- cov; bandwidth$dg <- dg
  class(bandwidth) <- c("bw.qte", class(bandwidth))
  return(bandwidth)
}
