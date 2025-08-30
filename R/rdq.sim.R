#' Simulation the asymptotic distributions
#' @description \code{rdq.sim} produces iid draws from the asymptotic distribution of the conditional quantile process estimate.
#'
#' @param x a vector (or a matrix) of covariates.
#' @param d a numeric vector, the treatment status.
#' @param x0 the cutoff point.
#' @param z0 the value of the covariates at which to evaluate the effects.
#' @param dz the number of covariates.
#' @param cov either 0 or 1. Set \emph{cov=1} if covariates are present in the model;
#' otherwise set \emph{cov=0}.
#' @param tt a vector of quantiles.
#' @param hh the bandwidth values (specified for each quantile level).
#' @param hh2 the bandwidth values for the local quadratic quantile regression.
#' @param fxp conditional density estimates on the right side of \eqn{x_0}.
#' @param fxm conditional density estimates on the left side of \eqn{x_0}.
#' @param n.sim the number of simulation repetitions.
#'
#' @return A list with elements:
#' \describe{
#' \item{dcp}{realizations from the asymptotic distribution of the conditional quantile process, from the right side of \eqn{x_0}.}
#' \item{dcm}{realizations from the asymptotic distribution of the conditional quantile process, from the left side of \eqn{x_0}.}
#' \item{drp}{realizations from the asymptotic distribution of the bias corrected conditional quantile process, from the right side of \eqn{x_0}.}
#' \item{drm}{realizations from the asymptotic distribution of the bias corrected conditional quantile process, from the left side of \eqn{x_0}.}
#' }
#' @keywords internal
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
#' fp = rdq.condf(x=x,Q=ab$qp.est,bcoe=ab$bcoe.p,taus=tlevel,taul=tlevel2,delta,cov=0)
#' fm = rdq.condf(x=x,Q=ab$qm.est,bcoe=ab$bcoe.m,taus=tlevel,taul=tlevel2,delta,cov=0)
#' sa = QTE.RD:::rdq.sim(x=x,d=d,x0=0,z0=NULL,dz=0,cov=0,tt=tlevel,hh,hh,fxp=fp$ff,fxm=fm$ff,n.sim=200)
#'
rdq.sim <- function(x,d,x0,z0,dz,cov,tt,hh,hh2,fxp,fxm,n.sim){
  m = length(tt)
  if(cov==0){x <- as.vector(x); d <- as.vector(d); n <- length(x); z <- x - x0; w <- NULL; dg <- 1}
  if(cov==1){n <- nrow(x); z <- x[,1] - x0; w <- x[,2:(1+dz)]}
  if(cov==1 & dz==1){dg <- length(z0)}	# number of groups by covariates
  if(cov==1 & dz >1){dg <- nrow(z0)}
  dim.x  <- 2*(1+dz)
  dim.x2 <- 3*(1+dz)
  chs <- (dim.x+1):dim.x2
  p1.p <- array(0,c(dim.x,m,n.sim))
  p1.m <- p1.p
  p3.p <- array(0,c(dim.x2,m,n.sim))
  p3.m <- p3.p
  for(j in 1:n.sim){
    u  <- runif(n)
    for(k in 1:m){
      kx <- depa(z/hh[k])
      kx2 <- depa(z/hh2[k])
      en <- n*hh[k]
      en2 <- n*hh2[k]
      zh <- z/hh[k]
      xj <- cbind(1,w,zh,(w*zh))
      qj <- cbind((zh^2),(w*(zh^2)))
      xj2 <- cbind(xj,qj)
      uu <- tt[k] - (u <= tt[k])
      p1.p[,k,j] <- apply((d*uu*kx*xj),2,sum)/sqrt(en)
      p1.m[,k,j] <- apply(((1-d)*uu*kx*xj),2,sum)/sqrt(en)
      p3.p[,k,j] <- apply((d*uu*kx2*xj2),2,sum)/sqrt(en2)
      p3.m[,k,j] <- apply(((1-d)*uu*kx2*xj2),2,sum)/sqrt(en2)
    }
  }
  Gs1 <- array(0,c(dg,m,n.sim)); Gs2 <- Gs1
  Gr1 <- Gs1; Gr2 <- Gr1
  for(k in 1:m){
    kx <- depa(z/hh[k])
    kx2 <- depa(z/hh2[k])
    en <- n*hh[k]
    en2 <- n*hh2[k]
    zh <- z/hh[k]
    xj <- cbind(1,w,zh,(w*zh))
    qj <- cbind((zh^2),(w*(zh^2)))
    xj2 <- cbind(xj,qj)
    q1.p <- solve(crossprod((kx*xj),(d*fxp[,k]*xj))/en)
    q1.m <- solve(crossprod((kx*xj),((1-d)*fxm[,k]*xj))/en)
    pp1 <- p1.p[,k,]
    pm1 <- p1.m[,k,]
    d1p <- apply(pp1, 2, function(x) q1.p%*%x)
    d1m <- apply(pm1, 2, function(x) q1.m%*%x)
    # for robust band
    q2.p <- q1.p
    q2.m <- q1.m
    p2.p <- crossprod((d*fxp[,k]*xj),(kx*qj))/en
    p2.m <- crossprod(((1-d)*fxm[,k]*xj),(kx*qj))/en
    d2p <- q2.p %*% p2.p
    d2m <- q2.m %*% p2.m
    q3.p  <- solve(crossprod((kx2*xj2),(d*fxp[,k]*xj2))/en2)
    q3.m  <- solve(crossprod((kx2*xj2),((1-d)*fxm[,k]*xj2))/en2)
    pp3 <- p3.p[,k,]
    pm3 <- p3.m[,k,]
    d3p <- apply(pp3, 2, function(x) q3.p %*% x)
    d3m <- apply(pm3, 2, function(x) q3.m %*% x)
    r1 <- (hh[k]/hh2[k])^{(1+4)/2}
    # collect outcomes
    if(cov==0){
      Gs1[1,k,] <- d1p[1,]
      Gs2[1,k,] <- d1m[1,]
      Gr1[1,k,] <- d1p[1,] - r1*d2p[1,]*d3p[chs,]
      Gr2[1,k,] <- d1m[1,] - r1*d2m[1,]*d3m[chs,]
    }
    if(cov==1){
      for(i in 1:dg){
        if(dz==1){z.eval <- c(1,z0[i])}
        if(dz>=2){z.eval <- c(1,z0[i,])}
        Gs1[i,k,] <- apply(d1p[1:(1+dz),], 2, function(x) crossprod(z.eval,x))
        Gs2[i,k,] <- apply(d1m[1:(1+dz),], 2, function(x) crossprod(z.eval,x))
        d21p <- z.eval %*% d2p[1:(1+dz),]
        d21m <- z.eval %*% d2m[1:(1+dz),]
        Gr1[i,k,] <- Gs1[i,k,] - r1*apply(d3p[chs,], 2, function(x) d21p %*% x)
        Gr2[i,k,] <- Gs2[i,k,] - r1*apply(d3m[chs,], 2, function(x) d21m %*% x)
      }
    }
  }
  return(list(dcp = Gs1, dcm = Gs2, drp = Gr1, drm = Gr2))
}
