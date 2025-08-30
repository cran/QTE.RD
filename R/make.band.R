#' Uniform confidence bands for QTE
#' @description
#' \code{make.band} constructs uniform confidence bands using the output of \code{rdq.sim}.
#' The function \code{rdq.band} calls this function to generates uniform bands.
#'
#' @usage make.band(n.sam,Dc.p,Dc.m,Dr.p,Dr.m,dz,cov,taus,hh,Qy.p,Qy.m,
#'      bias.p,bias.m,alpha,n.sim)
#'
#' @param n.sam the sample size.
#' @param Dc.p simulated values from \eqn{D_{1,v}(x_{0}^{+},z,\tau)}.
#' @param Dc.m simulated values from \eqn{D_{1,v}(x_{0}^{-},z,\tau)}.
#' @param Dr.p simulated values from \eqn{D_{1,v}(x_{0}^{+},z,\tau) - D_{2,v}(x_{0}^{+},z,\tau)}.
#' @param Dr.m simulated values from \eqn{D_{1,v}(x_{0}^{-},z,\tau) - D_{2,v}(x_{0}^{-},z,\tau)}.
#' @param dz the number of covariates
#' @param cov either 0 or 1. Set \emph{cov=1} if covariates are present in the model;
#' otherwise set \emph{cov=0}.
#' @param taus a vector of quantiles of interest.
#' @param hh the bandwidth values.
#' @param Qy.p estimated conditional quantiles at \eqn{(x_{0}^{+},z)}.
#' @param Qy.m estimated conditional quantiles at \eqn{(x_{0}^{-},z)}.
#' @param bias.p estimated bias terms at \eqn{(x_{0}^{+},z)}.
#' @param bias.m estimated bias terms at \eqn{(x_{0}^{-},z)}.
#' @param alpha a number between 0 and 1, the desired significance level.
#' @param n.sim the number of simulation repetitions.
#'
#' @return A list with elements:
#' \describe{
#' \item{qte}{QTE estimates without bias correction.}
#' \item{qte.r}{QTE estimates with bias correction.}
#' \item{uband}{uniform confidence band for QTE without bias correction.}
#' \item{uband.r}{uniform confidence band for QTE with robust bias correction.}
#' \item{sig}{standard errors for the bias-uncorrected QTE estimates.}
#' \item{sig.r}{standard errors for the bias-corrected QTE estimates. The values reflect
#' the impact of the bias correction on the estimation precision.}
#' }
#' @importFrom stats quantile
#' @seealso [rdq.band]
#' @keywords internal
#' @examples
#' n = 500
#' x = runif(n,min=-4,max=4)
#' d = (x > 0)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' tlevel = seq(0.1,0.9,by=0.1)
#' tlevel2 = c(0.05,tlevel,0.95)
#' hh = rep(2,length(tlevel))
#' hh2 = rep(2,length(tlevel2))
#' sel = tlevel2 %in% tlevel
#'
#' ab = rdq(y=y,x=x,d=d,x0=0,z0=NULL,tau=tlevel2,h.tau=hh2,cov=0)
#' delta = c(0.05,0.09,0.14,0.17,0.19,0.17,0.14,0.09,0.05)
#' fp = rdq.condf(x=x,Q=ab$qp.est,bcoe=ab$bcoe.p,taus=tlevel,taul=tlevel2,delta=delta,cov=0)
#' fm = rdq.condf(x=x,Q=ab$qm.est,bcoe=ab$bcoe.m,taus=tlevel,taul=tlevel2,delta=delta,cov=0)
#' bp = rdq.bias(y[d==1],x[d==1],dz=0,x0=0,z0=NULL,taus=tlevel,hh,hh,fx=fp$ff[(d==1),],cov=0)
#' bm = rdq.bias(y[d==0],x[d==0],dz=0,x0=0,z0=NULL,taus=tlevel,hh,hh,fx=fm$ff[(d==0),],cov=0)
#'
#' sa = QTE.RD:::rdq.sim(x=x,d=d,x0=0,z0=NULL,dz=0,cov=0,tt=tlevel,hh,hh,fxp=fp$ff,fxm=fm$ff,n.sim=200)
#' ba = QTE.RD:::make.band(n,Dc.p=sa$dcp,Dc.m=sa$dcm,Dr.p=sa$drp,Dr.m=sa$drm,dz=0,cov=0,
#' taus=tlevel,hh,Qy.p=as.matrix(ab$qp.est[sel,]),Qy.m=as.matrix(ab$qm.est[sel,]),
#' bias.p=bp$bias,bias.m=bm$bias,alpha=0.1,n.sim=200)
#'
make.band <- function(n.sam,Dc.p,Dc.m,Dr.p,Dr.m,dz,cov,taus,hh,Qy.p,Qy.m,bias.p,bias.m,alpha,n.sim){
  m <- length(taus)
  if(dz==0){
    dim(Dc.p) <- c(1,m,n.sim); dim(Dc.m) <- c(1,m,n.sim)
    dim(Dr.p) <- c(1,m,n.sim); dim(Dr.m) <- c(1,m,n.sim)
  }
  dg <- dim(Dc.p)[1]
  uci <- array(0,c(m,2,dg)); uci.r <- uci
  Qd <- array(0,c(m,dg)); Qd.adj <- Qd
  sig <- array(0,c(m,dg)); sig.r <- sig

  for(i in 1:dg){
    Gs <- Dc.p[i,,] - Dc.m[i,,]
    Gr <- Dr.p[i,,] - Dr.m[i,,]
    Qy <- Qy.p[, i] - Qy.m[, i]
    Qy.adj <- Qy - (bias.p[, i] - bias.m[, i])
    # Uniform band
    res_s <- compute.bands(Gs, Qy, n.sim, n.sam, hh, alpha)
    uci[,,i] <- res_s$band
    sig[,i] <- res_s$sigma
    # Robust uniform band
    res_r <- compute.bands(Gr, Qy.adj, n.sim, n.sam, hh, alpha)
    uci.r[,,i] <- res_r$band
    sig.r[,i] <- res_r$sigma
    # Point estimates
    Qd[,i] <- Qy
    Qd.adj[,i] <- Qy.adj
  }
  return(list(qte = Qd, qte.r = Qd.adj, uband = uci, uband.r = uci.r, s = sig, s.r = sig.r))
}
