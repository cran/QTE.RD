#' Uniform confidence bands for conditional quantile processes
#' @description
#' \code{make.band.cq} constructs uniform confidence bands for conditional quantile processes as functions of tau for each side of the cutoff.
#' See \code{make.band} as well. The function \code{rdq.band} calls this function to generates uniform bands for conditional quantiles.
#'
#' @usage make.band.cq(n.sam,Dc.p,Dc.m,Dr.p,Dr.m,dz,cov,taus,hh,Qy.p,Qy.m,
#'      bias.p,bias.m,alpha,n.sim)
#'
#' @param n.sam the sample size.
#' @param Dc.p simulated values from \eqn{D_{1,v}(x_{0}^{+},z,\tau)}.
#' @param Dc.m simulated values from \eqn{D_{1,v}(x_{0}^{-},z,\tau)}.
#' @param Dr.p simulated values from \eqn{D_{1,v}(x_{0}^{+},z,\tau) - D_{2,v}(x_{0}^{+},z,\tau)}.
#' @param Dr.m simulated values from \eqn{D_{1,v}(x_{0}^{-},z,\tau) - D_{2,v}(x_{0}^{-},z,\tau)}.
#' @param dz the number of covariates.
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
#' \item{qp}{conditional quantile estimates at \eqn{x_{0}^{+}} (i.e., above the cutoff) without bias correction.}
#' \item{qp.r}{bias corrected conditional quantile estimates at \eqn{x_{0}^{+}}.}
#' \item{qm}{conditional quantile estimates at \eqn{x_{0}^{-}} (i.e., below the cutoff) without bias correction.}
#' \item{qm.r}{bias corrected conditional quantile estimates at \eqn{x_{0}^{-}}.}
#' \item{ubandp}{uniform confidence band for conditional quantiles at \eqn{x_{0}^{+}}
#' without bias correction.}
#' \item{ubandp.r}{uniform confidence band for conditional quantiles at \eqn{x_{0}^{+}}
#' with robust bias correction.}
#' \item{ubandm}{uniform confidence band for conditional quantiles at \eqn{x_{0}^{-}}
#' without bias correction.}
#' \item{ubandm.r}{uniform confidence band for conditional quantiles at \eqn{x_{0}^{-}}
#' with robust bias correction.}
#' \item{sp}{standard errors of the conditional quantile estimates without bias correction
#' at \eqn{x_{0}^{+}}.}
#' \item{sp.r}{standard errors of the conditional quantile estimates with robust
#' bias correction at \eqn{x_{0}^{+}}.}
#' \item{sm}{standard errors of the conditional quantile estimates without bias correction
#' at \eqn{x_{0}^{-}}.}
#' \item{sm.r}{standard errors of the conditional quantile estimates with robust
#' bias correction at \eqn{x_{0}^{-}}.}
#' }
#' @export
#' @importFrom quantreg bandwidth.rq
#' @seealso [make.band()]
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
#' fp = rdq.condf(x=x,Q=ab$qp.est,bcoe=ab$bcoe.p,taus=tlevel,taul=tlevel2,delta,cov=0)
#' fm = rdq.condf(x=x,Q=ab$qm.est,bcoe=ab$bcoe.m,taus=tlevel,taul=tlevel2,delta,cov=0)
#' bp = rdq.bias(y[d==1],x[d==1],dz=0,x0=0,z0=NULL,taus=tlevel,hh,hh,fx=fp$ff[(d==1),],cov=0)
#' bm = rdq.bias(y[d==0],x[d==0],dz=0,x0=0,z0=NULL,taus=tlevel,hh,hh,fx=fm$ff[(d==0),],cov=0)
#'
#' sa = rdq.sim(x=x,d=d,x0=0,z0=NULL,dz=0,cov=0,tt=tlevel,hh,hh,fxp=fp$ff,fxm=fm$ff,n.sim=200)
#' ba.cq = make.band.cq(n,Dc.p=sa$dcp,Dc.m=sa$dcm,Dr.p=sa$drp,Dr.m=sa$drm,dz=0,cov=0,
#' taus=tlevel,hh,Qy.p=as.matrix(ab$qp.est[sel,]),Qy.m=as.matrix(ab$qm.est[sel,]),
#' bias.p=bp$bias,bias.m=bm$bias,alpha=0.1,n.sim=200)
#'
make.band.cq <- function(n.sam,Dc.p,Dc.m,Dr.p,Dr.m,dz,cov,taus,hh,Qy.p,Qy.m,bias.p,bias.m,alpha,n.sim){
  m <- length(taus)
  if(dz==0){
    dim(Dc.p) <- c(1,m,n.sim); dim(Dc.m) <- c(1,m,n.sim)
    dim(Dr.p) <- c(1,m,n.sim); dim(Dr.m) <- c(1,m,n.sim)
  }
  dg <- dim(Dc.p)[1]
  uci.p <- array(0,c(m,2,dg)); ucr.p <- uci.p
  uci.m <- array(0,c(m,2,dg)); ucr.m <- uci.m
  Qp <- array(0,c(m,dg)); Qp.adj <- Qp; Qm <- Qp; Qm.adj <- Qp
  sigp <- array(0,c(m,dg)); sigp.r <- sigp
  sigm <- array(0,c(m,dg)); sigm.r <- sigm
  for(k in 1:2){
    for(i in 1:dg){
      if(k==1){Gs <- Dc.p[i,,]; Gr <- Dr.p[i,,]}
      if(k==2){Gs <- Dc.m[i,,]; Gr <- Dr.m[i,,]}
      if(k==1){Qy <- Qy.p[,i]; Qy.adj <- Qy.p[,i] - bias.p[,i]}
      if(k==2){Qy <- Qy.m[,i]; Qy.adj <- Qy.m[,i] - bias.m[,i]}

      # uniform band
      if(m==1){shat <- sqrt(mean(Gs*Gs))}
      if(m >1){shat <- sqrt(apply((Gs*Gs),1,mean))}
      ra <- Gs/matrix(rep(shat,n.sim),ncol=n.sim)
      rs <- apply(abs(ra),2,max)
      cp <- quantile(rs,probs=(1-alpha))
      sigma <- shat/sqrt(n.sam*hh)
      if(k==1){uci.p[,,i] <- cbind((Qy - cp*sigma),(Qy + cp*sigma))}
      if(k==2){uci.m[,,i] <- cbind((Qy - cp*sigma),(Qy + cp*sigma))}

      # robust uniform band
      if(m==1){shat2 <- sqrt(mean(Gr*Gr))}
      if(m >1){shat2 <- sqrt(apply((Gr*Gr),1,mean))}
      ra2 <- Gr/matrix(rep(shat2,n.sim),ncol=n.sim)
      rs2 <- apply(abs(ra2),2,max)
      cp2 <- quantile(rs2,probs=(1-alpha))
      sigma2 <- shat2/sqrt(n.sam*hh)
      if(k==1){ucr.p[,,i] <- cbind((Qy.adj - cp2*sigma2),(Qy.adj + cp2*sigma2))}
      if(k==2){ucr.m[,,i] <- cbind((Qy.adj - cp2*sigma2),(Qy.adj + cp2*sigma2))}

      # point estimates
      if(k==1){Qp[,i] <- Qy; Qp.adj[,i] <- Qy.adj}
      if(k==2){Qm[,i] <- Qy; Qm.adj[,i] <- Qy.adj}

      # sigma
      if(k==1){sigp[,i] <- sigma; sigp.r[,i] <- sigma2}
      if(k==2){sigm[,i] <- sigma; sigm.r[,i] <- sigma2}
    }
  }
  return(list(qp = Qp, qm = Qm, qp.r = Qp.adj, qm.r = Qm.adj, ubandp = uci.p, ubandp.r = ucr.p, ubandm = uci.m, ubandm.r = ucr.m, sp = sigp, sp.r = sigp.r, sm = sigm, sm.r = sigm.r))
}
