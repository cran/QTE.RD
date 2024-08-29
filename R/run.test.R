#' Run tests
#' @description
#' \code{run.test} performs hypothesis testing. The function \code{rdq.test} calls this function to run tests.
#'
#' @usage run.test(n.sam,dz,taus,hh,Dc.p,Dc.m,Dr.p,Dr.m,Qy.p,Qy.m,bias.p,bias.m,
#'      cov,bias,alpha,n.sim,test.type,std.opt)
#'
#' @param n.sam the sample size.
#' @param dz the number of covariates.
#' @param taus a vector of quantiles of interest.
#' @param hh the bandwidth values.
#' @param Dc.p simulated values from \eqn{D_{1,v}(x_{0}^{+},z,\tau)}.
#' @param Dc.m simulated values from \eqn{D_{1,v}(x_{0}^{-},z,\tau)}.
#' @param Dr.p simulated values from \eqn{D_{1,v}(x_{0}^{+},z,\tau) - D_{2,v}(x_{0}^{+},z,\tau)}.
#' @param Dr.m simulated values from \eqn{D_{1,v}(x_{0}^{-},z,\tau) - D_{2,v}(x_{0}^{-},z,\tau)}.
#' @param Qy.p estimated conditional quantiles at \eqn{(x_{0}^{+},z)}.
#' @param Qy.m estimated conditional quantiles at \eqn{(x_{0}^{-},z)}.
#' @param bias.p estimated bias terms at \eqn{(x_{0}^{+},z)}.
#' @param bias.m estimated bias terms at \eqn{(x_{0}^{-},z)}.
#' @param cov either 0 or 1. Set \emph{cov=1} if covariates are present in the model;
#' otherwise set \emph{cov=0}.
#' @param bias either 0 or 1. If \emph{bias=1}, the QTE estimate is bias corrected and
#' the robust confidence band in Qu, Yoon, and Perron (2024) is produced.
#' If \emph{bias=0}, no bias correction is implemented.
#' @param alpha a number between 0 and 1, the desired significance level.
#' @param n.sim the number of simulation repetitions.
#' @param test.type a value in 1--4. Set \emph{type} to 1 to test the null hypothesis of a zero
#' treatment effect against the alternative hypothesis of significant treatment effects;
#' set \emph{type} to 2 to test the null hypothesis of homogeneous treatment against heterogeneous treatment effects;
#' set \emph{type} to 3 to test the null hypothesis of uniformly non-negative treatment effects against the presence of negative effects;
#' and set \emph{type} to 4 to test the null hypothesis of uniformly non-positive treatment effects against the presence of positive effects at some quantiles.
#' @param std.opt either 0 or 1. If \emph{std.opt=1}, the test statistic is standardized so that
#' the variance is equalized across quantiles; if \emph{std.opt=0}, the test is not standardized.
#'
#' @return A list with elements:
#' \describe{
#' \item{test.stat}{test statistics.}
#' \item{cr.value}{critical values.}
#' \item{p.val}{p values.}
#' }
#' @export
#' @seealso [rdq.test]
#' @references Zhongjun Qu, Jungmo Yoon, Pierre Perron (2024), "Inference on Conditional Quantile
#' Processes in Partially Linear Models with Applications to the Impact of Unemployment Benefits,"
#' The Review of Economics and Statistics; https://doi.org/10.1162/rest_a_01168
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
#' bt <- run.test(n,dz=0,taus=tlevel,hh,Dc.p=sa$dcp,Dc.m=sa$dcm,Dr.p=sa$drp,Dr.m=sa$drm,
#' Qy.p=as.matrix(ab$qp.est[sel,]),Qy.m=as.matrix(ab$qm.est[sel,]),bias.p=bp$bias,bias.m=bm$bias,
#' cov=0,bias=1,alpha=0.1,n.sim=200,test.type=1,std.opt=1)
#'
run.test <- function(n.sam,dz,taus,hh,Dc.p,Dc.m,Dr.p,Dr.m,Qy.p,Qy.m,bias.p,bias.m,cov,bias,alpha,n.sim,test.type,std.opt){
  m <- length(taus)
  if(dz==0){
    dim(Dc.p) <- c(1,m,n.sim); dim(Dc.m) <- c(1,m,n.sim)
    dim(Dr.p) <- c(1,m,n.sim); dim(Dr.m) <- c(1,m,n.sim)
  }
  dg <- dim(Dc.p)[1]
  ts <- array(0,c(dg,1))
  za <- array(0,c(dg,length(alpha)))
  pv <- array(0,c(dg,1))
  for(i in 1:dg){
    Qy <- Qy.p[,i] - Qy.m[,i]
    Qy.adj <- Qy - (bias.p[,i] - bias.m[,i])
    if(bias==0){Gs <- t(Dc.p[i,,] - Dc.m[i,,])}
    if(bias==1){Gs <- t(Dr.p[i,,] - Dr.m[i,,])}
    if(m==1){shat <- sqrt(mean(Gs*Gs))}
    if(m >1){shat <- sqrt(apply((Gs*Gs),2,mean))}
    if(std.opt==1){
      Gr <- Gs/matrix(rep(shat,n.sim),byrow=TRUE,nrow=n.sim)
      if(bias==0){Q.t <- (sqrt(n.sam*hh)/shat)*Qy}
      if(bias==1){Q.t <- (sqrt(n.sam*hh)/shat)*Qy.adj}
    }
    if(std.opt==0){
      Gr <- Gs
      if(bias==0){Q.t <- sqrt(n.sam*hh)*Qy}
      if(bias==1){Q.t <- sqrt(n.sam*hh)*Qy.adj}
    }
    if(test.type==1){ # Treatment Significance
      ts[i] <- max(abs(Q.t))
      zz <- apply(abs(Gr),1,max)
      za[i,] <- quantile(zz,probs=(1-alpha))
    }
    if(test.type==2){ # Treatment Homegeneity
      m2 <- sqrt(n.sam*hh)/shat
      ts[i] <- max(abs(Q.t - (m2/mean(m2))*rep(mean(Q.t),m)))
      m2.mat <- matrix(rep((m2/mean(m2)),n.sim),byrow=TRUE,nrow=n.sim)
      zmean <- apply(Gr,1,mean)
      zz <- apply(abs(Gr - m2.mat*matrix(rep(zmean,m),nrow=n.sim)),1,max)
      za[i,] <- quantile(zz,probs=(1-alpha))		# critical values
    }
    if(test.type==3){ # Treatment Unambiguity
      ts[i] <- max(abs((Q.t <= 0)*Q.t))  		# test statistics
      zz <- apply(abs((Gr <= 0)*Gr),1,max)
      za[i,] <- quantile(zz,probs=(1-alpha))		# critical values
    }
    if(test.type==4){ # Treatment (negative) Unambiguity
      ts[i] <- max(abs((Q.t >= 0)*Q.t))  		# test statistics
      zz <- apply(abs((Gr >= 0)*Gr),1,max)
      za[i,] <- quantile(zz,probs=(1-alpha))		# critical values
    }
    pa <- mean(zz >= ts[i])
    if(pa < n.sim^{-1}){pv[i] <- n.sim^{-1}}
    else {pv[i] <- pa}
  }
  colnames(za) <- (1-alpha)
  return(list(test.stat = ts, cr.value = za, p.val = pv))
}
