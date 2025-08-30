#' Uniform confidence bands for QTE
#'
#' @description
#' \code{rdq.band} produces uniform confidence bands for QTEs with and without bias correction.
#' This function is used by \code{rd.qte} to generate uniform bands.
#'
#' @param y a numeric vector, the outcome variable.
#' @param x a vector (or a matrix) of covariates, When no covariates are included,
#' \eqn{x} is simply a vector of the running variable
#' and \eqn{z0} can be left unspecified. When covariates are included,
#' \eqn{x} should be a matrix with the running variable in the first column and
#' the covariates in the remaining columns.
#' @param d a numeric vector, the treatment status.
#' @param x0 the cutoff point.
#' @param z0 the value of the covariates at which to evaluate the effects.
#' For example, if a female dummy is included, `z0 = 1` may indicate the female subgroup.
#' @param tau a vector of quantiles of interest.
#' @param bdw the bandwidth value(s). If `bdw` is a scalar, it is interpreted as the
#' bandwidth for the median. The bandwidths for the rest of the quantiles are
#' computed automatically using the formula in Yu and Jones (1998).
#' If it is a vector with the same dimension as `tau``,
#' the function will use these values for the respective quantiles accordingly.
#' @param alpha a numeric value between 0 and 1 specifying the significance level.
#' For example, setting `alpha = 0.1` yields a 90% uniform confidence band.
#' Multiple significance levels can be specified, e.g., `alpha = c(0.1, 0.05)`.
#'
#' @return
#' \describe{
#' \item{qte}{QTE estimates without bias correction.}
#' \item{qte.cor}{bias corrected QTE estimates.}
#' \item{uband}{uniform confidence band for QTE without bias correction.}
#' \item{uband.robust}{uniform confidence band for QTE with robust bias correction.}
#' \item{sig}{standard errors for each quantile level for estimates without bias correction.}
#' \item{sig.r}{standard errors for each quantile level for estimates with robust bias correction.}
#' \item{uband.p}{uniform confidence band for the conditional quantile estimates on the right side of the cutoff, without bias correction.}
#' \item{uband.robust.p}{uniform confidence band for the conditional quantile estimates on the right side of the cutoff, robust to the bias correction.}
#' \item{uband.m}{uniform confidence band for the conditional quantile estimates on the left side of the cutoff, without bias correction.}
#' \item{uband.robust.m}{uniform confidence band for the conditional quantile estimates on the left side of the cutoff, robust to the bias correction.}
#' }
#' @export
#' @seealso [rd.qte]
#'
#' @references Zhongjun Qu, Jungmo Yoon, Pierre Perron (2024), "Inference on Conditional Quantile
#' Processes in Partially Linear Models with Applications to the Impact of Unemployment Benefits,"
#' The Review of Economics and Statistics; \doi{10.1162/rest_a_01168}
#' @references Zhongjun Qu and Jungmo Yoon (2019), "Uniform Inference on Quantile Effects
#' under Sharp Regression Discontinuity Designs," Journal of Business and Economic Statistics,
#' 37(4), 625–647; \doi{10.1080/07350015.2017.1407323}
#' @references Keming Yu and M. C. Jones (1998), “Local Linear Quantile Regression,”
#' Journal of the American Statistical Association, 93(441), 228–237; \doi{10.2307/2669619}
#'
#' @examples
#' # Without covariate
#' n = 500
#' x = runif(n,min=-4,max=4)
#' d = (x > 0)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' tlevel = seq(0.1,0.9,by=0.1)
#' D = rdq.band(y=y,x=x,d=d,x0=0,z0=NULL,tau=tlevel,bdw=2,alpha=0.1)
#'
#' # (continued) With covariates
#' z = sample(c(0,1),n,replace=TRUE)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + d*z + rnorm(n)
#' \donttest{D = rdq.band(y=y,x=cbind(x,z),d=d,x0=0,z0=c(0,1),tau=tlevel,bdw=2,alpha=0.1)}
#'
rdq.band <- function(y,x,d,x0,z0=NULL,tau,bdw,alpha=0.1){
  x <- as.matrix(x)
  dz <- ncol(x)-1
  cov <- if(dz == 0) 0 else 1
  # remove missing observations
  mis <- apply(apply(cbind(y,x,d),2,is.na),1,max)
  y <- y[mis==0]; x <- as.matrix(x[mis==0,]); d <- d[mis==0]
  n <- length(y)
  if(length(alpha)==0){stop("Provide alpha to obtain confidence band.")}
  if(cov==0){w = NULL; dg = 1}
  if(cov==1){w = as.matrix(x[,-1])}
  if(cov==1 & dz==1){dg = length(z0)}	# number of subgroups by covariates
  if(cov==1 & dz >1){dg = nrow(z0)}
  n.sim = ifelse((n<50000),1000,500)	# simulation repetitions for uniform bands
  # quantile levels and bandwidths
  bdw.opt <- if (length(bdw) == 1) 1 else if (length(bdw) == length(tau)) 2
  if(length(bdw)>1 & length(bdw)!=length(tau))
  {stop("The length of bdw should be one or equal to the length of tau.")}
  if(bdw.opt==1){
    tt <- sort(unique(c(tau,0.5)))	# qualtile levels to estimate
    hh <- bdw*((2*tt*(1-tt)/(pi*dnorm(qnorm(tt))^{2}))^{1/5})  # quantile specific bandwidths
  } else {tt <- tau; hh <- bdw
  }
  tt.ext <- c(0.25,0.5)*tt[1]		# for conditional density estimation
  tt.exp <- sort(c(tt.ext,tt,(1-tt.ext)))
  ind <- tt.exp %in% tt
  if(bdw.opt==1){hh2 <- bdw*((2*tt.exp*(1-tt.exp)/(pi*dnorm(qnorm(tt.exp))^{2}))^{1/5})}
  if(bdw.opt==2){hh2 <- c(rep(hh[1],2),hh,rep(hh[1],2))}
  # obtains QTE and uniform bands
  ab <- rdq(y,x,d,x0,z0,tau=tt.exp,h.tau=hh2,cov)
  # Bias estimation and correction
  delta <- bandwidth.rq(tt,n,hs=F)
  fp <- rdq.condf(x,Q=ab$qp.est,bcoe=ab$bcoe.p,taus=tt,taul=tt.exp,delta,cov)
  fm <- rdq.condf(x,Q=ab$qm.est,bcoe=ab$bcoe.m,taus=tt,taul=tt.exp,delta,cov)
  bp <- rdq.bias(y[d==1],x[(d==1),],dz,x0,z0,taus=tt,hh2[ind],hh2[ind],fx=fp$ff[(d==1),],cov)
  bm <- rdq.bias(y[d==0],x[(d==0),],dz,x0,z0,taus=tt,hh2[ind],hh2[ind],fx=fm$ff[(d==0),],cov)
  # uniform bands
  sm <- rdq.sim(x,d,x0,z0,dz,cov,tt=tt,hh2[ind],hh2[ind],fxp=fp$ff,fxm=fm$ff,n.sim)
  # uniform band for QTE
  ba <- make.band(n,Dc.p=sm$dcp,Dc.m=sm$dcm,Dr.p=sm$drp,Dr.m=sm$drm,dz,cov,taus=tt,hh2[ind],Qy.p=as.matrix(ab$qp.est[ind,,drop=FALSE]),Qy.m=as.matrix(ab$qm.est[ind,,drop=FALSE]),bias.p=bp$bias,bias.m=bm$bias,alpha,n.sim)
  # uniform bands for conditional quantiles
  ba2 <- make.band.cq(n,Dc.p=sm$dcp,Dc.m=sm$dcm,Dr.p=sm$drp,Dr.m=sm$drm,dz,cov,taus=tt,hh2[ind],Qy.p=as.matrix(ab$qp.est[ind,,drop=FALSE]),Qy.m=as.matrix(ab$qm.est[ind,,drop=FALSE]),bias.p=bp$bias,bias.m=bm$bias,alpha,n.sim)
  out <- list(qte = ba$qte, qte.cor = ba$qte.r, uband = ba$uband, uband.robust = ba$uband.r, sig = ba$s, sig.r = ba$s.r, uband.p = ba2$ubandp, uband.m = ba2$ubandm, uband.robust.p = ba2$ubandp.r, uband.robust.m = ba2$ubandm.r, tau = tt, alpha = alpha, cov = cov)
  class(out) <- c("band.qte", class(out))
  return(out)
}
