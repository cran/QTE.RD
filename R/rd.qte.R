#' QTE and its uniform confidence band.
#'
#' @description
#' \code{rd.qte} is the main function of the QTE.RD package. It estimates QTE with/without covariates.
#' If \emph{bias=1}, it corrects the bias in QTE estimates and obtains the robust confidence band and if \emph{bias=0}, no bias correction is implemented.
#'
#' @usage rd.qte(y, x, d, x0, z0=NULL, tau, bdw, bias)
#'
#' @param y a numeric vector, the outcome variable.
#' @param x a vector (or a matrix) of covariates. When no covariates are included,
#' \eqn{x} is simply a vector of the running variable. When covariates are present,
#' \eqn{x} should be a matrix where the first column contains the running variable
#' and the remaining columns contain the covariates.
#' @param d a numeric vector, the treatment status.
#' @param x0 the cutoff point.
#' @param z0 the value of the covariates at which to evaluate the effects.
#' For example, if a female dummy z is included, z0 = 1 may indicate the female subgroup.
#' @param tau a vector of quantiles of interest.
#' @param bdw the bandwidth value(s). If 'bdw' is a scalar, it is interpreted as the
#' bandwidth for the median. See the function \code{rdq.bandwidth} for how to select this bandwidth.
#' The bandwidths for the rest of the quantiles are computed automatically using the formula of Yu and Jones (1998).
#' If it is a vector with the same dimension as 'tau',
#' the function will use these values for the respective quantiles accordingly.
#' @param bias either 0 or 1. If bias=1, the QTE estimate is bias corrected and
#' the robust confidence band in Qu, Yoon, and Perron (2024) is produced.
#' If bias=0, no bias correction is implemented.
#'
#' @return A list with elements:
#' \describe{
#' \item{qte}{QTE estimates.}
#' \item{uband}{uniform confidence band for QTE. If \emph{bias=1}, the band is robust
#' capturing the effect of the bias correction. If \emph{bias=0}, no bias correction is implemented.}
#' \item{sigma}{standard errors for each quantile level. If \emph{bias=1}, its value
#' captures the effect of the bias correction. If \emph{bias=0}, no bias correction is implemented.}
#' \item{qp.est}{conditional quantile estimates on the right side of \eqn{x_{0}} (or for the \eqn{D=1} group).}
#' \item{qm.est}{conditional quantile estimates on the left side of \eqn{x_{0}} (or for the \eqn{D=0} group).}
#'}
#' @export
#'
#' @import quantreg
#' @importFrom stats approx
#' @importFrom stats dnorm
#' @importFrom stats qnorm
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
#' n <- 500
#' x <- runif(n,min=-4,max=4)
#' d <- (x > 0)
#' y <- x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' tlevel <- seq(0.1,0.9,by=0.1)
#' \donttest{A <- rd.qte(y=y,x=x,d=d,x0=0,z0=NULL,tau=tlevel,bdw=2,bias=1)}
#'
#' # (continued) With covariates
#' z <- sample(c(0,1),n,replace=TRUE)
#' y <- x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + d*z + rnorm(n)
#' \donttest{A <- rd.qte(y=y,x=cbind(x,z),d=d,x0=0,z0=c(0,1),tau=tlevel,bdw=2,bias=1)}
#'
rd.qte <- function(y,x,d,x0,z0=NULL,tau,bdw,bias){
  x <- as.matrix(x)
  dz <- ncol(x)-1
  cov <- if(dz == 0) 0 else 1
  # remove missing observations
  mis <- apply(apply(cbind(y,x,d),2,is.na),1,max)
  y <- y[mis==0]; x <- as.matrix(x[mis==0,]); d <- d[mis==0]
  n <- length(y)
  bdw.opt <- if (length(bdw) == 1) 1 else if (length(bdw) == length(tau)) 2
  if(length(bdw)>1 & length(bdw)!=length(tau))
  {stop("The length of bdw should be one or equal to the length of tau.")}
  if(bdw.opt==1){
    tt <- sort(unique(c(tau,0.5)))
    hh <- bdw*((2*tt*(1-tt)/(pi*dnorm(qnorm(tt))^{2}))^{1/5})  # quantile specific bandwidths
  } else{tt <- tau; hh <- bdw
  }
  ab0 <- rdq(y,x,d,x0,z0,tau=tt,h.tau=hh,cov)
  Qp <- ab0$qp.est; Qm <- ab0$qm.est
  Qd <- ab0$qte
  if(bias==1){ # Bias estimation and correction
    tt.ext <- c(0.25,0.5)*tt[1]	# for conditional density estimation
    tt.exp <- sort(c(tt.ext,tt,(1-tt.ext)))
    ind <- tt.exp %in% tt
    if(bdw.opt==1){hh2 <- bdw*((2*tt.exp*(1-tt.exp)/(pi*dnorm(qnorm(tt.exp))^{2}))^{1/5})}
    if(bdw.opt==2){hh2 <- c(rep(hh[1],2),hh,rep(hh[1],2))}
    ab <- rdq(y,x,d,x0,z0,tau=tt.exp,h.tau=hh2,cov)
    delta = bandwidth.rq(tt,n,hs=F)
    fp <- rdq.condf(x,Q=ab$qp.est,bcoe=ab$bcoe.p,taus=tt,taul=tt.exp,delta,cov)
    fm <- rdq.condf(x,Q=ab$qm.est,bcoe=ab$bcoe.m,taus=tt,taul=tt.exp,delta,cov)
    bp <- rdq.bias(y[d==1],x[(d==1),],dz,x0,z0,taus=tt,hh2[ind],hh2[ind],fx=fp$ff[(d==1),],cov)
    bm <- rdq.bias(y[d==0],x[(d==0),],dz,x0,z0,taus=tt,hh2[ind],hh2[ind],fx=fm$ff[(d==0),],cov)
    Qp.adj <- Qp - bp$bias
    Qm.adj <- Qm - bm$bias
    Qd.adj <- Qd - (bp$bias - bm$bias)
  }
  if(bias==0){out = list(tau=tt, qte = Qd, qp.est = Qp, qm.est = Qm, cov=cov)}
  if(bias==1){out = list(tau=tt, qte = Qd.adj, qp.est = Qp.adj, qm.est = Qm.adj, cov=cov)}
  out <- append(out, list(y=y, x=x, d=d, x0=x0, z0=z0, bdw=bdw, bias=bias), after=5)
  class(out) <- c("qte", class(out))
  return(out)
}
