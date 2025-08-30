#' tests for QTE
#' @description
#' \code{rdq.test} provides testing results for hypotheses on the treatment effects concerning (i) treatment significance, (ii) homogeneity of effects over quantiles,
#' and (iii) positive or negative dominance hypothesis.
#'
#' @usage rdq.test(y,x,d,x0,z0=NULL,tau,bdw,bias,alpha=0.1,type=1,std.opt=1)
#'
#' @param y a numeric vector, the outcome variable.
#' @param x a vector (or a matrix) of covariates.
#' When no covariates are included, \eqn{x} is simply a vector of the running variable
#' and \eqn{z0} can be left unspecified. When covariates are included,
#' \eqn{x} should be a matrix with the running variable in the first column and
#' the covariates in the remaining columns.
#' @param d a numeric vector, the treatment status.
#' @param x0 the cutoff point.
#' @param z0 the value of the covariates at which to evaluate the effects.
#' For example, if a female dummy is included, \emph{z0 = 1} indicates the female subgroup.
#' @param tau a vector of quantiles of interest.
#' @param bdw the bandwidth value(s). If `bdw` is a scalar, it is interpreted as the
#' bandwidth for the median. The bandwidths for the rest of the quantiles are
#' computed automatically using the formula in Yu and Jones (1998).
#' If it is a vector with the same dimension as `tau`,
#' the function will use these values for the respective quantiles accordingly.
#' @param bias either 0 or 1. If bias=1, the QTE estimate is bias corrected and
#' the robust confidence band in Qu, Yoon, and Perron (2024) is produced.
#' If \emph{bias=0}, no bias correction is implemented.
#' @param alpha a numeric value between 0 and 1 specifying the significance level.
#' For example, setting `alpha = 0.1` yields a 90% uniform confidence band.
#' Multiple significance levels can be specified, e.g., `alpha = c(0.1, 0.05)`.
#' @param type a value in 1--4. Set \emph{type} to 1 to test the null hypothesis of a zero
#' treatment effect against the alternative hypothesis of significant treatment effects;
#' set \emph{type} to 2 to test the null hypothesis of homogeneous treatment against heterogeneous treatment effects;
#' set \emph{type} to 3 to test the null hypothesis of uniformly non-negative treatment effects against the presence of negative effects;
#' and set \emph{type} to 4 to test the null hypothesis of uniformly non-positive treatment effects against the presence of positive effects at some quantiles.
#' @param std.opt either 0 or 1. If `std.opt=1`, the test statistic is standardized so that
#' the variance is equalized across quantiles; if `std.opt=0`, the test is not standardized.
#' @return
#' A list with elements:
#' \describe{
#' \item{statistic}{test statistics.}
#' \item{cr.va}{critical values.}
#' \item{p.value}{p values.}
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
#' B = rdq.test(y=y,x=x,d=d,x0=0,z0=NULL,tau=tlevel,bdw=2,bias=1,alpha=c(0.1,0.05),type=c(1,2,3))
#'
#' # (continued) With covariates
#' z = sample(c(0,1),n,replace=TRUE)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + d*z + rnorm(n)
#' \donttest{B = rdq.test(y=y,x=cbind(x,z),d=d,x0=0,z0=c(0,1),tau=tlevel,bdw=2,bias=1,
#' alpha=c(0.1,0.05),type=c(3,4))}
#'
#'
rdq.test <- function(y,x,d,x0,z0=NULL,tau,bdw,bias,alpha=0.1,type=1,std.opt=1){
  x <- as.matrix(x)
  dz <- ncol(x)-1
  cov <- if(dz == 0) 0 else 1
  # remove missing observations
  mis <- apply(apply(cbind(y,x,d),2,is.na),1,max)
  y <- y[mis==0]; d <- d[mis==0]
  x <- x[mis == 0, , drop=FALSE]
  n <- length(y)
  if(length(alpha)==0){stop("Provide values for alpha.")}
  if(length(type)==0){stop("The option type cannot be empty.")}
  # Group setups
  w <- if(cov==0) NULL else as.matrix(x[, -1])
  dg <- if(cov==0) 1 else if(dz==1) length(z0) else nrow(z0)
  n.sim <- ifelse((n<50000),1000,500)	# simulation repetitions for uniform bands
  # quantile levels and bandwidths
  bdw.opt <- if (length(bdw) == 1) 1 else if (length(bdw) == length(tau)) 2
  if(length(bdw) > 1 & length(bdw)!=length(tau))
  {stop("The length of bdw should be one or equal to the length of tau.")}
  if (bdw.opt==1) {
    tt <- sort(unique(c(tau,0.5)))	# qualtile levels to estimate
    hh <- bdw*((2*tt*(1-tt)/(pi*dnorm(qnorm(tt))^{2}))^{1/5})  # quantile specific bandwidths
  } else {tt <- tau; hh <- bdw
  }
  tt.ext <- c(0.25,0.5)*tt[1]		# for conditional density estimation
  tt.exp <- sort(c(tt.ext,tt,(1-tt.ext)))
  ind <- tt.exp %in% tt
  if(bdw.opt==1){hh2 <- bdw*((2*tt.exp*(1-tt.exp)/(pi*dnorm(qnorm(tt.exp))^{2}))^{1/5})}
  if(bdw.opt==2){hh2 <- c(rep(hh[1],2),hh,rep(hh[1],2))}
  # obtains QTE and test statistics
  ab <- rdq(y,x,d,x0,z0,tau=tt.exp,h.tau=hh2,cov)
  # Bias estimation
  delta <- bandwidth.rq(tt,n,hs=FALSE)
  fp <- rdq.condf(x,Q=ab$qp.est,bcoe=ab$bcoe.p,taus=tt,taul=tt.exp,delta,cov)
  fm <- rdq.condf(x,Q=ab$qm.est,bcoe=ab$bcoe.m,taus=tt,taul=tt.exp,delta,cov)
  bp <- rdq.bias(y[d==1],x[(d==1),],dz,x0,z0,taus=tt,hh2[ind],hh2[ind],fx=fp$ff[(d==1),],cov)
  bm <- rdq.bias(y[d==0],x[(d==0),],dz,x0,z0,taus=tt,hh2[ind],hh2[ind],fx=fm$ff[(d==0),],cov)
  # tests for QTE
  sm <- rdq.sim(x,d,x0,z0,dz,cov,tt=tt,hh2[ind],hh2[ind],fxp=fp$ff,fxm=fm$ff,n.sim)
  te.st <- list()
  cr.va <- list()
  p.val <- list()
  for(j in seq_along(type)){
    bt <- run.test(n,dz,taus=tt,hh2[ind],Dc.p=sm$dcp,Dc.m=sm$dcm,Dr.p=sm$drp,Dr.m=sm$drm,Qy.p=as.matrix(ab$qp.est[ind,,drop=FALSE]),Qy.m=as.matrix(ab$qm.est[ind,,drop=FALSE]),bias.p=bp$bias,bias.m=bm$bias,cov,bias,alpha,n.sim,test.type=type[j],std.opt)
    # save results by type
    switch(
      as.character(type[j]),
      `1` = {te.st$significance = bt$test.stat; cr.va$significance = bt$cr.value; p.val$significance = bt$p.val},
      `2` = {te.st$homogeneity = bt$test.stat; cr.va$homogeneity = bt$cr.value; p.val$homogeneity = bt$p.val},
      `3` = {te.st$unambiguity = bt$test.stat; cr.va$unambiguity = bt$cr.value; p.val$unambiguity = bt$p.val},
      `4` = {te.st$ne.unambiguity = bt$test.stat; cr.va$ne.unambiguity = bt$cr.value; p.val$ne.unambiguity = bt$p.val},
      stop("Unknown test type: ", type[j])
    )
  }
  out <- list(statistic = te.st, cr.value = cr.va, p.value = p.val, type=type, cov=cov, dg=dg, alpha=alpha)
  class(out) <- c("test.qte", class(out))
  return(out)
}
