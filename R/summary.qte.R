#' Uniform confidence band for QTE.
#'
#' @description
#' \code{summary.qte} returns uniform confidence bands and standard errors for QTE estimates.
#'
#' @param object It is an object of class "qte" produced by \code{rd.qte}.
#' @param alpha a number between 0 and 1, the desired significance level. For example,
#' when alpha=0.1, one will get a 90% uniform band.
#' @param ... optional arguments.
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
#' \item{uband.p}{uniform confidence band for conditional quantiles on the right side of \eqn{x_{0}}.}
#' \item{uband.m}{uniform confidence band for conditional quantiles on the left side of \eqn{x_{0}}.}
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
#' The Review of Economics and Statistics; https://doi.org/10.1162/rest_a_01168
#' @references Zhongjun Qu and Jungmo Yoon (2019), "Uniform Inference on Quantile Effects
#' under Sharp Regression Discontinuity Designs," Journal of Business and Economic Statistics,
#' 37(4), 625–647; https://doi.org/10.1080/07350015.2017.1407323
#'
#' @examples
#' # Without covariate
#' n <- 500
#' x <- runif(n,min=-4,max=4)
#' d <- (x > 0)
#' y <- x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' tlevel = seq(0.1,0.9,by=0.1)
#' \donttest{A <- rd.qte(y=y,x=x,d=d,x0=0,z0=NULL,tau=tlevel,bdw=2,cov=0,bias=1)}
#' \donttest{A2 <- summary(A,alpha=0.1)}
#'
#' # (continued) With covariates
#' z <- sample(c(0,1),n,replace=TRUE)
#' y <- x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + d*z + rnorm(n)
#' \donttest{A <- rd.qte(y=y,x=cbind(x,z),d=d,x0=0,z0=c(0,1),tau=tlevel,bdw=2,cov=1,bias=1)}
#' \donttest{A2 <- summary(A,alpha=0.1)}
#'
summary.qte <- function(object,alpha,...){
  # obtains uniform confidence bands and standard errors
  if(length(alpha)==0){stop("Provide alpha to obtain confidence band.")}
  tau <- object$tau
  bdw <- object$bdw
  cov <- object$cov
  bias <- object$bias
  ba  <- rdq.band(object$y,object$x,object$d,object$x0,object$z0,tau,bdw,cov,alpha)
  if(bias==0){out = list(uband = ba$uband, sigma = ba$sig, uband.p = ba$uband.p, uband.m = ba$uband.m)}
  if(bias==1){out = list(uband = ba$uband.robust, sigma = ba$sig.r, uband.p = ba$uband.robust.p, uband.m = ba$uband.robust.m)}
  out <- append(out, list(tau=tau, qte=object$qte, qp=object$qp.est, qm=object$qm.est, cov=cov, bias=bias, alpha=alpha), after=4)
  class(out) <- c("summary.qte", "qte", class(out))
  return(out)
}
