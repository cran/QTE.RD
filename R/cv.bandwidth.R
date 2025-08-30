#' Cross-validation bandwidth
#' @description
#' \code{cv.bandwidth} implements the cross-validation bandwidth selection rule. The function \code{rdq.bandwidth} calls this function to obtain the CV bandwidth.
#'
#' @param y a numeric vector, the outcome variable.
#' @param x the running variable.
#' @param z additional covariates.
#' @param dz the number of covariates z.
#' @param x0 the cutoff point.
#' @param val a set of candidate values for the CV bandwidth.
#' @param xl if \emph{xl=0.5}, the CV bandwidth is computed using the 50% of observations closest to \eqn{x_0}.
#' @param order either 1 or 2. When \emph{order=1}, a local linear regression is used, and
#' when \emph{order=2}, a local quadratic regression is used.
#' @param bdy either 0 or 1. When \emph{bdy=1}, the CV bandwidth is computed by
#' treating x as a boundary point. Otherwise, x is treated as an interior point.
#'
#' @return
#' A list with elements:
#' \describe{
#' \item{h.cv}{the selected CV bandwidth values at the median.}
#' \item{cand}{values of the criterion function evaluated at each of candidate value.}
#' }
#' @export
#' @keywords internal
#' @seealso [rdq.bandwidth]
#' @importFrom stats quantile
#' @references Zhongjun Qu, Jungmo Yoon, Pierre Perron (2024), "Inference on Conditional Quantile
#' Processes in Partially Linear Models with Applications to the Impact of Unemployment Benefits,"
#' The Review of Economics and Statistics; https://doi.org/10.1162/rest_a_01168
#' @references Zhongjun Qu and Jungmo Yoon (2019), "Uniform Inference on Quantile Effects
#' under Sharp Regression Discontinuity Designs," Journal of Business and Economic Statistics,
#' 37(4), 625–647; https://doi.org/10.1080/07350015.2017.1407323
#'
#' @examples
#' n = 500
#' x = runif(n,min=-4,max=4)
#' d = (x > 0)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' cv.bandwidth(y=y, x=x, z=NULL, dz=0, x0=0, val=c(1,2,3,4), xl=0.5, order=2, bdy=1)
#' cv.bandwidth(y=y, x=x, z=NULL, dz=0, x0=0, val=c(1,2,3,4), xl=0.5, order=1, bdy=1)
#'
cv.bandwidth <- function(y, x, z, dz, x0, val, xl, order, bdy){
  x.u <- sort(unique(x))
  wdt <- quantile(abs(x-x0),probs=xl)
  x.m <- x.u[(abs(x.u-x0) < wdt) & x.u!=x0]
  if(dz>=1){
    z <- as.matrix(z)
    sel <- if(order==1) (2+1):(2 + dz) else (3+1):(3 + dz)
  }
  cri <- array(0,c(length(val),1))
  for (i in 1:length(val)){
    h.tau <- val[i]
    cri.h <- NULL
    for (j in 1:length(x.m)){
      xe <- x.m[j]
      if(bdy==1){
        if(xe>=x0){sgn <-((x.u > x0)&(x.u > xe)&(x.u <= xe+h.tau))}
        if(xe< x0){sgn <-((x.u < x0)&(x.u < xe)&(x.u >= xe-h.tau))}
      }
      if(bdy==0){
        if(xe>=x0){sgn <-((x.u > x0)&(x.u!=xe)&(x.u <= xe+h.tau)&(x.u >= xe-h.tau))}
        if(xe< x0){sgn <-((x.u < x0)&(x.u!=xe)&(x.u <= xe+h.tau)&(x.u >= xe-h.tau))}
      }
      index <- x %in% x.u[sgn]
      xx <- x[index] - xe
      yy <- y[index]
      ww <- depa(xx/val[i])
      if(order==1){ # local linear regression
        xs <- if(dz == 0) xx else cbind(xx, z[index,], xx*z[index,])
      }
      if(order==2){ # local quadratic regression
        if(dz==0){xs <- cbind(xx,(xx^2))}
        if(dz!=0){xs <- cbind(xx,(xx^2),z[index,],(xx*z[index,]),((xx^2)*z[index,]))}
      }
      coe <- rq(yy ~ xs, method="fn", tau = 0.5, weights = ww)$coef
      index2 <- (x==xe)
      if(dz==0) {Qy <- coe[1]}
      if(dz==1) {Qy <- coe[1] + coe[sel]*z[index2,]}
      if(dz >1) {Qy <- coe[1] + (z[index2,] %*% coe[sel])}
      cri.h <- c(cri.h,sum(abs(y[index2]-Qy)))
    }
    cri[i] <- mean(cri.h)
  }
  return(list(h.cv = val[which.min(cri)], cand = cbind(val,cri)))
}
