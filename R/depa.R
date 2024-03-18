#' Epanechnikov kernel
#'
#' @param xx the evaluation points.
#' @param loc the location parameter (normalized to be 0).
#' @param scale the scale parameter (normalized to be 1).
#'
#' @return values.
#' @export
#'
#' @examples
#' depa(0)
#' depa(seq(-1,1,by=0.05))
#'
depa <- function(xx,loc=0,scale=1){
  nx <- (xx-loc)/scale
  return((scale^{-1})*(3/4)*(1-nx^2)*(abs(nx)<1))
}
