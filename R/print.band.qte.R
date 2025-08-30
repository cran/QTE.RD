#' Print a band.qte object
#'
#' @param x object returned from \code{band.qte}.
#' @param ... optional arguments.
#' @export
#' @keywords internal
#'
print.band.qte <- function(x,...){
  cov <- x$cov
  alpha <- x$alpha
  dg <- ncol(x$qte)
  cat("\n\n")
  cat(format("QTE and Uniform Bands", width = 70, justify = "centre"), "\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat(format("", width = 20), format("Bias cor.", width = 8, justify = "centre"),
      format(paste((1-alpha)*100,"% Uniform Conf. Band",sep = ""), width = 38, justify = "centre"), "\n")
  cat(format("Tau", width = 10, justify = "centre"),
      format("Est.", width = 10, justify = "centre"),
      format("Est.", width = 8, justify = "centre"),
      format("Non-robust",width = 19, justify = "centre"),
      format("Robust",width = 19, justify = "centre") ,"\n")
  if(cov==0){
    for (i in 1:length(x$tau)) {
      cat(format(x$tau[i], width = 8, justify = "centre"),
          formatC(c(x$qte[i],x$qte.cor[i],x$uband[i,,],x$uband.robust[i,,]),format="f",digits=3,width=10),
          "\n", sep = "")
    }
  }
  if(cov==1){
    for(j in 1:dg){
      cat(paste(rep("-", 70), collapse = ""), "\n")
      cat(format(paste("Group-",j,sep=""), width = 10,justify ="centre"),"\n", sep = "")
      for (i in 1:length(x$tau)) {
        cat(format(x$tau[i], width = 8, justify = "centre"),
            formatC(c(x$qte[i,j],x$qte.cor[i,j],x$uband[i,,j],x$uband.robust[i,,j]),format="f",digits=3, width=10),"\n", sep = "")
      }
    }
  }
}
