#' Print a bw.qte object
#'
#' @param x object returned from \code{bandwidth.qte}.
#' @param ... optional arguments.
#' @export
#' @keywords internal
#'
print.bw.qte <- function(x,...){
  cat("\n\n")
  cat(format("Selected Bandwidths", width = 60, justify = "centre"), "\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  cat(format("Method", width = 26), "\t", format("Values", width = 20, justify ="centre"), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat(format("Cross Validation", width = 26), "\t",
      format(x$cv, digits = 3, width = 12, justify ="centre"),"\n")
  if(x$cov==0){
    cat(format("MSE Optimal", width = 26), "\t",
        format(c(x$opt.m,x$opt.p), digits = 3, width=12, justify ="centre"),"\n")
  }
  if(x$cov==1){
    for(j in 1:x$dg){
      cat(format(paste("MSE Optimal,","Group-",j,sep=""), width = 26), "\t",
          format(c(x$opt.m[j,],x$opt.p[j,]), digits = 3, width=12, justify ="centre"),"\n")
    }
  }
}
