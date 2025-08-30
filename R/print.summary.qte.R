#' Print a summary.qte object
#'
#'
#' @param x object returned from \code{summary.qte}.
#' @param ... optional arguments.
#' @export
#' @keywords internal
#'
print.summary.qte <- function(x,...){
  cov <- x$cov
  bias <- x$bias
  alpha <- x$alpha
  cat("\n\n")
  cat(format("QTE", width = 70, justify = "centre"), "\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  if(bias==0) {
    cat(format("", width = 20),
        format("Pointwise", width = 18, justify = "centre"),
        format("Uniform", width = 18, justify = "centre"), "\n")
    cat(format("Tau", width = 12, justify = "centre"),
        format("Est.", width = 10, justify = "centre"),
        format("Std.Err", width = 14, justify = "centre"),
        format(paste((1 - alpha) * 100, "% Conf. Band",sep = ""),
               width = 16, justify = "centre"),"\n")
  }
  if (bias==1) {
    cat(format("", width = 12), format("Bias cor.", width = 8, justify = "centre"),
        format("Pointwise", width = 16, justify = "centre"),
        format("Uniform", width = 16, justify = "centre"), "\n")
    cat(format("Tau", width = 12, justify = "centre"),
        format("Est.", width = 10, justify = "centre"),
        format("Robust S.E.", width = 14, justify = "centre"),
        format(paste((1 - alpha) * 100, "% Conf. Band",sep = ""),
               width = 16, justify = "centre"),"\n")
  }
  if(cov==0){
    for (i in 1:length(x$tau)) {
      cat(format(x$tau[i], width = 8, justify = "centre"),
          formatC(c(x$qte[i],x$sigma[i],x$uband[i,,]), format="f", digits = 3, width = 12),
          "\n", sep = "")
    }
  }
  if(cov==1){
    for(j in 1:ncol(x$qte)){
      cat(paste(rep("-", 70), collapse = ""), "\n")
      cat(format(paste("Group-",j,sep=""), width = 10,justify ="centre"),"\n", sep = "")
      for (i in 1:length(x$tau)) {
        cat(format(x$tau[i], width = 8, justify = "centre"),
            formatC(c(x$qte[i,j],x$sigma[i,j],x$uband[i,,j]), format="f", digits = 3, width = 12),
            "\n", sep = "")
      }
    }
  }
}
