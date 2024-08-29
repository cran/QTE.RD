#' Print a qte object.
#'
#' @param x object returned from \code{rd.qte}.
#' @param ... optional arguments.
#'
#' @export
#'
print.qte <- function(x,...){
  cov <- x$cov
  bias <- x$bias
  cat("\n\n")
  cat(format("QTE", width = 30, justify = "centre"), "\n")
  cat(paste(rep("-", 30), collapse = ""), "\n")
  if(bias==0) {
    cat(format("Tau", width = 15, justify = "centre"),
        format("Est.", width = 15, justify = "centre"),"\n")
  }
  if (bias==1) {
    cat(format("", width = 17), format("Bias cor.", width = 10, justify = "centre"), "\n")
    cat(format("Tau", width = 15, justify = "centre"),
        format("Est.", width = 15, justify = "centre"),"\n")
  }
  if(cov==0){
    for (i in 1:length(x$tau)) {
      cat(format(x$tau[i], width = 10, justify = "centre"),
          formatC(x$qte[i], format="f", digits = 3, width = 15),
          "\n", sep = "")
    }
  }
  if(cov==1){
    for(j in 1:ncol(x$qte)){
      cat(paste(rep("-", 30), collapse = ""), "\n")
      cat(format(paste("Group-",j,sep=""), width = 10,justify ="centre"),"\n", sep = "")
      for (i in 1:length(x$tau)) {
        cat(format(x$tau[i], width = 10, justify = "centre"),
            formatC(x$qte[i,j], format="f", digits = 3, width = 15),
            "\n", sep = "")
      }
    }
  }
}
