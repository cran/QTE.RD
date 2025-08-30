#' Print a test.qte object
#'
#'
#' @param x object returned from \code{test.qte}.
#' @param ... optional arguments.
#' @export
#' @keywords internal
#'
print.test.qte <- function(x,...){
  te.st <- x$statistic
  cr.va <- x$cr.value
  p.val <- x$p.value
  type  <- x$type
  alpha <- x$alpha
  cov  <- x$cov
  dg   <- x$dg
  cat("\n\n")
  cat(format("Testing hypotheses on quantile process",
             width = 80, justify = "centre"), "\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  cat(format("NULL Hypthoesis", width = 38), "\t",
      format("test stat.", width = 12, justify = "centre"),
      format("critical value", width = 12, justify = "centre"),
      format("p value", width = 12, justify = "centre"), "\n")
  cat(format("",width = 55, collapse = ""),
      format(paste((100*alpha),"%","      ",sep=""), justify = "left"),"\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  if(cov==0){
    if(1 %in% type){
      cat(format("Significance: QTE(tau|x,z)=0 for all taus ", width = 42),
          format(te.st$significance, digits = 2, nsmall = 2, width = 8),
          format(cr.va$significance, digits = 2, nsmall = 2, width = 8),
          format(p.val$significance, digits = 2, nsmall = 2, width = 8), "\n")
    }
    if(2 %in% type){
      cat(format("Homogeneity: QTE(tau|x,z) is constant ", width = 42),
          format(te.st$homogeneity, digits = 2, nsmall = 2, width = 8),
          format(cr.va$homogeneity, digits = 2, nsmall = 2, width = 8),
          format(p.val$homogeneity, digits = 2, nsmall = 2, width = 8), "\n")
    }
    if(3 %in% type){
      cat(format("Dominance: QTE(tau|x,z)>=0 for all taus ", width = 42),
          format(te.st$unambiguity, digits = 2, nsmall = 2, width = 8),
          format(cr.va$unambiguity, digits = 2, nsmall = 2, width = 8),
          format(p.val$unambiguity, digits = 2, nsmall = 2, width = 8), "\n")
    }
    if(4 %in% type){
      cat(format("Dominance: QTE(tau|x,z)<=0 for all taus ", width = 42),
          format(te.st$ne.unambiguity, digits = 2, nsmall = 2, width = 8),
          format(cr.va$ne.unambiguity,digits = 2, nsmall = 2, width = 8),
          format(p.val$ne.unambiguity,digits = 2, nsmall = 2, width = 8), "\n")
    }
  }
  if(cov==1){
    for(j in 1:dg){
      if(j>1){cat(paste(rep("-", 80), collapse = ""), "\n")}
      cat(format(paste("Group-",j,sep=""), width = 10,justify ="centre"),"\n", sep = "")
      if(1 %in% type){
        cat(format("Significance: QTE(tau|x,z)=0 for all taus ", width = 42),
            format(te.st$significance[j,], digits = 2, nsmall = 2, width = 8),
            format(cr.va$significance[j,], digits = 2, nsmall = 2, width = 8),
            format(p.val$significance[j,], digits = 2, nsmall = 2, width = 8), "\n")
      }
      if(2 %in% type){
        cat(format("Homogeneity: QTE(tau|x,z) is constant ", width = 42),
            format(te.st$homogeneity[j,], digits = 2, nsmall = 2, width = 8),
            format(cr.va$homogeneity[j,], digits = 2, nsmall = 2, width = 8),
            format(p.val$homogeneity[j,], digits = 2, nsmall = 2, width = 8), "\n")
      }
      if(3 %in% type){
        cat(format("Dominance: QTE(tau|x,z)>=0 for all taus ", width = 42),
            format(te.st$unambiguity[j,], digits = 2, nsmall = 2, width = 8),
            format(cr.va$unambiguity[j,], digits = 2, nsmall = 2, width = 8),
            format(p.val$unambiguity[j,], digits = 2, nsmall = 2, width = 8), "\n")
      }
      if(4 %in% type){
        cat(format("Dominance: QTE(tau|x,z)<=0 for all taus ", width = 42),
            format(te.st$ne.unambiguity[j,], digits = 2, nsmall = 2, width = 8),
            format(cr.va$ne.unambiguity[j,],digits = 2, nsmall = 2, width = 8),
            format(p.val$ne.unambiguity[j,],digits = 2, nsmall = 2, width = 8), "\n")
      }
    }
  }
}
