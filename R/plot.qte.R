#' QTE plots
#' @description
#' \code{plot.qte} generates plots summarizing the QTE estimates and their uniform confidence bands, helping users visualize the results.
#' It also makes plots for conditional quantile processes for each side of the cutoff.
#'
#' @param x an object of class "qte" or "summary.qte" produce by \code{rd.qte}.
#' @param ptype either 1 or 2. Set \emph{ptype=1} for the QTE plots, and
#' \emph{ptype=2} for the conditional quantile plots. The default value is 1.
#' @param ytext the y-axis label.
#' @param mtext the title of the plot.
#' @param subtext the subtitles (used for the conditional quantile plots only).
#' @param ... optional arguments to plot
#'
#' @return plot(s) of the QTE estimates and uniform confidence bands.
#' @export
#' @import plotrix
#' @importFrom graphics axis layout legend lines par polygon title
#' @importFrom stats runif
#'
#' @examples
#' # Without covariate
#' n = 500
#' x = runif(n,min=-4,max=4)
#' d = (x > 0)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' tlevel = seq(0.1,0.9,by=0.1)
#' \donttest{A <- rd.qte(y=y,x=x,d=d,x0=0,z0=NULL,tau=tlevel,bdw=2,cov=0,bias=1)}
#' \donttest{plot(A)}
#'
#' y.text = "test scores"
#' m.text = "QTE and Uniform band"
#' \donttest{plot(A,ytext=y.text,mtext=m.text)}
#'
#' \donttest{A2 <- summary(A,alpha=0.1)}
#' \donttest{plot(A2)}
#'
# (continued) With covariates
#' z = sample(c(0,1),n,replace=TRUE)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + d*z + rnorm(n)
#' \donttest{A <- rd.qte(y=y,x=cbind(x,z),d=d,x0=0,z0=c(0,1),tau=tlevel,bdw=2,cov=1,bias=1)}
#' \donttest{A2 <- summary(A,alpha=0.1)}
#'
#' y.text = "test scores"
#' m.text = c("D=0","D=1")
#' \donttest{plot(A2,ytext=y.text,mtext=m.text)}
#'
#' # conditional quantile plots
#' n = 500
#' x = runif(n,min=-4,max=4)
#' d = (x > 0)
#' y = x + 0.3*(x^2) - 0.1*(x^3) + 1.5*d + rnorm(n)
#' tlevel = seq(0.1,0.9,by=0.1)
#' \donttest{A <- rd.qte(y=y,x=x,d=d,x0=0,z0=NULL,tau=tlevel,bdw=2,cov=0,bias=1)}
#' \donttest{A2 <- summary(A,alpha=0.1)}
#'
#' y.text = "test scores"
#' m.text = "Conditional quantile functions"
#' sub.text = c("D=0 group","D=1 group")
#' \donttest{plot(A2,ptype=2,ytext=y.text,mtext=m.text,subtext=sub.text)}
#'
#'
plot.qte <- function(x,ptype=1,ytext=NULL,mtext=NULL,subtext=NULL,...){
  if(!(ptype %in% c(1,2))){stop("The option 'ptype' should be either 1 or 2.")}
  if(ptype==1){qte <- x$qte; band <- x$uband}
  if(ptype==2){qp <- x$qp; qm <- x$qm; bandp <- x$uband.p; bandm <- x$uband.m}
  tau <- x$tau; cov <- x$cov
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  cls.type <- ("summary.qte" %in% class(x)) # if 1, point estimates + uniform bands
  # QTE plots
  if(ptype==1){
    if(cls.type==0){band <- NULL}
    if(cov==0){
      ran <- cbind(qte,band[,,1])
      inc <- (max(ran)-min(ran))*0.1
      y.ran <- c((min(ran)-inc),(max(ran)+inc))
      plot(tau,qte,ylim=y.ran,xaxt="n",xlab="Quantile Index",ylab=ytext,type="l",lty=1,cex.lab=1.3,cex.axis=1.4)
      axis(1,at=tau,labels=tau,cex.axis=1.4)
      title(main=paste(mtext," ",sep=""))
      lines(tau,rep(0,length(tau)),lwd=0.7)
      polygon(rbind(cbind(tau,band[,1,1]),cbind(rev(tau),rev(band[,2,1]))),col="#64646437", border="grey", pch=16,cex=.5)
    }
    if(cov==1){
      b2 <- band
      if(cls.type==1){dim(b2) <- c(nrow(band),(2*dim(band)[3]))}
      ran <- cbind(qte,b2)
      inc <- (max(ran)-min(ran))*0.1
      y.ran <- c((min(ran)-inc),(max(ran)+inc))
      dg <- dim(qte)[2]
      if(length(mtext)==0){mtext = sprintf("Group%d",seq(1:dg))}
      if(dg==2){layout(matrix(c(1,2), nrow=1, ncol=2, byrow = TRUE), widths=c(3,3), heights=c(1,1))}
      if((dg>2)|(dg %% 2==0)){par(mfrow=c((dg/2),2),oma=c(5,4,0,0)+0.1,mar=c(2,0,1,1)+0.1)}
      if(dg==3){layout(mat = matrix(c(1,1,2,2,0,3,3,0),nrow = 2, byrow = T)); par(mar = c(4, 4, 1, 2))}
      if(dg==5){par(mfrow = c(3,2),oma = c(4,4,0,0) + 0.1,mar = c(2,1,2,1) + 0.1)}
      if(dg==9){par(mfrow = c(3,3),oma = c(4,4,0,0) + 0.1,mar = c(2,1,2,1) + 0.1)}
      for(i in 1:dg){
        plot(tau,qte[,i],ylim=y.ran,xaxt="n",type="l",xlab="",ylab="",lty=1)
        axis(1, at=tau, labels=tau,cex.axis=1.2)
        title(main=mtext[i],cex.main=1.4)
        lines(tau,rep(0,length(tau)),lwd=0.7)
        polygon(rbind(cbind(tau,band[,1,i]),cbind(rev(tau),rev(band[,2,i]))),col="#64646437", border="grey", pch=16,cex=.5)
      }
      title(xlab = "Quantile Index",ylab = ytext, outer = TRUE, line = 2,cex.lab=1.3)
    }
  }
  # conditional quantile plots
  if(ptype==2){
    if(cls.type==0){bandp <- NULL; bandm <- NULL}
    if(length(qm)==0){q1 <- qp; band1 <- bandp; q2 <- NULL; band2 <- NULL}
    if(length(qp)==0){q1 <- qm; band1 <- bandm; q2 <- NULL; band2 <- NULL}
    if(length(qp)>0 & length(qm)>0){q1 <- qp; q2 <- qm; band1 <- bandp; band2 <- bandm}
    ran <- cbind(q1,q2,band1[,,1],band2[,,1])
    inc <- (max(ran)-min(ran))*0.1
    y.ran <- c((min(ran)-inc),(max(ran)+inc))
    if(cov==0){
      plot(tau,q1,xlab="Quantile Index",ylim=y.ran,ylab=ytext,type="l")
      polygon(rbind(cbind(tau,band1[,1,1]),cbind(rev(tau),rev(band1[,2,1]))),col="#64646437", border="grey", pch=16,cex=.5)
      lines(tau,q2,col="blue",lty=1)
      polygon(rbind(cbind(tau,band2[,1,1]),cbind(rev(tau),rev(band2[,2,1]))),col="#1B98E026", border="grey", pch=16,cex=.5)
      title(main=mtext,cex.main=1.4)
      if(length(subtext)>0){
        legend(0.4,quantile(ran,prob=0.1),legend=subtext,col=c("black","blue"),lty=c(1,1))
      }
    }
    if(cov==1){
      dg <- dim(q1)[2]
      if(length(mtext)==0){mtext <- sprintf("Group%d",seq(1:dg))}
      if(dg==2){par(mfrow=c(1,2))}
      if((dg>2)|(dg %% 2==0)){par(mfrow=c((dg/2),2),oma=c(5,4,0,0)+0.1,mar=c(2,0,1,1)+0.1)}
      if(dg==3){layout(mat = matrix(c(1,1,2,2,0,3,3,0),nrow = 2, byrow = T)); par(mar = c(4, 4, 1, 2))}
      if(dg==5){par(mfrow = c(3,2),oma = c(4,4,0,0) + 0.1,mar = c(2,1,2,1) + 0.1)}
      if(dg==9){par(mfrow = c(3,3),oma = c(4,4,0,0) + 0.1,mar = c(2,1,2,1) + 0.1)}
      for(j in 1:dg){
        plot(tau,q1[,j],ylim=y.ran,ylab="",xlab="",type="l")
        polygon(rbind(cbind(tau,band1[,1,j]),cbind(rev(tau),rev(band1[,2,j]))),col="#64646437", border="grey", pch=16,cex=.5)
        lines(tau,q2[,j],col="blue",lty=2)
        polygon(rbind(cbind(tau,band2[,1,j]),cbind(rev(tau),rev(band2[,2,j]))),col="#1B98E026", border="grey", pch=16,cex=.5)
        title(main=mtext[j],cex.main=1.4)
        if(length(subtext)>0){
          legend(0.4,quantile(ran,prob=0.1),legend=subtext,col=c("black","blue"),lty=c(1,2))
        }
      }
      title(xlab = "Quantile Index",ylab = ytext, outer = TRUE, line = 2,cex.lab=1.3)
    }
  }
}
