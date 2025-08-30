## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(QTE.RD)
require(quantreg)

## ----eval = FALSE-------------------------------------------------------------
# install.packages("QTE.RD")

## -----------------------------------------------------------------------------
library(QTE.RD)

## -----------------------------------------------------------------------------
data("ddk_2011")

## -----------------------------------------------------------------------------
trk <- ddk_2011$tracking
con <- ddk_2011$etpteacher
hgh <- ddk_2011$highstream
yy  <- ddk_2011$ts_std
xx  <- ddk_2011$percentile

## -----------------------------------------------------------------------------
yc <- yy[trk==1]
xc <- xx[trk==1]
dc <- hgh[trk==1]
x0 <- 50
tlevel <- 1:9/10
hh <- 20

## -----------------------------------------------------------------------------
A <- rd.qte(y=yc,x=xc,d=dc,x0,z0=NULL,tau=tlevel,bdw=hh,bias=1)
A2 <- summary(A,alpha=0.1)
A2

## ----fig.width=8.5, fig.height=5.5, out.width="95%"---------------------------
y.text <- "test scores"
m.text <- "Effects of assignment to lower vs. upper sections"
plot(A2,ytext=y.text,mtext=m.text)

## ----fig.width=8.5, fig.height=6, out.width="95%"-----------------------------
y.text <- "test scores"
m.text <- "Conditional quantile functions"
sub.text <- c("Upper section","Lower section")
plot(A2,ptype=2,ytext=y.text,mtext=m.text,subtext=sub.text)

## -----------------------------------------------------------------------------
B <- rdq.test(y=yc,x=xc,d=dc,x0,z0=NULL,tau=tlevel,bdw=hh,bias=1,
              alpha=c(0.1,0.05),type=c(1,2,3,4),std.opt=1)
B

## -----------------------------------------------------------------------------
C <- rdq.bandwidth(y=yc,x=xc,d=dc,x0,z0=NULL,cv=1,val=(5:20),pm.each=0)
C

## -----------------------------------------------------------------------------
zc <- ddk_2011$girl[trk==1]
z.eval <- c(0,1)
A <- rd.qte(y=yc,x=cbind(xc,zc),d=dc,x0,z0=z.eval,tau=tlevel,bdw=hh,
            bias=1)
A2 <- summary(A,alpha=0.1)
A2

## ----fig.width=9, fig.height=5.8, out.width="100%"----------------------------
y.text <- "test scores"
m.text <- c("Boys","Girls")
plot(A2,ytext=y.text,mtext=m.text)

## ----fig.width=9, fig.height=5.8, out.width="100%"----------------------------
y.text <- "test scores"
m.text <- c("Boys","Girls")
sub.text <- c("Upper section","Lower section")
plot(A2,ptype=2,ytext=y.text,mtext=m.text,subtext=sub.text)

## -----------------------------------------------------------------------------
B <- rdq.test(y=yc,x=cbind(xc,zc),d=dc,x0,z0=z.eval,tau=tlevel,bdw=hh,
              bias=1,alpha=c(0.1,0.05),type=c(1,2,3,4))
B

## -----------------------------------------------------------------------------
C <- rdq.bandwidth(y=yc,x=cbind(xc,zc),d=dc,x0,z0=z.eval,cv=1,
                   val=(5:20),pm.each=0)
C

## -----------------------------------------------------------------------------
D <- rdq.band(y=yc,x=cbind(xc,zc),d=dc,x0,z0=z.eval,tau=tlevel,
              bdw=hh,alpha=0.1)
D

