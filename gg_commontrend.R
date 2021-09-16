
### GG.COMMONTREND ####

GG.ComT <- function (data, rank, k)
{ 
  
  begin= k+1
  max= length(rownames(data))
  width=length(colnames(data))
  length=(max-k)
  
  cotest = ca.jo(data, type = "trace", ecdet = "none", K=k,    spec="transitory")
  equation = cajorls(cotest, r = rank , reg.number = NULL)
  
  alpha=matrix(NA,width,rank)
  alpha [,1:rank] =equation$rlm$coefficients[1:rank,1:width]####otherwise, when rank is one, the data structure will change 
  oalpha <- qr.Q(qr(alpha),complete=TRUE) [ ,(rank+1):width] #### (width-rank) is the number of common trends
  
  beta= equation$beta
  obeta <- qr.Q(qr(beta),complete=TRUE)[,(rank+1):width]
  
  loading=obeta %*% ginv (t(oalpha)%*%obeta)
  
  Pure.T= matrix(NA,(width-rank),(max-k) )
  Pure.T[1:(width-rank),]= t(oalpha) %*% t (data[begin:max,])
  Com.T= loading%*%Pure.T
  
  station.p = alpha %*% ginv ( t(beta)%*% alpha) %*% t(beta) %*% t (data[begin:max,])
  
  result=list (method="Gonzalo and Grange(1995)",length = length, lag.chosen=k,
               beta = beta, othog.beta = obeta, alpha=alpha,othog.alpha=oalpha,
               common.trend=Com.T,  pure.trend=Pure.T, 
               loading.vector= loading,  stationary=station.p, data.used=t(data[(k+1):max,]) )
  
  class (result) <- "ComT"
  return(result)
}

plotComT <-function(ComT, i, 
                    x.axis=NA, approx.ticks=7,
                    legend=c("Original Data","Common Trend"), 
                    main="", ylab="", xlab="") 
  
{  if(!class(ComT)=="ComT") stop ("Class must be 'ComT' ")
  
  ComTD=ComT$common.trend[i,]
  Origin=ComT$data.used[i,]
  max=length(ComTD) 
  
  plot (Origin,type="l",ylab=ylab,xlab=xlab, axes  =  FALSE)
  
  if (length(x.axis) == (max+ComT$lag.chosen) ) { 
    xlable=paste (x.axis[(length(x.axis)-max+1):length(x.axis)])
    ticks=seq(1,max, by=round(max/approx.ticks))
    axis(1,at=ticks,labels= xlable[ticks], cex.axis=0.7) 
  }else if (is.na(x.axis[1])==TRUE) {
    axis(1)
  }else if (!length(x.axis) == (max+ComT$lag.chosen) ){ 
    stop("Length of x.axis must be the same as length of original data" ) 
  } 
  
  axis(2)
  title(main=main)
  lines (ComTD+mean(ComT$stationary[i,]),lwd=2,col="BLUE") 
  
  position=max(Origin)
  legend(0,position,legend,lwd=1:2,col=c("Black","BLUE"),bty ="n")
  box()
}

## Plot the Common Trend
plot1=plotComT(pt_all,1,x.axis=NA, approx.ticks=20,
               legend=c("Original Data (ADA)","Common Trend"),
               main="Common Trend(s) from Benchmark Markets: CARDANO (ADA)",
               ylab="Price", xlab="Time")

pt_bn_cb = GG.ComT(ts_log_bn_cb, 1, k1_bn_cb)
pt_bn_kk = GG.ComT(ts_log_bn_kk, 1, k1_bn_kk)
pt_cb_kk = GG.ComT(ts_log_cb_kk, 1, k1_cb_kk)


pt_all = GG.ComT(ts_log, 1, k1_cb_kk)
