## Simulate from Geometric Brownian Motion

library(tseries)
library(zoo)

#----------Use EuStockMarket-------------

FTSE<-EuStockMarkets[,"FTSE"]
Asset<-FTSE
sim.size<-500
n<-length(Asset)
rt<-diff(log(Asset))
rbar<-mean(rt)
s<-sd(rt)
delta_t<-1
mu_hat<-rbar+s^2/2
rt.sim<-rnorm(sim.size,mean=(mu_hat-s^2/2),sd=s)
Asset.sim<-rep(NA,sim.size)
Asset.sim[1]<-Asset[n]*exp(rt.sim[1])
for(i in 2:sim.size){
  Asset.sim[i]<-Asset.sim[i-1]*exp(rt.sim[i])
}

yl<-min(Asset)*0.85
yu<-max(Asset)*1.9
plot(ts(Asset),xlim=c(0,(n+sim.size)),ylim=c(yl,yu))
lines((n+1):(n+sim.size),Asset.sim,col="red",lwd=2)
grid(col="black",lwd=2)