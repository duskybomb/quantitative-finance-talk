#  - Calculate expected return and volatility for all possible portfolios that 
# can be constructed by varying the portfolio weights of the assets.
#  - The set of all possible portfolios, represented by their expected return 
# and volatilities has the characteristic shape.
#  - Considers 10000 portfolios, where portfolio weights are randomly simulated 
# and corresponding portfolio return.
#  - Consider the global portfolio with passive investment strategy, where you 
# want to invest in the ETF of FTSE, DAX, SMI and CAC and use the data in EuStockMarkets dataset.

library(tseries)

Index_Value<-as.matrix(EuStockMarkets)
r<-diff(log(Index_Value))*100
no.of.portf<-10000
set.seed(1)
sigma<-mu<-rep(NA,no.of.portf)
for(i in 1:no.of.portf){
  w <- sample(1:1000,4,replace=T)
  w <- w/sum(w) ## weight for i-th portfolio
  rp <- r%*%w   ## returns of i-th portfolio
  mu[i] <- mean(rp)  ## mean return of i-th portfolio
  sigma[i] <- sd(rp) ## volatility of i-th portfolio
}

##################################

plot(sigma,mu,xlab = "volatility"
     ,ylab="expected return",col="grey")
abline(h=0.065,col="red",lwd=2)
segments(0.8,0.04,0.8,0.065,col="blue",lwd=2)
segments(0.85,0.04,0.85,0.065,col="green",lwd=2)
arrows(0.775,0.07,0.8,0.065,col="black",lwd=2)
arrows(0.875,0.07,0.85,0.065,col="black",lwd=2)
text(0.77,0.071,"w1")
text(0.88,0.071,"w2")
points(0.779,0.065,col="blue",lwd=2)
text(0.779,0.066,"w")
points(0.81,0.07,col="blue",lwd=2)
text(0.81,0.0715,"v")

### Plot the efficient frontier
Sigma<-cov(r)

er<-seq(0.045,0.075,0.001)
frontier<-matrix(NA,nrow=length(er),ncol=2)
for(i in 1:length(er)){
  port_optim<-portfolio.optim(r
                              ,pm=er[i]
                              ,covmat=Sigma)
  frontier[i,]<-c(port_optim$ps,port_optim$pm)
}
lines(frontier,col="red")
