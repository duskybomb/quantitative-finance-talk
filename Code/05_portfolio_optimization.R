# - You can do it easily using portfolio.optim in tseries package.
# - Suppose you are considering the global portfolio with passive investment
# strategy, where you want to invest in the ETF of FTSE, DAX, SMI and CAC.
# - Your annualized expected return is 12.5%.
# - Consider annualized risk-free rate of return as 3%.

Index_Value<-as.matrix(EuStockMarkets)

## log-return in percentage terms
r<-diff(log(Index_Value))*100

## expected return
expected_return <- 12.5/252

## portfolio covariance
Sigma<-cov(r)

library(tseries)

port_optim<-portfolio.optim(r
                            ,pm=expected_return
                            ,covmat=Sigma
                            ,rf=3/252)

## optimized portfolio weights

weight<-port_optim$pw*100
names(weight)<-colnames(EuStockMarkets)
weight


## expected return
port_optim$pm

## volatility at optimized weights
port_optim$ps


### Plot the efficient frontier
er<-seq(0.045,0.075,0.001)
frontier<-matrix(NA,nrow=length(er),ncol=2)
for(i in 1:length(er)){
  port_optim<-portfolio.optim(r
                              ,pm=er[i]
                              ,covmat=Sigma
                              ,rf=3/252)
  frontier[i,]<-c(port_optim$ps,port_optim$pm)
}
plot(frontier,col="red",type = "l"
     ,xlab="volatility"
     ,ylab = "expected return")