# Suppose price of a stock move up or down with probability 0.5 or 
# Size of the movement follow $Poisson(\lambda=5)$
# If the price of the stock is Re 1/-; then what will be the price of the stock after 21600 seconds
# The model:
#  $$
#  P_t=P_{t-1}+\pm M_t,
#  $$
#  where $M_t \sim Poisson(\lambda=5)$


set.seed(321)
n<-21600
P<-M<-rep(NA,n)
P[1]<-100 ## Current price 100 paisa or Re 1/-
for(sec in 2:n){
  toss<-sample(c("H","T"),1,replace = TRUE,prob = c(0.5,0.5))
  M[sec]<-rpois(1,lambda = 5)
  if(toss=="H")P[sec]<-P[sec-1]+ M[sec]
  if(toss=="T")P[sec]<-P[sec-1]- M[sec]
}
par(mfrow=c(1,2))
plot(ts(P))
abline(h=0,lwd=2,col="red")
plot(ts(M))
abline(h=0,lwd=2,col="red")


# Example 3: Random Walk with Random Return

# Simple return of an asset is nothing but movement of the price with respect to previous price.
# Suppose $R_t\sim N(\mu=0,\sigma=0.01)$ on every seconds.
# If the price of the stock is Re 1/-; then what will be the price of the stock after 21600 seconds

set.seed(321)
n<-21600
P<-rep(NA,n)
P[1]<-100 ## Current price 100 paisa or Re 1/-

rt<-rnorm(n,mean=0.0001,sd=0.01)
for(sec in 2:n) P[sec]<-P[sec-1]*(1+rt[sec])

par(mfrow=c(1,2))
plot(ts(P))
abline(h=0,lwd=2,col="red")
plot(ts(rt))
abline(h=0,lwd=2,col="red")

## Simulation from stationary process
mu<-0
s<-1
n<-100
r<-rnorm(n,mu,s)
plot(ts(r))

## Price from the rates
P<-r
P[1]<-1
P<-cumsum(P)
plot(ts(P))
## Price may be non-stationary. However, its steps could be a stationary process.

### Random Walk Hypothesis

library(tseries)
head(EuStockMarkets)
FTSE<-EuStockMarkets[,"FTSE"]
## Plot FTSE
n<-length(FTSE)
plot(ts(FTSE))
grid(col = "red")
## Compute the log-return of the FTSE
log_return<-diff(log(FTSE))*100
n<-length(log_return)
## The time-series plot of the log-return 
plot(ts(log_return))
grid(col="red")

## Dickey-Fuller test for unit-root

## Step 1: Check if log-Price is non-stationary
adf.test(log(FTSE))

## Step 2: Check if log-return is non-stationary
adf.test(log_return)
