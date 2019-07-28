# Arithmetic expression and Standard Calculation
2 + 2 
exp(-2)
pi
cos(pi/3)

# Assignments
x<-2
y<-3
x + y

# Vectors in R
Nifty.50 <- c(8114.30, 7965.50, 8033.30, 8002.30, 7929.10 )
Nifty.50

# Random Sample and graph
r<-rnorm(n=1000,mean=0,sd=1)
hist(r,col="red")

# Install Necessary Packages
install.packages(c("tseries", "xts", "zoo"))

# Dowload stock price data from Yahoo
library(tseries)
Nifty.50<-get.hist.quote(instrument = "^NSEI"
                         ,start="2016-11-21"
                         ,end="2016-11-25"
                         ,quote="AdjClose"
                         ,provider = "yahoo")

Nifty.50

# Compute Log-return 
ln_rt<-diff(log(Nifty.50))
ln_rt

# Compute Simple return
Rt<-exp(ln_rt)-1
Rt

Rt*100

ln_rt*100

# Average Return
mean(ln_rt)

# Volatility (Standard Deviation)
sd(ln_rt)*100

# Monthly volatility of Nifty 50
sqrt(22)*sd(ln_rt)*100

# Yearly volatility of Nifty 50
sqrt(252)*sd(ln_rt)*100
