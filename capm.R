data<-read.csv("stock_treasury.csv")
# Risk Free Rate is in percentage and annualised. 
# So the following conversion is required.
Rf<-data$UST_Yr_1/(100*250)
plot(ts(Rf),ylab="US Treasury 1 Year Yield")

n<-nrow(data)

## Compute log-return
ln_rt_snp500<-diff(log(data$SnP500))-Rf[2:n]
ln_rt_ibm<-diff(log(data$IBM_AdjClose))-Rf[2:n]
ln_rt_apple<-diff(log(data$Apple_AdjClose))-Rf[2:n]
ln_rt_msft<-diff(log(data$MSFT_AdjClose))-Rf[2:n]
ln_rt_intel<-diff(log(data$Intel_AdjClose))-Rf[2:n]

## log-return of the portfolio
ln_r <- cbind(ln_rt_ibm,ln_rt_apple,ln_rt_msft,ln_rt_intel)
head(ln_r)

w = c(0.2,0.3,0.25,0.25)
ln_rt_portf = ln_r%*%w

capm_ibm<-lm(ln_rt_ibm~ln_rt_snp500)
capm_ibm_analysis<-coefficients(summary(capm_ibm))
capm_ibm_analysis <- round(capm_ibm_analysis,digits = 5)
rownames(capm_ibm_analysis)<-c("alpha","beta")
## Result of capm using lm() for IBM
capm_ibm_analysis

plot(ln_rt_snp500,ln_rt_ibm,xlab="S&P 500",ylab="ibm")
abline(capm_ibm,col="blue")
grid(col="red")
## Adjusted R-Squareed
summary(capm_ibm)$adj.r.squared


rse<-summary(capm_ibm)$sigma
al <-capm_ibm$coefficients[1]-2*rse
b <- capm_ibm$coefficients[2]
abline(a= al,b= b,col=3,lty=2)
au <-capm_ibm$coefficients[1]+2*rse
abline(a=au,b=b,col=3,lty=2)



## CAPM on Microsoft
capm_msft<-lm(ln_rt_msft~ln_rt_snp500)
## Adjusted R-Squareed
summary(capm_msft)$adj.r.squared


## CAPM on Apple
capm_msft<-lm(ln_rt_msft~ln_rt_snp500)
## Adjusted R-Squareed
summary(capm_msft)$adj.r.squared


## CAPM on Intel
capm_intel<-lm(ln_rt_intel~ln_rt_snp500)
## Adjusted R-Squareed
summary(capm_intel)$adj.r.squared


# CAPM on Portfolio Optimization
capm_portf<-lm(ln_rt_portf~ln_rt_snp500)
capm_portf_analysis<-round(coefficients(summary(capm_portf)),digit=5)
rownames(capm_portf_analysis)<-c("alpha","beta")
## Result of capm using lm() for Microsoft
capm_portf_analysis


summary(capm_portf)$adj.r.squared