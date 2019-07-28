## Let's go into some real data
library(xts)
print(load("../data/demo_intradaydata.rda"))
## So we have to decipher
##    background
##    data.nifty
##    data.stock.cash
##    data.stock.futures

class(background)
str(background)
background

class(data.nifty)
str(data.nifty)
head(data.nifty[[1]])

class(data.stock.cash)                  # There are 15
str(data.stock.cash)
str(data.stock.cash[[1]])               # This is TCS, there are 4 days
tcs <- data.stock.cash[[1]]             # This is the 1st day of TCS
tail(tcs[[1]])
plot(tcs[[1]][,"ltp"])

class(data.stock.futures)
str(data.stock.futures)
tcs.f <- data.stock.futures[[1]]
str(tcs.f)
tcs.f.day1 <- tcs.f[[1]]
str(tcs.f.day1)
head(tcs.f.day1)
plot(tcs.f.day1[,"ltp"])

## Now let's do some examples of using this data
source("functions.R")

## Plot of Nifty level at tick frequency, for one day
plot (data.nifty[[1]]$nifty/100,
      type = "l",
      main = "Nifty index",
      ylab = "Nifty index")

## Plot of Nifty at a 5m resolution
nifty.values <- intervalData(name = "nifty",
                             instrument = NULL, interval = "300")
plot (nifty.values[[1]]$nifty/100,
      type = "l",
      main = "Nifty index differenced by five minutes",
      ylab = "Nifty index")

## Superpose TCS spot and futures for one day
spot <- tcs[[1]][,"ltp"]
futures <- tcs.f.day1[,"ltp"]
both <- cbind(spot,futures)
plot(as.zoo(both), plot.type="single", col=c("black","red"))
