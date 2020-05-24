# https://systematicinvestor.wordpress.com/2013/07/30/stop-loss/
# Stop loss strategy
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
rm(list = ls())
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#******************************************************************  
load.packages('quantmod')  

tickers = spl('SPY')    

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)        
bt.prep(data, align='keep.all', dates='1999::')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices   

models = list()

#*****************************************************************
# Code Strategies
#****************************************************************** 
data$weight[] = NA
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)

#*****************************************************************
# Code Strategies : MA Cross Over
#****************************************************************** 
sma.fast = SMA(prices, 20)
sma.slow = SMA(prices, 50)

buy.signal = iif(cross.up(sma.fast, sma.slow), 1, NA)

data$weight[] = NA
data$weight[] = iif(cross.up(sma.fast, sma.slow), 1, iif(cross.dn(sma.fast, sma.slow), 0, NA))
models$ma.cross = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

# Next, I created the custom.stop.fn() function in bt.stop.r at github to handle 
# user defined stop functions. Let’s have a look at 3 different flavors of stops below:

#*****************************************************************
# User Defined Stops
#****************************************************************** 
# fixed stop: exit trade once price falls below % from entry price
fixed.stop <- function(weight, price, tstart, tend, pstop) {
  index = tstart : tend
  if(weight > 0)
    price[ index ] < (1 - pstop) * price[ tstart ]
  else
    price[ index ] > (1 + pstop) * price[ tstart ]
}

# trailing stop: exit trade once price falls below % from max price since start of trade
trailing.stop <- function(weight, price, tstart, tend, pstop) {
  index = tstart : tend
  if(weight > 0) 
    price[ index ] < (1 - pstop) * cummax(price[ index ])
  else
    price[ index ] > (1 + pstop) * cummin(price[ index ])
}   

# trailing stop: exit trade once price either
# - falls below % from max price since start of trade OR
# - rises above % from entry price
trailing.stop.profit.target <- function(weight, price, tstart, tend, pstop, pprofit) {
  index = tstart : tend
  if(weight > 0) {
    temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
    
    # profit target
    temp = temp | price[ index ] > (1 + pprofit) * price[ tstart ]
  } else {
    temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
    
    # profit target
    temp = temp | price[ index ] < (1 - pprofit) * price[ tstart ]       
  }
  return( temp )  
}

# Each user defined stop function have 4 required inputs:
# 1. weight – signal or weight indicating position
# 2. price – price for asset
# 3. tstart – index(time) when trade was open
# 4. tend – index(time) when trade is closed

#*****************************************************************
# Exit using fixed stop
#****************************************************************** 
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), fixed.stop, 
                               pstop = 1/100)
models$ma.cross.fixed.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#*****************************************************************
# Exit using trailing stop
#****************************************************************** 
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), trailing.stop, 
                               pstop = 1/100)
models$ma.cross.trailing.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#*****************************************************************
# Exit using trailing stop or profit target
#******************************************************************         
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), trailing.stop.profit.target, 
                               pstop = 1/100, pprofit = 1.5/100)
models$ma.cross.trailing.stop.profit.target = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#*****************************************************************
# Create Report
#******************************************************************     
strategy.performance.snapshoot(models, T)

