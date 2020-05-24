# https://systematicinvestor.wordpress.com/2011/12/02/backtesting-with-short-positions/
# backtesting with short position
# The strategy uses the 5 day rate of change (ROC5) and the 252 day rate of change (ROC252):
# Buy (or cover short) at the close if yesterday the ROC252 crossed above the ROC5 and today 
#     the ROC252 is still above the ROC5.
# Sell (or open short) at the close if yesterday the ROC5 crossed above the ROC252 and today 
#     the ROC5 is still above the ROC252.
rm(list = ls())
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#******************************************************************
load.packages('quantmod')
tickers = spl('SPY')

data = new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all', dates='1995::2020')

#*****************************************************************
# Code Strategies
#******************************************************************
prices = data$prices
models <- list()
# Buy & Hold
data$weight[] = 1
models$buy.hold = bt.run(data)

# ROC Strategy
roc5 = prices / mlag(prices,5)
roc252 = prices / mlag(prices,252)
roc5.1 = mlag(roc5,1)
roc5.2 = mlag(roc5,2)
roc252.1 = mlag(roc252,1)
roc252.2 = mlag(roc252,2)

data$weight[] = NA
data$weight = iif(roc252.2 < roc5.2 & roc252.1 > roc5.1 & roc252 > roc5, 1, data$weight$SPY)
data$weight = iif(roc252.2 > roc5.2 & roc252.1 < roc5.1 & roc252 < roc5, -1, data$weight$SPY)
models$roc.cross = bt.run(data, trade.summary=T)
head(models$roc.cross$weight, 280)


#*****************************************************************
# Create Report
#******************************************************************
#plotbt.custom.report(roc.cross, buy.hold, trade.summary=T)

plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)
models <- rev(models)
plotbt.custom.report(models)







