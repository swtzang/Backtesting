# https://systematicinvestor.wordpress.com/2011/12/16/backtesting-rebalancing-methods/
# periodic rebalancing
#
# Load Systematic Investor Toolbox (SIT)
# setInternet2(TRUE)
rm(list=ls())
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
library(pacman)
p_load('quantmod')
tickers = spl('SPY,TLT')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='1900::2011')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices   
nperiods = nrow(prices)
target.allocation = matrix(c(0.5, 0.5), nrow=1)

# Buy & Hold    
data$weight[] = NA 
data$weight[1,] = target.allocation
capital = 100000
data$weight[] = (capital / prices) * data$weight
buy.hold = bt.run(data, type='share', capital=capital)
# apply(coredata(data$weight), 2, ifna.prev)

# Rebalance periodically
models = list()
for(period in spl('months,quarters,years')) {
  data$weight[] = NA 
  data$weight[1,] = target.allocation
  
  period.ends = endpoints(prices, period)
  period.ends = period.ends[period.ends > 0]       
  data$weight[period.ends,] = repmat(target.allocation, len(period.ends), 1)
  
  capital = 100000
  data$weight[] = (capital / prices) * data$weight
  models[[period]] = bt.run(data, type='share', capital=capital)  
}
models$buy.hold = buy.hold              

#*****************************************************************
# Create Report
#******************************************************************         
plotbt.custom.report(models)        


# Plot BuyHold and Monthly Rebalancing Weights
layout(1:2)
plotbt.transition.map(models$buy.hold$weight, 'buy.hold', spl('red,orange'))
abline(h=50)
plotbt.transition.map(models$months$weight, 'months', spl('red,orange'))
abline(h=50)
#
plotbt.transition.map(models$quarters$weight, 'quarters', spl('red, orange'))
plotbt.transition.map(models$years$weight, 'years', spl('red, orange'))
# helper function to create barplot with labels
barplot.with.labels <- function(data, main, plotX = TRUE) {
  par(mar=c(iif(plotX, 6, 2), 4, 2, 2))
  x = barplot(100 * data, main = main, las = 2, names.arg = iif(plotX, names(data), ''))
  text(x, 100 * data, round(100 * data,1), adj=c(0.5,1), xpd = TRUE)
}
# Plot Portfolio Turnover for each Rebalancing method
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
#
#compute.turnover(models$months, data)
#portfolio.value = rowSums(abs(models$months$weight), na.rm=T)
#portfolio.turnover = rowSums(abs(models$months$weight - mlag(models$months$weight)), na.rm=T)




