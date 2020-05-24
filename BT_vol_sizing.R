# https://systematicinvestor.wordpress.com/2012/06/12/volatility-position-sizing-2/
# volatility sizing 
# http://thepatternsite.com/MoneyMgmt.html

###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
# setInternet2(TRUE)
rm(list = ls())
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')   
tickers = 'SPY'

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)        
bt.prep(data, align='keep.all', dates='1994::')

#*****************************************************************
# Buy and Hold
#****************************************************************** 
prices = data$prices
models = list()

data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)

#*****************************************************************
# Buy and Hold with target 10% Volatility
#****************************************************************** 
ret.log = bt.apply.matrix(data$prices, ROC, type='continuous')
# head(momentum(data$prices, n = 2))
hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 60)
#
data$weight[] = 0.1 / hist.vol
models$buy.hold.volatility.weighted = bt.run.share(data, clean.signal=T)

#*****************************************************************
# Buy and Hold with target 10% Volatility and Max Total leverage 100%
#******************************************************************         
data$weight[] = 0.1 / hist.vol
rs = rowSums(data$weight)
data$weight[] = data$weight / iif(rs > 1, rs, 1)             
models$buy.hold.volatility.weighted.100 = bt.run.share(data, clean.signal=T)

#*****************************************************************
# Same, rebalanced Monthly
#****************************************************************** 
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]

data$weight[] = NA
data$weight[period.ends,] = 0.1 / hist.vol[period.ends,]
rs = rowSums(data$weight[period.ends,])
data$weight[period.ends,] = data$weight[period.ends,] / iif(rs > 1, rs, 1)           
models$buy.hold.volatility.weighted.100.monthly = bt.run.share(data, clean.signal=T)

#*****************************************************************
# Create Report
#****************************************************************** 
# Plot perfromance
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)            
mtext('Cumulative Performance', side = 2, line = 1)

plotbt.custom.report.part2(rev(models))

# Plot Portfolio Turnover for each strategy
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', plotX = F, label='both')
