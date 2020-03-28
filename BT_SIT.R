# https://systematicinvestor.wordpress.com/2011/11/25/introduction-to-backtesting-library-in-the-systematic-investor-toolbox/

#======================================================
# Introduction to Backtesting library of SIT
# https://systematicinvestor.wordpress.com/2011/11/   # 2011/11/25
#=====================================================================
#I wrote a simple Backtesting library to evaluate and analyze Trading Strategies. 
#I will use this library to present the performance of trading strategies that I will 
#study in the next series of posts.
#It is very easy to write a simple Backtesting routine in R, for example:
#=================
rm(list=ls())

# na.locf: last observation carried forward
# Generic function for replacing each NA with the most recent non-NA prior to it.
# na.rm:	logical. Should leading NAs be removed?

bt.simple <- function(data, signal) 
{
  # lag singal
  signal = Lag(signal, 1)
  
  # back fill
  signal = na.locf(signal, na.rm = FALSE)
  signal[is.na(signal)] = 0
  
  # calculate Close-to-Close returns
  ret = ROC(Cl(data), type='discrete')
  ret[1] = 0
  
  # compute stats 
  bt = list()
  bt$ret = ret * signal
  bt$equity = cumprod(1 + bt$ret)             
  return(bt)
}

# Test for bt.simple functions
library('quantmod')

# load historical prices from Yahoo Finance
data = getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)
str(data)
tail(data)
# Buy & Hold
signal = rep(1, nrow(data))
buy.hold = bt.simple(data, signal)
ls(buy.hold)
# MA Cross
sma = SMA(Cl(data),200)
signal = ifelse(Cl(data) > sma, 1, 0)
sma.cross = bt.simple(data, signal)
names(sma.cross)
head(sma.cross$equity)
# Create a chart showing the strategies perfromance in 2000:2009
dates = '2000::2020'
buy.hold.equity = buy.hold$equity[dates] / as.double(buy.hold$equity[dates][1])
sma.cross.equity = sma.cross$equity[dates] / as.double(sma.cross$equity[dates][1])

chartSeries(buy.hold.equity, TA = c(addTA(sma.cross.equity, on=1, col='red')),  
            theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) ) 

#========================================================================================================
#The code I implemented in the Systematic Investor Toolbox is a bit longer, but follows the same logic. 
#It provides extra functionality: ability to handle multiple securities, weights or shares backtesting, 
#and customized reporting. Following is a sample code to implement the above strategies using the backtesting 
#library in the Systematic Investor Toolbox:
# Load Systematic Investor Toolbox (SIT)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)
#=======================================================================================================
# The bt.prep function merges and aligns all symbols in the data environment.
# The bt.apply function applies user given function to each symbol in the data environment.
# The bt.run computes the equity curve of strategy specified by data$weight matrix. 
# The data$weight matrix holds weights (signals) to open/close positions.
# The plotbt.custom.report function creates the customized report, which can be fined tuned by the user
#=======================================================================================================

#*****************************************************************
# Load historical data
#******************************************************************     
load.packages('quantmod')
tickers = spl('SPY')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all', dates='1970::2011')

#*****************************************************************
# Code Strategies
#****************************************************************** 
models<-list()
prices = data$prices    

# Buy & Hold    
data$weight[] = 1
models$buy.hold = bt.run(data) 
# you may turn off the timezone error message for just now
# options('xts_check_TZ'=FALSE)

# MA Cross
sma = bt.apply(data, function(x) { SMA(Cl(x), 200) } )  
data$weight[] = NA
data$weight[] = iif(prices >= sma, 1, 0)
#================================================================================================
#https://github.com/systematicinvestor/SIT/blob/fa252258525a4f3e29da1f845ad683d917dafef7/R/bt.r
models$sma.cross = bt.run(data, trade.summary=T)           
models$sma.cross$trade.summary
#===============================================================================================
# https://github.com/systematicinvestor/SIT/blob/fa252258525a4f3e29da1f845ad683d917dafef7/R/bt.r
#=================================================================================================
# ntrades: number of trades
# avg.pnl: average return of profit and loss
# leng: average holding period
# win.prob: win trades/total trades
# Expectancy by Van Tharp : Expectancy = (PWin * AvgWin) - (PLoss * AvgLoss)	
# Profit Factor is computed as follows: (PWin * AvgWin) / (PLoss * AvgLoss)
#*****************************************************************
# Create Report
#****************************************************************** 
# https://github.com/systematicinvestor/SIT/blob/fa252258525a4f3e29da1f845ad683d917dafef7/R/bt.summary.r
#plotbt.custom.report(sma.cross, buy.hold)
plotbt.custom.report(models)
#by 3 parts
getOption("device")
plotbt(models)
plotbt(models$buy.hold, plottype = '12M')
plotbt(models$buy.hold, xfun = function(x) {100 * compute.drawdown(x$equity)})
#models = variable.number.arguments(sma.cross)
#models[1]
plotbt.custom.report.part1(models$sma.cross, trade.summary =T)
#
plotbt.custom.report.part2(models$sma.cross, trade.summary =T)
plotbt.custom.report.part3(models$sma.cross, trade.summary =T)
#
print(plotbt.monthly.table(models$sma.cross$equity, smain=names(models)[1], make.plot=F))
#
windows()
plot.table(list2matrix(bt.detail.summary(models$sma.cross, models$sma.cross$trade.summary)), 
           text.cex = 1, smain=names(models)[1])

# see details in bt.trade.summary.helper()
bt.detail.summary(models$sma.cross, models$sma.cross$trade.summary)

strategy.performance.snapshoot(models,T)	
plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)




