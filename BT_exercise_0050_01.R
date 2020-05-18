# This is to download data from yahoo
rm(list=ls())
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

load.packages('quantmod')
library(xts, zoo)
# tickers = spl('0050.TW')

# etf50 <- read.table("tw50_20030630_20181231.txt", header = TRUE, fileEncoding = "UTF-8")
etf50 <- read.table("tw50_20030630_20181231.txt", header = TRUE)
etf50 <- etf50[-c(1, 2)]
colnames(etf50) <- c("date", "tw50")
head(etf50)
tail(etf50)
str(etf50)
#
# convert to time series data
etf50.xts <- xts(etf50$tw50, order.by = as.Date(as.character(etf50$date), format = "%Y%m%d"))
head(etf50.xts)
colnames(etf50.xts) <- 'tw50'
head(etf50.xts)
#
data <- new.env()
data$prices <- etf50.xts
# Buy & Hold 
data$weight <- data$prices * NA
data$weight[] = 1
data$execution.price <- data$prices
data$execution.price[] <- NA
models$buy.hold = bt.run(data) 

# you may turn off the timezone error message for just now
# options('xts_check_TZ'=FALSE)

# MA Cross
#sma = bt.apply(data, function(x) { SMA((x), 200) }) 
md = 50
sma = SMA(data$prices, md)
head(sma, md)
head(data$prices, md)
data$weight[] = NA
data$weight[] = iif(data$prices >= sma, 1, 0)
head(data$weight, md)
#names(data)
#
models$sma.cross = bt.run(data)  
names(models$sma.cross)
str(models$sma.cross)
models$sma.cross$trade.summary
#
bt.detail.summary(models$sma.cross)
plotbt.transition.map(models$sma.cross$weight)
plotbt.monthly.table(models$sma.cross$equity)
# Create a chart showing the strategies perfromance in 2000:2009
dates = '2005::2020'
buy.hold.equity = models$buy.hold$equity[dates] / as.double(models$buy.hold$equity[dates][1])
sma.cross.equity = models$sma.cross$equity[dates] / as.double(models$sma.cross$equity[dates][1])

chartSeries(buy.hold.equity, TA = c(addTA(sma.cross.equity, on=1, col='red')),  
            theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) ) 









