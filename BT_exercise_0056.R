# https://bookdown.org/kochiuyu/Technical-Analysis-with-R/technical-indicators.html
# This is the case of downloading data from TEJ
rm(list=ls())
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

install.packages('pacman')
library(pacman)
p_load(quantmod, xts)
# tickers = spl('0050.TW')

# etf50 <- read.table("tw50_20030630_20181231.txt", header = TRUE, fileEncoding = "UTF-8")
# etf56 <- read.csv("tw0056_2007_2019_03.csv", header = T)
# etf56 <- read.delim("tw0056_2007_2019_01.csv", header = T)
# etf56 <- read.table("tw0056_2007_2019.txt", header = TRUE)
etf56 <- read.table("tw0056_20070101_20191231.txt", header = TRUE)
#etf56 <- read.csv("tw0056_20070101_20191231.csv", header = TRUE)
#etf50 <- read.table("tw50_20030630_20181231.txt", header = TRUE)
etf56 <- etf56[c(3, 7)]
colnames(etf56) <- c("date", "price")
head(etf56)
tail(etf56)
str(etf56)
# 檢查是否有na 值
sum(is.na(etf56$price))

# convert to time series data
# library(lubridate)
# etf50.xts <- xts(etf50$tw50, order.by = ymd(as.character(etf50$date)))
etf56.xts <- xts(etf56$price, order.by = as.Date(as.character(etf56$date), format = "%Y%m%d"))
head(etf56.xts)
colnames(etf56.xts) <- 'price'
head(etf56.xts)
#
data <- new.env()
# Three inputs that you have to provide for backtesting:
# 1. prices; 2. weight; 3. execution.price
data$prices = data$weight = data$execution.price = etf56.xts
#data$prices <-  etf56.xts
#data$weight =  etf56.xts
#data$execution.price <- etf56.xts
#
data$weight <- data$prices * NA
data$weight[] = 1
#data$execution.price <- data$prices
data$execution.price[] <- NA
names(data)
# Buy & Hold 
models <- list()
models$buy.hold = bt.run(data) 
names(models$buy.hold)
tail(models$buy.hold$equity)

# you may turn off the timezone error message for just now
# options('xts_check_TZ'=FALSE)

# MA Cross
# sma = bt.apply(data, function(x) { SMA((x), 200) }) 
# md: days of moving average
# one more imput you have to provide: symbolnames
md = 50
sma = SMA(data$prices, md)
head(sma, md)
head(data$prices, md)
data$weight[] = NA
data$weight[] = iif(data$prices >= sma, 1, 0)
data$symbolnames <- 'tw56'
head(data$weight, md)
#names(data)
#
models$sma.cross = bt.run(data, type = 'weight', do.lag = 1, trade.summary = TRUE)  
names(models$sma.cross)
str(models$sma.cross)
models$sma.cross$trade.summary
#
# present the results: 
bt.detail.summary(models$sma.cross)
plotbt.transition.map(models$sma.cross$weight)
plotbt.monthly.table(models$sma.cross$equity)
models <- rev(models)
plotbt.custom.report(models)
plot.table(list2matrix(bt.detail.summary(models$sma.cross, models$sma.cross$trade.summary)), 
           text.cex = 1, smain=names(models)[1])

list2matrix(bt.detail.summary(models$sma.cross, models$sma.cross$trade.summary))

strategy.performance.snapshoot(models,T)

plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)
# Create a chart showing the strategies perfromance in 2000:2009
dates = '2007::2019'
buy.hold.equity = models$buy.hold$equity[dates] / as.double(models$buy.hold$equity[dates][1])
sma.cross.equity = models$sma.cross$equity[dates] / as.double(models$sma.cross$equity[dates][1])
#
chartSeries(buy.hold.equity, TA = c(addTA(sma.cross.equity, on=1, col='red')),  
            theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) ) 









