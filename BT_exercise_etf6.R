rm(list = ls())

etf6 <- read.table('ETF6_20080101-20200430.csv', sep = ',', header = T)
#
head(etf6)

etf6 <- etf6[, -2]
colnames(etf6) <- c('id', 'date', 'price')
head(etf6)

library(pacman)
p_load(reshape2)

etf6.l <- dcast(etf6, date~id)
head(etf6.l)

etf6.l <- na.omit(etf6.l)
head(etf6.l)

str(etf6.l)

# convert into xts
etf6.xts <- xts(etf6.l[, -1], order.by = as.Date(as.character(etf6.l$date), format = '%Y%m%d'))
class(etf6.xts)
# SIT 
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
#
data <- new.env()
# 1. prices; 2. weight; 3. execution.price
# buy and hold
etf56 <- etf6.xts$`0056`
data$prices = data$weight = data$execution.price = etf56
data$weight[] <- 1
data$execution.price[] <- NA
names(data)

#
model <- list()
model$buy.hold <- bt.run(data)

# moving average
md <- 50
sma <- SMA(data$prices, md)
data$weight[] <- NA
data$weight[] <- iif(data$prices >= sma, 1, 0)
data$symbolnames <- 'tw56'
#
model$sma.cross <- bt.run(data, trade.summary = T)
#
strategy.performance.snapshoot(model, T)
