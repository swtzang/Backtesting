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

# etf6.l <- na.omit(etf6.l)
# head(etf6.l)

str(etf6.l)

# convert into xts
etf6.xts <- xts(etf6.l[, -1], order.by = as.Date(as.character(etf6.l$date), format = '%Y%m%d'))
class(etf6.xts)
head(etf6.xts)
# SIT 
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
#
data <- new.env()
# 1. prices; 2. weight; 3. execution.price
# buy and hold
etf56 <- etf6.xts$`0056`
head(etf56)
data$prices = data$weight = data$execution.price = etf56
data$weight[] <- 1
data$execution.price[] <- NA
names(data)

etf52 <- etf6.xts$`0052`
head(etf52)
data$prices = data$weight = data$execution.price = etf52
data$weight[] <- 1
data$execution.price[] <- NA
names(data)
#
model <- list()
#
etf3 <- etf6.xts[, 1:3]
head(etf3)
names(etf3)
colnames(etf3) <- c('e50', 'e52', 'e56')
names(etf3)
md = 50
i = 'e50'
for (i in names(etf3)) {
  data$prices = data$weight = data$execution.price = etf3[, i]
  data$weight[] <- 1
  data$execution.price[] <- NA
  model[[i]] <- bt.run(data)
  sma <- SMA(data$prices, md)
  data$weight[] <- iif(data$prices >= sma, 1, 0)
  i <- paste(i, '.sma.cross', sep = '')
  model[[i]] <- bt.run(data)
}

strategy.performance.snapshoot(model, T)
plotbt(model, plotX = T, log = 'y', LeftMargin = 3)            
mtext('Cumulative Performance', side = 2, line = 1)
#
model <- list()
#
model$buy.hold <- bt.run(data)


#
etf3 <- etf6.xts[, 1:3]
data$prices = data$weight = data$execution.price = etf3
data$execution.price[] <- NA
prices <- data$prices
n <- ncol(prices)
data$weight <- ntop(prices, n)
model$etf3.bh <- bt.run(data)

# moving average
md <- 50
data$prices = data$weight = data$execution.price = etf3$`0056`
sma <- SMA(data$prices, md)
data$weight[] <- NA
data$weight[] <- iif(data$prices >= sma, 1, 0)
data$symbolnames <- 'tw56'
#
model$tw56.sma.cross <- bt.run(data, trade.summary = T)
#
strategy.performance.snapshoot(model, T)
plotbt(model, plotX = T, log = 'y', LeftMargin = 3)            
mtext('Cumulative Performance', side = 2, line = 1)
#===================================================
# multiple models
#===================================================
colnames(etf3) <- c("etf50", "etf52", "etf56")
data <- new.env()
models <- list()
weights <- list()
# equal weight for 3 assets
data$prices = data$weight = data$execution.price = etf3 
prices <- data$prices
n <- ncol(prices)
weights$etf3.equal.weight <- ntop(prices, n) 
# buy and hold for each asset
data <- new.env()
data$prices = data$weight = data$execution.price = etf3$`0050`
md = 50
for (i in names(etf3)) {
  data <- new.env()
  data$prices = data$weight = data$execution.price = etf3[, i]
  data$weight[] = 1
  models[[i]] <- bt.run(data) 
  sma <- SMA(data$prices, md)
  data$weight[] <- iif(data$prices >= sma, 1, 0)
  i <- paste(i, '.sma.cross', sep = '')
  models[[i]] <- bt.run(data)
}

plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)
strategy.performance.snapshoot(models,T)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)            
mtext('Cumulative Performance', side = 2, line = 1)







