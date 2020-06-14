rm(list = ls())

etf6 <- read.table('ETF6_20080101-20200430.csv', sep = ',', header = T)
#
head(etf6)
tail(etf6)

etf6 <- etf6[, -2]
colnames(etf6) <- c('id', 'date', 'price')
head(etf6)

library(pacman)
p_load(reshape2, xts, TTR, quantmod)

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
# 
model <- list()
model$etf56 <- bt.run(data, trade.summary = T)
names(model$etf56)
model$etf56$trade.summary
#
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
  model[[i]] <- bt.run(data, trade.summary = T)
  sma <- SMA(data$prices, md)
  data$weight[] <- iif(data$prices >= sma, 1, 0)
  i <- paste(i, '.sma.cross', sep = '')
  model[[i]] <- bt.run(data, trade.summary = T)
}
#
names(model)
names(model$e56.sma.cross)
model$e56.sma.cross$trade.summary
tail(model$e56.sma.cross$equity)
#-------------------------------------------------
strategy.performance.snapshoot(model, T)
plotbt(model, plotX = T, log = 'y', LeftMargin = 3)            
mtext('Cumulative Performance', side = 2, line = 1)
plotbt.strategy.sidebyside(model, return.table=T, make.plot = T)
model$e56.sma.cross
bt.detail.summary(model)
# --------------------------------------------------
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

#=============================================================
# MVP investment strategy
#=============================================================
# monthly rebalance
# covariance matrix 
# use monthly returns to compute monthly covariance matrix
etf3.m <- to.monthly(etf3, indexAt = 'lastof', OHLC = FALSE)
head(etf3.m)
etf3.w <- to.weekly(etf3, indexAt = 'lastof', OHLC = FALSE)
head(etf3.w)
# 

#=================================================================
# MVP portfolio
#=================================================================
# Reset inputs to SIT bt function
data$prices = data$weight = data$execution.price = etf3.m
#data$prices <- industry.price.sample
#data$weight <- industry.price.sample
#data$execution.price <- industry.price.sample
data$execution.price[] <- NA
prices <- data$prices
n <- ncol(prices)
#*****************************************************************
# Create Constraints
#*****************************************************************
constraints = new.constraints(n, lb = 0, ub = +Inf)
# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        
#
ret = prices / mlag(prices) - 1
weight = coredata(prices)
weight[] = NA
i = 36
for (i in 36:dim(weight)[1]) {
  # using 36 historical monthly returns
  hist = ret[ (i- 36 +1):i, ]
  hist = na.omit(hist)
  # create historical input assumptions
  ia = create.historical.ia(hist, 12)
  # s0 = apply(coredata(hist),2, sd)     
  ia$cov = cov(coredata(hist))
  #ia$cov = cor(coredata(hist), use='complete.obs',method='kendall') * (s0 %*% t(s0))
  # use min.risk.portfolio() to compute MVP weights
  weight[i,] = min.risk.portfolio(ia, constraints)
}
weight
#apply(weight, 1, sum)

data$weight[] = weight     
#capital = 100000
#data$weight[] = (capital / prices) * data$weight
model$min.var.monthly = bt.run(data, type = "weight")
#
plotbt.strategy.sidebyside(model, return.table=T, make.plot = T)
plotbt(model)
#====================
# multiple models
#====================
#*****************************************************************
# Create Constraints
#*****************************************************************
constraints = new.constraints(n, lb = 0, ub = 1)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        

#*****************************************************************
# Create Portfolios
#*****************************************************************          
ret = prices / mlag(prices) - 1
weight = coredata(prices)
weight[] = NA
#
weights = list()
# Equal Weight 1/N Benchmark
weights$equal.weight = weight
weights$equal.weight[] = ntop(prices, n)
start.i = 35
weights$equal.weight[1:start.i,] = NA
#
weights$min.var = weight
weights$min.maxloss = weight
weights$min.mad = weight
weights$min.cvar = weight
weights$min.cdar = weight
weights$min.cor.insteadof.cov = weight
weights$min.mad.downside = weight
weights$min.risk.downside = weight
#
#
for (i in 36:dim(weight)[1]) {
  # using 36 historical monthly returns
  hist = ret[ (i- 36 +1):i, ]
  hist = na.omit(hist)
  # create historical input assumptions
  ia = create.historical.ia(hist, 12)
  s0 = apply(coredata(hist),2, sd)     
  # ia$cov = cov(coredata(hist))
  ia$cov = cor(coredata(hist), use='complete.obs',method='kendall') * (s0 %*% t(s0))
  # use min.risk.portfolio() to compute MVP weights
  weights$min.var[i,] = min.risk.portfolio(ia, constraints)
  weights$min.maxloss[i,] = min.maxloss.portfolio(ia, constraints)
  weights$min.mad[i,] = min.mad.portfolio(ia, constraints)
  weights$min.cvar[i,] = min.cvar.portfolio(ia, constraints)
  weights$min.cdar[i,] = min.cdar.portfolio(ia, constraints)
  weights$min.cor.insteadof.cov[i,] = min.cor.insteadof.cov.portfolio(ia, constraints)
  weights$min.mad.downside[i,] = min.mad.downside.portfolio(ia, constraints)
  weights$min.risk.downside[i,] = min.risk.downside.portfolio(ia, constraints)
}

models = list()
# i = "equal.weight"
for(i in names(weights)) {
  data$weight[] = NA
  data$weight[] = weights[[i]]    
  models[[i]] = bt.run.share(data, clean.signal = F)
}

# Plot perfromance
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)            
mtext('Cumulative Performance', side = 2, line = 1)

# Plot Strategy Statistics  Side by Side
plotbt.strategy.sidebyside(models)