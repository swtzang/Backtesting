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

# convert into xts
etf6.xts <- xts(etf6.l[, -1], order.by = as.Date(as.character(etf6.l$date), format = '%Y%m%d'))
class(etf6.xts)
head(etf6.xts)
etf3.xts <- etf6.xts[, 1:3]
etf3.w.xts <- to.weekly(etf3.xts, indexAt = 'lastof', OHLC = FALSE)
etf3.m.xts <- to.monthly(etf3.xts, indexAt = 'lastof', OHLC = FALSE)

# SIT 
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
#
# 1. prices; 2. weight; 3. execution.price
# buy and hold
# set up inputs of SIT bt function
data <- new.env()
data$prices = data$weight = data$execution.price = etf3.w.xts
data$execution.price[] <- NA
data$symbolnames <- colnames(data$prices)
prices = data$prices   
n = ncol(prices)

# Equal Weight 1/N Benchmark
data$weight = ntop(prices, n)  
#
models <- list()
models$equal.weight <- bt.run(data, trade.summary = T)

#=================================================================
# MVP portfolio
#=================================================================
# reset inputs to SIT bt function
data$prices = data$weight = data$execution.price = etf3.w.xts
#data$prices <- industry.price.sample
#data$weight <- industry.price.sample
#data$execution.price <- industry.price.sample
data$execution.price[] <- NA
prices <- data$prices

#*****************************************************************
# Create Constraints
#*****************************************************************
constraints = new.constraints(n, lb = -Inf, ub = +Inf)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        

ret = prices / mlag(prices) - 1
weight = coredata(prices)
weight[] = NA

# i = 36
# i = 245
for (i in 36:dim(weight)[1]) {
  # using 36 historical monthly returns
  hist = ret[ (i- 36 +1):i, ]
  hist = na.omit(hist)
  # create historical input assumptions
  ia = create.historical.ia(hist, 52)
  s0 = apply(coredata(hist),2, sd)     
  ia$cov = cor(coredata(hist), use='complete.obs',method='kendall') * (s0 %*% t(s0))
  weight[i,] = min.risk.portfolio(ia, constraints)
}

data$weight[] = weight     
#capital = 100000
#data$weight[] = (capital / prices) * data$weight
models$min.var.weekly = bt.run(data)
# to verify the default do.lag  = 1 day
# sum(as.numeric(weight[36,])*as.numeric(ret[37,]))
# min.var.monthly$ret[37, ]
plotbt.custom.report.part1(models$min.var.weekly, models$equal.weight)
#
layout(1:2)
plotbt.transition.map(models$min.var.weekly$weight)
legend('topright', legend = 'min.var.weekly', bty = 'n')
plotbt.transition.map(models$equal.weight$weight)
legend('topright', legend = 'equal weight', bty = 'n')

strategy.performance.snapshoot(models, T)
models.tw <- rev(models)
plotbt.custom.report(models.tw)








