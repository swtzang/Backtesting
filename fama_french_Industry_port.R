# Homework
#==============================================================================================
#1. Download 10 industry portfolio returns (average value-weighted monthly returns) from 
#   Fama  French data library (http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)
#2. Compute equal weight portfolio returns EACH month starting from 2000/01 to 2020/03. 
#   Denote this strategy as the Benchmark portfolio and create its backtesting report using SIT. 
#3. Compute  MVP portfolio returns by rebalancing EACH month starting from 2000/01 to 2020/03. 
#   Use in-sample data range of 36 months to compute covariance matrix. Denote this strategy 
#   as the MVP portfolio and create its backtesting report using SIT.
#4. Plot both strategies side by side and compare their performance and comment.



rm(list = ls())
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#****************************************************************** 
library(pacman)
p_load(quantmod, quadprog, lpSolve)
#
industry10 <- read.table("10_Industry_Portfolios_Wout_Div.txt", header = TRUE)
date <- seq(as.Date("1926-08-01"), length=1126, by="1 month") - 1
industry10 <- xts(coredata(industry10[, -1])/100, order.by = date)
# convert into prices
industry.price <- cumprod(industry10+1)*100
head(industry.price)
tail(industry.price)
#
industry.price.sample <- industry.price['199912/202003']
#
models.tw<-list()
# set up inputs of SIT bt function
data <- new.env()
data$prices <- industry.price.sample
data$weight <- industry.price.sample
data$execution.price <- industry.price.sample
data.tw$execution.price[] <- NA
data$symbolnames <- colnames(data$prices)
prices = data$prices   
n = ncol(prices)

# Equal Weight 1/N Benchmark
data$weight = ntop(prices, n)  
#
models.tw$equal.weight <- bt.run(data, trade.summary = T)
#
#capital = 100000
#data$weight[] = (capital / prices) * data$weight
#equal.weight = bt.run(data, type='share')
#head(equal.weight$ret)
#
bt.detail.summary(models.tw$equal.weight)
plotbt.transition.map(models.tw$equal.weight$weight)
plotbt.monthly.table(models.tw$equal.weight$equity)
strategy.performance.snapshoot(models.tw, T)
#=================================================================
# MVP portfolio
#=================================================================
# reset sample range
industry.price.sample <- industry.price['199701/202003']
# industry10.price.sample <- industry10['199701/202003']
# reset inputs to SIT bt function
data$prices <- industry.price.sample
data$weight <- industry.price.sample
data$execution.price <- industry.price.sample
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

i = 36
i = 245
for( i in 36 : (dim(weight)[1]) )  {
  # using 36 historical monthly returns
  hist = ret[ (i- 36 +1):i, ]
  hist = na.omit(hist)
  # create historical input assumptions
  ia = create.historical.ia(hist, 12)
  s0 = apply(coredata(hist),2,sd)     
  ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
  
  weight[i,] = min.risk.portfolio(ia, constraints)
}

# apply(weight, 1, sum)

data$weight[] = weight     
#capital = 100000
#data$weight[] = (capital / prices) * data$weight
models.tw$min.var.monthly = bt.run(data)
# to verify the default do.lag  = 1 day
# sum(as.numeric(weight[36,])*as.numeric(ret[37,]))
# min.var.monthly$ret[37, ]

strategy.performance.snapshoot(models.tw, T)
models.tw <- rev(models.tw)
plotbt.custom.report(models.tw)
