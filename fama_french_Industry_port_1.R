# Difference between previous version: try to combine equal weighting and mvp into model list()
# https://systematicinvestor.wordpress.com/2011/12/13/backtesting-minimum-variance-portfolios/
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
head(industry10)
# convert into prices
industry.price <- cumprod(industry10 + 1)*100
head(industry.price)
tail(industry.price)
#=================================================================
# equal weight and MVP portfolio
#=================================================================
# reset sample range
industry.price.sample <- industry.price['199701/202003']
# industry10.price.sample <- industry10['199701/202003']
# reset inputs to SIT bt function
data <- new.env()
data$prices = data$weight = data$execution.price = industry.price.sample
#data$prices <- industry.price.sample
#data$weight <- industry.price.sample
#data$execution.price <- industry.price.sample
data$execution.price[] <- NA
prices <- data$prices
n <- ncol(prices)

#*****************************************************************
# Create Constraints
#*****************************************************************
constraints = new.constraints(n, lb = -Inf, ub = +Inf)
# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        
#===================================================================
#

ret = prices / mlag(prices) - 1
start.i <- 36
# start.i <- which(index(prices) == '1999-12-31')
weight = coredata(prices)
weight[] = NA
weights <- list()
  # equal weight 1/N benchmark
  weights$equal.weight <- weight
  weights$equal.weight[] <- ntop(prices, n)
  weights$equal.weight[1:start.i, ] <- NA
  #
  weights$min.var <- weight
  weights$min.maxloss <- weight
  weights$min.mad <- weight
  weights$min.cvar <- weight
  weights$min.cdar <- weight
  weights$min.cor.insteadof.cov <- weight
  # weights$min.mad.downsid <- weight
  weights$min.risk.downside <- weight
  # following optimizations use a non-linear solver
  weights$erc = weight        
  weights$min.avgcor = weight    
# i = 36
# i = 245
# construct portfolios
for (i in start.i:dim(weight)[1]) {
  # using 36 historical monthly returns
  hist = ret[ (i- 36 +1):i, ]
  hist[is.na(hist)] <- 0
  # hist = na.omit(hist)
  # create historical input assumptions
  ia = create.historical.ia(hist, 12)
  s0 = apply(coredata(hist),2, sd)     
  ia$cov = cor(coredata(hist), use='complete.obs',method='kendall') * (s0 %*% t(s0))
  # construct portfolios based on various risk measures
  weights$min.var[i,] = min.risk.portfolio(ia, constraints)
  weights$min.maxloss[i,] = min.maxloss.portfolio(ia, constraints)
  weights$min.mad[i,] = min.mad.portfolio(ia, constraints)
  weights$min.cvar[i,] = min.cvar.portfolio(ia, constraints)
  weights$min.cdar[i,] = min.cdar.portfolio(ia, constraints)
  weights$min.cor.insteadof.cov[i,] = min.cor.insteadof.cov.portfolio(ia, constraints)
  # weights$min.mad.downside[i,] = min.mad.downside.portfolio(ia, constraints)
  weights$min.risk.downside[i,] = min.risk.downside.portfolio(ia, constraints)
  # following optimizations use a non-linear solver       
  constraints$x0 = weights$erc[(j-1),]
  weights$erc[j,] = find.erc.portfolio(ia, constraints)       
  
  constraints$x0 = weights$min.avgcor[(j-1),]
  weights$min.avgcor[j,] = min.avgcor.portfolio(ia, constraints)                      
  
  risk.contributions$erc[j,] = portfolio.risk.contribution(weights$erc[j,], ia)
}

# apply(weight, 1, sum)

data$weight[] = weight     
#capital = 100000
#data$weight[] = (capital / prices) * data$weight
models.tw$min.var.monthly = bt.run(data)
# to verify the default do.lag  = 1 day
# sum(as.numeric(weight[36,])*as.numeric(ret[37,]))
# min.var.monthly$ret[37, ]
plotbt.custom.report.part1(models.tw$min.var.monthly, models.tw$equal.weight)
#
layout(1:2)
plotbt.transition.map(models.tw$min.var.monthly$weight)
legend('topright', legend = 'min.var.monthly', bty = 'n')
plotbt.transition.map(models.tw$equal.weight$weight)
legend('topright', legend = 'equal weight', bty = 'n')

strategy.performance.snapshoot(models.tw, T)
models.tw <- rev(models.tw)
plotbt.custom.report(models.tw)
