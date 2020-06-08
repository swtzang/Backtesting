# Reference: 
# https://systematicinvestor.wordpress.com/2012/03/19/backtesting-asset-allocation-portfolios/
# https://systematicinvestor.wordpress.com/?s=minimum+variance+portfolio
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod,quadprog,corpcor,lpSolve')
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                            
bt.prep(data, align='remove.na', dates='1990::2011')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices   
n = ncol(prices)

# find week ends
period.ends = endpoints(prices, 'weeks')
period.ends = period.ends[period.ends > 0]

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
start.i = which(period.ends >= (63 + 1))[1]

weight = NA * prices[period.ends,]
weights = list()
# Equal Weight 1/N Benchmark
weights$equal.weight = weight
weights$equal.weight[] = ntop(prices[period.ends,], n)  
weights$equal.weight[1:start.i,] = NA

weights$min.var = weight
weights$min.maxloss = weight
weights$min.mad = weight
weights$min.cvar = weight
weights$min.cdar = weight
weights$min.cor.insteadof.cov = weight
weights$min.mad.downside = weight
weights$min.risk.downside = weight

# following optimizations use a non-linear solver
weights$erc = weight        
weights$min.avgcor = weight     

risk.contributions = list() 
risk.contributions$erc = weight     

# construct portfolios
for( j in start.i:len(period.ends) ) {
  i = period.ends[j]
  
  # one quarter = 63 days
  hist = ret[ (i- 63 +1):i, ]
  
  # create historical input assumptions
  ia = create.historical.ia(hist, 252)
  s0 = apply(coredata(hist),2,sd)     
  ia$correlation = cor(coredata(hist), use='complete.obs',method='pearson')
  ia$cov = ia$correlation * (s0 %*% t(s0))
  
  # construct portfolios based on various risk measures
  weights$min.var[j,] = min.risk.portfolio(ia, constraints)
  weights$min.maxloss[j,] = min.maxloss.portfolio(ia, constraints)
  weights$min.mad[j,] = min.mad.portfolio(ia, constraints)
  weights$min.cvar[j,] = min.cvar.portfolio(ia, constraints)
  weights$min.cdar[j,] = min.cdar.portfolio(ia, constraints)
  weights$min.cor.insteadof.cov[j,] = min.cor.insteadof.cov.portfolio(ia, constraints)
  weights$min.mad.downside[j,] = min.mad.downside.portfolio(ia, constraints)
  weights$min.risk.downside[j,] = min.risk.downside.portfolio(ia, constraints)
  
  # following optimizations use a non-linear solver       
  constraints$x0 = weights$erc[(j-1),]
  weights$erc[j,] = find.erc.portfolio(ia, constraints)       
  
  constraints$x0 = weights$min.avgcor[(j-1),]
  weights$min.avgcor[j,] = min.avgcor.portfolio(ia, constraints)                      
  
  risk.contributions$erc[j,] = portfolio.risk.contribution(weights$erc[j,], ia)
}











