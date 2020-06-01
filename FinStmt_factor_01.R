# https://systematicinvestor.wordpress.com/2012/01/29/multiple-factor-model-fundamental-data/
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
rm(list = ls())
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

dow.jones.components <- function (){
  url = 'http://money.cnn.com/data/dow30/'
  txt = join(readLines(url))
  temp = gsub(pattern = '" >', replacement = '<td>', txt, perl = TRUE)
  temp = gsub(pattern = '</a>', replacement = '</td>', temp, perl = TRUE) 
  temp = extract.table.from.webpage(temp, 'Volume', has.header = T)
  trim(temp[, 'Company'])}

tickers = dow.jones.components()

data.fund <- new.env()
temp = paste(iif(nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep = '')

for (i in 1:length(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 80)

data.fund[[tickers[1]]]

fund.data




