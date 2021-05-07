source('framework/data.R')
source('framework/backtester.R')
source('framework/processResults.R')

# Read in data
dataList <- getData(directory="A2")

# Choose strategy
strategyFile <-'strategies/a2_template.R'

cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

# Strategy parameters 

dataList <- lapply(dataList, function(x)x[375:1077])
series=1:length(dataList)
lb <- list(short=as.integer(10),medium=as.integer(20),long=as.integer(30))
params <- list(lookbacks=lb,sdParam=1.5,series=series)
print("Parameters:")
print(params)

# Do backtest 
results <- backtest(dataList,getOrders,params,sMult=0.2)
pfolioPnL <- plotResults(dataList,results)