source('framework/data.R')  
source('framework/backtester.R')
source('framework/processResults.R')  
source('framework/utilities.R')  
source('example_strategies.R') 

# load data
dataList <- getData(directory="EXAMPLE")

# choose strategy from example_strategies
strategy <- "fixed"
          
# check that the choice is valid
is_valid_example_strategy <- function(strategy) { 
    strategy %in% example_strategies
}
stopifnot(is_valid_example_strategy(strategy))

# load in strategy and params
load_strategy(strategy) # function from example_strategies.R

inSampDays <- 200 # in-sample period 1:inSampDays
dataList <- lapply(dataList, function(x) x[1:inSampDays])

sMult <- 0.20 # slippage multiplier
#sMult <- 0

# Do backtest
results <- backtest(dataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results)