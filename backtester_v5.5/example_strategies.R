example_strategies <- c("fixed", 
                        "big_spender",
                        "bankrupt", 
                        "copycat", 
                        "random", 
                        "rsi_contrarian", 
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period",
                        "simple_limit") 
# end of example_strategies

example_params <- list(
                    "fixed"=list(sizes=rep(1,5)),
                    "big_spender"=list(sizes=rep(1,5)),
                    "bankrupt"=list(leverage=40000000),
                    "copycat"=NULL,
                    "random"=list(maxLots=100),
                    "rsi_contrarian"=list(lookback=10,threshold=25,series=1:5),
                    "bbands_contrarian"=list(lookback=20,sdParam=1.5,series=1:4,posSizes=rep(1,5)),
                    "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5),posSizes=rep(1,5)),
                    "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,5),holdPeriod=6),
                    "simple_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,5),series=4:5))
# end of example_params

load_strategy <- function(strategy) {
    # load strategy
    strategyFile <- file.path('strategies', paste0(strategy,'.R'))
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders
    # set params via global assignment
    params <<- example_params[[strategy]]
    print("Parameters:")
    print(params)
}