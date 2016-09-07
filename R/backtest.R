
apply_indicators <- function(){
  .indicators <<- lapply(.assets, function(asset){
    lapply(.indicators_fun, function(indicator){
      indicator(asset)
    })
  })
}

# I > 0 -> Long
# I < 0 -> Short
# I = 0 -> Neutral

#' @export
run_backtest <- function( rebalance_config ){
  periods <- lapply(.assets, .subset.xts, rebalance_config)
  rebalance <- lapply(lapply(periods, lapply, end), do.call, what = c)

  apply_indicators()
  positions <- mapply(function(rebalance, indicator) {
    lapply(indicator, "[", rebalance)
  },
  rebalance[names(.assets)], .indicators[names(.assets)],
  SIMPLIFY = FALSE)
  
  returns <- lapply(periods, get_returns)
  strategy_returns <- mapply(function(positions, returns) {
    lapply(positions, function(position){
      Map("*", head(position, -1), tail(returns, -1))
    })
  }, positions[names(.assets)], returns[names(.assets)],
  SIMPLIFY = FALSE)
  
  .strategy <<- lapply(strategy_returns, lapply, function(returns){
    Reduce(function(current_period, next_period) {
      rbind.xts(current_period, as.numeric(last(current_period)) * next_period)
    } , Map("+", 1, lapply(returns, na.fill, 0)))
  })
  
  attr(.strategy, "positions") <<- positions
  attr(.strategy, "configuration") <<- match.call()
}


get_returns <- function(periods) {
  returns <- attr(
    Reduce(function(current_, next_, ...){
      sub <- sweep(next_, last(current_), MARGIN = 2, FUN = "/") - 1
      structure(next_, 
                returns = append(attr(current_, "returns"), list(sub)))
    }, periods), 
    "returns")
  
  # first period return calculation
  append(list(
    sweep(periods[[1]], first(periods[[1]]), MARGIN = 2, FUN = "/") - 1
  ), 
  returns)
}
