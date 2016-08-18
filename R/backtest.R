
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
  returns <- lapply(periods, lapply, function(period) {
    sweep(period, first(period), MARGIN = 2, FUN = "/") - 1
  })

  apply_indicators()
  positions <- mapply(function(rebalance, indicator) {
    lapply(indicator, "[", rebalance)
  },
  rebalance[names(.assets)], .indicators[names(.assets)],
  SIMPLIFY = FALSE)

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
