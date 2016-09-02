
#' @export
get_assets <- function() .assets

#' @export
get_indicators <- function() .indicators

#' @export
get_strategies <- function() .strategy

#' @export
get_positions <- function() attr(.strategy, "positions")

#' @export
get_rebalance_dates <- function() {
  dates <- lapply(attr(.strategy, "positions"), lapply, function(p) index(na.omit(p))) # do not include NA positions
  lapply(dates, lapply, function(date) data.frame(Date = date, Day = weekdays(date)))
}

#' @export
get_backtest <- function() {
  structure(
    as.environment(
      list(
        Strategies = get_strategies(),
        Positions = get_positions(),
        Indicators = get_indicators())
    ),
    class = c("backtest","environment"))
}

#' @export
`[.backtest` <- function(x, i, j,...){
  
  if (!missing(i))
    x <- lapply(x, "[", i)
  
  if (!missing(j))
    x <- lapply(x, lapply, "[", j = j)
  
  structure( as.environment(x), class = c("backtest","environment") )
}
