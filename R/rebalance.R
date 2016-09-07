
.subset.xts <- function(x, i, ...){
  force(x)

  if (inherits(i,"rebalance"))
    return( i() )

  return( xts::.subset.xts(x, ...) )
}

#' @export
RebalanceConfig <-
  function(timespan = c("EndOfWeek","EndOfMonth","EndOfQuarter","EndOfYear","EndOfMonthWithStart"),
           kth = 1,
           on = c("Monday","Tuesday","Wednesday","Thursday","Friday"),
           start = ""){

    on <- match.arg(on)
    rebalance <- tryCatch({
      match.fun(timespan)
    },
    error = function(e){
      # if not available in global environment, look in package environment
      get(timespan, envir = getNamespace(packageName()))
    })

    formals(rebalance) <- modifyList(formals(rebalance),
                                     list(kth = kth, on = on, start = start))

    class(rebalance) <- c("function","rebalance")
    return(rebalance)
  }

EndOfMonth <- function(kth, on, start){
  xts::split.xts(parent.frame()$x[sprintf("%s::",start)], "months", k = kth)
}

EndOfMonthWithStart <- function(kth, on, start){
  ts_splitted <- xts::split.xts(parent.frame()$x[sprintf("%s::",start)], 
                                "months", k = kth)
  append(ts_splitted, list(first(ts_splitted[[1]])), after = 0)
}

EndOfWeek <- function(kth, on, start){
  xts::split.xts(parent.frame()$x[sprintf("%s::",start)], "weeks", k = kth)
}

EndOfQuarter <- function(kth, on, start){
  xts::split.xts(parent.frame()$x[sprintf("%s::",start)], "quarters", k = kth)
}

EndOfYear <- function(kth, on, start){
  xts::split.xts(parent.frame()$x[sprintf("%s::",start)], "years", k = kth)
}
