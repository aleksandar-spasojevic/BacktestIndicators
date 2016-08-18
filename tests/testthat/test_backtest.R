
library(BacktestIndicators)

config <- function(path_to_data = 'data.Rdata'){
  code <- "

  Sys.setenv(TZ = 'UTC')

  load('%s')
  A <- data[[1]]
  B <- data[[2]]
  C <- data[[3]]
  add_asset(A, B, C)

  # Indicators ----
  aboveMA <- function(x, width){
  MA <- rollapplyr(x, width = width, mean, na.rm = TRUE)
  return( ifelse( MA < x, 1, -1 ) ) # -1 short, 1 long
  }

  `above MA(20)` <- function(x, width = 20) aboveMA(x = x, width = width)
  `above MA(100)` <- function(x, width = 100) aboveMA(x = x, width = width)
  `above MA(200)` <- function(x, width = 200) aboveMA(x = x, width = width)
  `above MA(500)` <- function(x, width = 500) aboveMA(x = x, width = width)
  `above MA(150)` <- function(x, width = 150) aboveMA(x = x, width = width)
  `always Long` <- function(x) {x[] <- 1; x}
  `always Short` <- function(x) {x[] <- -1; x}

  add_indicator(`above MA(20)`,
  `above MA(100)`,
  `above MA(150)`,
  `above MA(200)`,
  `above MA(500)`,
  `always Long`,
  `always Short`)

  "

  eval(parse(text = sprintf(code, path_to_data)))
}

context("backtesting default configuration 'config()'")

test_that("EndOfMonth", {
  config()
  expect_silent( run_backtest( RebalanceConfig("EndOfMonth") ) )
  plot(get_backtest()[2])
  dev.off()
})

test_that("EndOfQuarter", {
  config()
  expect_silent( run_backtest( RebalanceConfig("EndOfQuarter") ) )
  plot(get_backtest()[2])
  dev.off()
})

test_that("EndOfWeek", {
  config()
  expect_silent( run_backtest( RebalanceConfig("EndOfWeek") ) )
  plot(get_backtest()[2])
  dev.off()
})

test_that("EndOfYear", {
  config()
  expect_silent( run_backtest( RebalanceConfig("EndOfYear") ) )
  plot(get_backtest()[2])
  dev.off()
})
