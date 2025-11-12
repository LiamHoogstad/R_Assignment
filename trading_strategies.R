# Trading Strategy Functions
# Simple Moving Average Crossover Strategy

#' Generate Buy/Sell Signals Based on Moving Average Crossover
#'
#' @param prices Numeric vector of closing prices
#' @param fast_window Integer, number of days for fast moving average (default: 50)
#' @param slow_window Integer, number of days for slow moving average (default: 200)
#' @return Numeric vector of signals: +1 (buy), -1 (sell), NA (insufficient data)
#' @description Generates trading signals where:
#'   - Signal = +1 when fast MA is above slow MA (buy position)
#'   - Signal = -1 when fast MA is below slow MA (sell position)
generate_ma_signals <- function(prices, fast_window = 30, slow_window = 150) {
  
  # Initialize vectors for moving averages
  n <- length(prices)
  fast_ma <- rep(NA, n)
  slow_ma <- rep(NA, n)
  
  # Calculate fast moving average
  for (i in fast_window:n) {
    fast_ma[i] <- mean(prices[(i - fast_window + 1):i])
  }
  
  # Calculate slow moving average
  for (i in slow_window:n) {
    slow_ma[i] <- mean(prices[(i - slow_window + 1):i])
  }
  
  # Generate signals: +1 when fast > slow, -1 when fast < slow
  signal <- sign(fast_ma - slow_ma)
  
  return(signal)
}


#' Backtest Moving Average Crossover Strategy
#'
#' @param prices Numeric vector of closing prices
#' @param signals Numeric vector of trading signals (+1 or -1)
#' @return List containing:
#'   - returns: Daily returns of the asset
#'   - strategy_returns: Daily returns of the strategy
#' @description Backtests the strategy by applying lagged signals to returns.
#'   Uses lagged signals to avoid look-ahead bias.
backtest_strategy <- function(prices, signals) {
  
  # Calculate daily returns
  returns <- c(NA, diff(prices) / head(prices, -1))
  
  # Lag the signals by one day (trade on next day's open)
  lagged_signals <- c(NA, head(signals, -1))
  
  # Calculate strategy returns
  strategy_returns <- lagged_signals * returns
  
  # Replace NA with 0 (no position = no return)
  returns[is.na(returns)] <- 0
  strategy_returns[is.na(strategy_returns)] <- 0
  
  # Return results as a list
  return(list(
    returns = returns,
    strategy_returns = strategy_returns
  ))
}