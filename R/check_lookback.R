check_lookback <- function(lookback){
  lookback <- as.integer(lookback)
  if(lookback > 90 || lookback < 14){
    stop("Lookback value is outside of required range, see function documentation")
  }
}
