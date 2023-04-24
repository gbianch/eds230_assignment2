#' Almond Yield Anonomoly Model
#' 
#' This function models almond yield anomaly in response to climate
#' @param temp is the minimum temperature in February (C),
#' @param precip is total precipitation (mm),
#' @return almond yield (ton acre^-1)
#'
# function definition
almond_modelR = function(temp, precip, year) {
  yield = (0.015*temp - 0.0046*temp^2 - 0.07*precip + 0.0043*precip^2 + 0.28)
  return(yield)
}