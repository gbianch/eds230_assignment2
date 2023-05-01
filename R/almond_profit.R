#' Almond Yield Profit Model
#' 
#' This function models almond profits based on almond yield anomaly in response to climate
#' @param yield_anom is the yield anomaly calculated by almond_modelR() from climate data,
#' @param P is the market price of almonds ($/kg),
#' @return almond yield (ton acre^-1)
#'
# function definition
almond_profit = function(yield_anom, P = 5.95, ) {
  
  price_ton <- P*1000
  baseline_p <- price_ton*0.9 #baseline profit ($/acre)
  profit = baseline_p + yield_anom*price_ton
  
  return(profit)
  
}
