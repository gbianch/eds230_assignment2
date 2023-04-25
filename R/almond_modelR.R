#' Almond Yield Anonomoly Model
#' 
#' This function models almond yield anomaly in response to climate
#' @param clim_data is the numeric data frame of climate data,
#' @param yr is the time series year to pull the temp and precipitation data,
#' @return almond yield (ton acre^-1)
#'
# function definition
almond_modelR = function(clim_data, yr) {
  data_year <- clim_data %>% 
    # filter data for year of interest
    filter(year == yr)
  
  feb_temp <- data_year %>% 
    filter(month == 2) %>% 
    select(tmin_c)
  
  jan_precip <- data_year %>% 
    filter(month == 1) 
  
  jan_total <- sum(jan_precip$precip)
  
 # yield = (0.015*temp - 0.0046*temp^2 - 0.07*precip + 0.0043*precip^2 + 0.28)
  
  max_yield = (-0.015*min(feb_temp)) - (0.0046*min(feb_temp)^2) - (0.07*jan_total) + (0.0043*jan_total^2) + 0.28
  min_yield = (-0.015*max(feb_temp)) - (0.0046*max(feb_temp)^2) - (0.07*jan_total) + (0.0043*jan_total^2) + 0.28
  mean_yield = (-0.015*mean(feb_temp, na.rm = TRUE)) - (0.0046*mean(feb_temp, na.rm = TRUE)^2) - (0.07*jan_total) + (0.0043*jan_total^2) + 0.28
 
  return(list(max_yield, min_yield, mean_yield))
}