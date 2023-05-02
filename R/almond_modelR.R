#' Almond Yield Anomaly Model
#' 
#' This function models almond yield anomaly in response to climate
#' @param clim_data is the numeric data frame of climate data,
#' @param yr is the time series year to pull the temp and precipitation data,
#' @return almond yield (ton acre^-1)
#'
# function definition
almond_modelR = function(clim_data, tmincoeff1 = 0.015, tmincoeff2 = 0.0046, pcoeff1 = 0.07, pcoeff2 = 0.0043) {
  source(here("R", "almond_profit.R"))
  
  annual_data <- climate %>% 
    group_by(year, month) %>% 
    summarize(temp_min = min(tmin_c),
              total_precip = sum(precip))
  
  feb_temp <- annual_data %>% 
    filter(month == 2) %>% 
    select(year, temp_min)
  
  jan_precip <- annual_data %>% 
    filter(month == 1) %>% 
    select(total_precip, year)
  year = feb_temp$year
  yield = (tmincoeff1*feb_temp$temp_min - tmincoeff2*feb_temp$temp_min^2 - pcoeff1*jan_precip$total_precip + pcoeff2*jan_precip$total_precip^2 + 0.28)
  profit = almond_profit(yield_anom = yield)
  return(data.frame(yield, "year" = year, profit))
  
 #return(list(annual=annualsolar[,c("year","elect")], mean=mean(annualsolar$elect)))
 #return(list(max_yield = max(yield), min_yield=min(yield), mean_yield=mean(yield)))
}

