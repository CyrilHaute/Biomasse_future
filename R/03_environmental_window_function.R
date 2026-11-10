
environmental_window_function <- function(years,
                                          year_i,
                                          data,
                                          info,
                                          varname){
  
  target_year <- as.numeric(year_i)
  
  years <- as.numeric(years)
  
  start_year <- target_year - 4
  end_year <- target_year + 5
  
  environmental_window <- years[years >= start_year & years <= end_year]

  if(target_year %in% c(2005:2009, 2095:2100)) {
    
    environmental_window <- environmental_window
    
  }

  environmental_window <- as.character(environmental_window)

  environmental_mean <- data[colnames(data) %in% c(info, environmental_window)]
  
  environmental_mean$year_mean <- rowMeans(data[colnames(data) %in% c(environmental_window)])
  
  environmental_mean <- environmental_mean |> 
    dplyr::select(-environmental_window)

  colnames(environmental_mean)[colnames(environmental_mean) == "year_mean"] <- paste0(varname, "_", target_year)
  
  return(environmental_mean)
  
}
