# years = year
# year_i = year[i]
# data = read_var_chl_ext_future
# info = c("x", "y", "scenario")
# varname = "chl"

# Environmental window
environmental_window_function <- function(years,
                                          year_i,
                                          data,
                                          info,
                                          varname){
  
  # Déterminer l'année cible
  target_year <- as.numeric(year_i)
  
  years <- as.numeric(years)
  
  # Définir les indices de début et de fin de la fenêtre de 10 ans
  start_year <- target_year - 4
  end_year <- target_year + 5
  
  # Limiter les années au domaine de 2023 à 2100
  environmental_window <- years[years >= start_year & years <= end_year]
  
  # S'assurer que la fenêtre est de taille 10
  
  # if(target_year %in% c(2023:2027, 2095:2100)) {
  # if(target_year %in% c(2015:2019, 2095:2100)) {
  if(target_year %in% c(2005:2009, 2095:2100)) {
    
    environmental_window <- environmental_window
    
  }

  environmental_window <- as.character(environmental_window)
  
  # environmental_mean <- data[colnames(data) %in% c(info, environmental_window)] |> 
  #   dplyr::group_by(dplyr::across(info)) |> 
  #   dplyr::summarise(year_mean = rowMeans(dplyr::across(environmental_window)))
  
  environmental_mean <- data[colnames(data) %in% c(info, environmental_window)]
  
  environmental_mean$year_mean <- rowMeans(data[colnames(data) %in% c(environmental_window)])
  
  environmental_mean <- environmental_mean |> 
    dplyr::select(-environmental_window)

  colnames(environmental_mean)[colnames(environmental_mean) == "year_mean"] <- paste0(varname, "_", target_year)
  
  return(environmental_mean)
  
}
