# load("data/raw/00_rls_surveys.Rdata")
# 
# rls_surveys <- rls_surveys |> 
#   dplyr::filter(latitude < 35 & latitude > -35)
# 
# rls_surveys_xy <- rls_surveys |>
#   dplyr::select(longitude, latitude) |>
#   terra::vect(geom = c("longitude", "latitude"), crs = "WGS84")
# 
# point_to_extract = rls_surveys
# name_id = "survey_id"
# name_longitude = "longitude"
# name_latitude = "latitude"
# data_to_extract_path = dhw[i]
# name_covariates = "DHW_q99"

covariates_extract_function <- function(point_to_extract,
                                        name_id,
                                        name_longitude,
                                        name_latitude,
                                        data_to_extract_path,
                                        name_covariates,
                                        year,
                                        type){
  
  point_to_extract_spatial <- point_to_extract |> 
    dplyr::select(name_longitude, name_latitude) |> 
    terra::vect(geom = c(name_longitude, name_latitude), crs = "WGS84")
  
  data_to_extract <- terra::rast(data_to_extract_path)
  
  if(type == "environment") {
    
    data_to_extract <- data_to_extract[terra::varnames(data_to_extract)[which(grepl(pattern = name_covariates, x = terra::varnames(data_to_extract)) == TRUE)]]
    
    data_to_extract <- data_to_extract[[which(grepl(pattern = paste0(year, collapse = "|"), x = terra::time(data_to_extract)) == TRUE)]]
    
  }else{
    
    data_to_extract <- data_to_extract[[terra::varnames(data_to_extract)[which(grepl(pattern = name_covariates, x = terra::varnames(data_to_extract)) == TRUE)]]]
    
  }

  names(data_to_extract) <- seq(from = year[1], by = year[2] - year[1], length.out = length(names(data_to_extract)))
  
  extracted_data <- terra::extract(x = data_to_extract, y = point_to_extract_spatial, method = "simple", ID = FALSE, xy = TRUE)
  extracted_data_xy <- cbind(point_to_extract[,c(name_id, name_longitude, name_latitude)], extracted_data)
  
  if(any(is.na(extracted_data_xy))) {
    
    extracted_data_na <- extracted_data_xy[!complete.cases(extracted_data_xy),]
    
    extracted_data_na_xy <- extracted_data_na |> 
      dplyr::select(name_longitude, name_latitude) |> 
      terra::vect(geom = c(name_longitude, name_latitude), crs = "WGS84")
    
    new_extracted_data <- terra::extract(x = data_to_extract, y = extracted_data_na_xy, method = "bilinear", ID = FALSE, xy = TRUE)
    new_extracted_data_xy <- cbind(extracted_data_na[,c(name_id, name_longitude, name_latitude)], new_extracted_data)
    
    data_no_na <- extracted_data_xy
    data_no_na[which(data_no_na[,name_id] %in% extracted_data_na[,name_id] == TRUE),] <- new_extracted_data_xy
    
  }else{
    
    data_no_na <- extracted_data_xy
    
  }
  
  data_no_na <- data_no_na[complete.cases(data_no_na),]
  
  data_no_na <- data_no_na[,c(name_id, name_longitude, name_latitude, "x", "y", names(data_to_extract))]

  if(type == "environment") {
    
    if(!any(grepl("DHW", data_to_extract_path))){
      
      model <- gsub("_.*", "", gsub(paste0(path_data, "/"), "", data_to_extract_path))
      scenario <- gsub("_.*", "", gsub(paste0(model, "_"), "", gsub(path_data, "", data_to_extract_path)))
      variable <- name_covariates
      
      data_no_na <- data_no_na |> 
        dplyr::mutate(model = model,
                      scenario = scenario,
                      variable = variable)
      
    }else{
      
      new_path <- gsub(".nc", "", gsub(paste0(c("DHW_", "_DHW"), collapse = "|"), "", gsub(path_data, "", data_to_extract_path)))
      scenario <- gsub("_.*", "", new_path)
      model <- gsub(".*_", "", new_path)
      variable <- name_covariates
      
      data_no_na <- data_no_na |> 
        dplyr::mutate(model = model,
                      scenario = scenario,
                      variable = variable)
      
    }
    
  }
  
  if(type == "social"){
    
    scenario <- tolower(gsub(pattern = "result(GDP)_", replacement =  "", x = scenario, fixed = TRUE))
    variable <- name_covariates
    
    data_no_na <- data_no_na |> 
      dplyr::mutate(model = NA,
                    scenario = scenario,
                    variable = variable)
    
  }
  
  return(data_no_na)
  
}


# point_to_extract = "data/extracted/mask.shp"
# data_to_extract_path = chl[i]
# name_covariates = "chl"


covariates_extract_function_ext <- function(point_to_extract,
                                            data_to_extract_path,
                                            name_covariates,
                                            type,
                                            year,
                                            resolution_change = NULL){
  
  point_to_extract_spatial <- terra::vect(point_to_extract)

  data_to_extract <- terra::rast(data_to_extract_path)
  
  if(!is.null(resolution_change)){
    
    data_to_extract <- terra::aggregate(data_to_extract, fact = resolution_change, fun = "mean", na.rm = TRUE)
    
  }

  if(type == "environment") {
    
    data_to_extract <- data_to_extract[terra::varnames(data_to_extract)[which(grepl(pattern = name_covariates, x = terra::varnames(data_to_extract)) == TRUE)]]
    
    # data_to_extract <- data_to_extract[[which(grepl(pattern = paste0(year, collapse = "|"), x = terra::time(data_to_extract)) == TRUE)]]
    
  }else{
    
    if(!is.null(resolution_change)){
      
      data_to_extract <- data_to_extract[[terra::names(data_to_extract)[which(grepl(pattern = name_covariates, x = terra::names(data_to_extract)) == TRUE)]]]
      
    }else{
      
      data_to_extract <- data_to_extract[[terra::varnames(data_to_extract)[which(grepl(pattern = name_covariates, x = terra::varnames(data_to_extract)) == TRUE)]]]
      
    }

  }
  
  mask <- terra::mask(data_to_extract, mask = point_to_extract_spatial)
  crds_mask <- terra::crds(mask)

  names(data_to_extract) <- seq(from = year[1], by = year[2] - year[1], length.out = length(names(data_to_extract)))

  extracted_data <- terra::extract(x = data_to_extract, y = crds_mask, method = "simple")
  extracted_data_xy <- cbind(crds_mask[,c("x", "y")], extracted_data)
  # extracted_data_xy <- read_var |> 
  #   dplyr::inner_join(extracted_data_xy)
  
  if(type == "environment") {
  
    if(!grepl("DHW", data_to_extract_path)){
      
      model <- gsub("_.*", "", gsub(paste0(path_data, "/"), "", data_to_extract_path))
      scenario <- gsub("_.*", "", gsub(paste0(model, "_"), "", gsub(path_data, "", data_to_extract_path)))
      variable <- terra::varnames(data_to_extract)[which(grepl(name_covariates, terra::varnames(data_to_extract)) == TRUE)]
      
      extracted_data_xy <- extracted_data_xy |> 
        dplyr::mutate(model = model,
                      scenario = scenario,
                      variable = variable)
      
    }else{
      
      new_path <- gsub(".nc", "", gsub(paste0(c("DHW_", "_DHW"), collapse = "|"), "", gsub("data/raw/covariates/environmental/", "", data_to_extract_path)))
      scenario <- gsub("_.*", "", new_path)
      model <- gsub(".*_", "", new_path)
      variable <- terra::varnames(data_to_extract)[which(grepl(name_covariates, terra::varnames(data_to_extract)) == TRUE)]
      
      extracted_data_xy <- extracted_data_xy |> 
        dplyr::mutate(model = model,
                      scenario = scenario,
                      variable = variable)
      
    }
    
  }
  
  if(type == "social"){
    
    scenario <- tolower(gsub(pattern = "result(GDP)_", replacement =  "", x = scenario, fixed = TRUE))
    variable <- name_covariates
    
    extracted_data_xy <- extracted_data_xy |> 
      dplyr::mutate(model = NA,
                    scenario = scenario,
                    variable = variable)
    
  }
  
  return(extracted_data_xy)
  
}

