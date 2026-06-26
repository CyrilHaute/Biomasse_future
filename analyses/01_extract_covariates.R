source("R/01_covariates_extract_function.R")

load("data/raw/00_rls_surveys.Rdata")

rls_surveys <- rls_surveys |> 
  dplyr::filter(latitude < 35 & latitude > -35)

########################## Environmental covariates ##########################

file_covariates <- list.files("data/raw/covariates/environmental", full.names = TRUE)

chl <- file_covariates[which(grepl("chlos", file_covariates) == TRUE)]
# o2 <- file_covariates[which(grepl("o2os", file_covariates) == TRUE)]
ph <- file_covariates[which(grepl("phos", file_covariates) == TRUE)]
sss <- file_covariates[which(grepl("sos", file_covariates) == TRUE)]
sst <- file_covariates[which(grepl("tos", file_covariates) == TRUE)]
# npp <- file_covariates[which(grepl("ppos", file_covariates) == TRUE)]
# dhw <- file_covariates[which(grepl("DHW", file_covariates) == TRUE)]

################## Interpolation ##################

chl_ext <- pbmcapply::pbmclapply(1:length(chl), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = chl[i],
                              name_covariates = "chl",
                              year = 2015:2100,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(chl_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental/"
  data <- chl_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

o2_ext <- pbmcapply::pbmclapply(1:length(o2), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = o2[i],
                              name_covariates = "o2",
                              year = 2015:2100,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(o2_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental/"
  data <- o2_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

ph_ext <- pbmcapply::pbmclapply(1:length(ph), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = ph[i],
                              name_covariates = "ph",
                              year = 2015:2100,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(ph_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental/"
  data <- ph_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

sss_ext <- pbmcapply::pbmclapply(1:length(sss), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = sss[i],
                              name_covariates = "so",
                              year = 2015:2100,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(sss_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental/"
  data <- sss_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

sst_ext <- pbmcapply::pbmclapply(1:length(sst), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = sst[i],
                              name_covariates = "tos",
                              year = 2015:2100,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(sst_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental/"
  data <- sst_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

npp_ext <- pbmcapply::pbmclapply(1:length(npp), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = npp[i],
                              name_covariates = "pp",
                              year = 2015:2100,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(npp_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental/"
  data <- npp_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

dhw_ext <- pbmcapply::pbmclapply(1:length(dhw), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = dhw[i],
                              name_covariates = "DHW_q99",
                              year = 2015:2100,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() -1)
lapply(1:length(dhw_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental/"
  data <- dhw_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})


path_data <- "data/raw/covariates/environmental_historical"
file_covariates <- list.files(path_data, full.names = TRUE)

chl <- file_covariates[which(grepl("chlos", file_covariates) == TRUE)]
ph <- file_covariates[which(grepl("phos", file_covariates) == TRUE)]
sss <- file_covariates[which(grepl("sos", file_covariates) == TRUE)]
sst <- file_covariates[which(grepl("tos", file_covariates) == TRUE)]

chl_ext <- pbmcapply::pbmclapply(1:length(chl), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = chl[i],
                              name_covariates = "chl",
                              year = 2005:2014,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(chl_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_historical/"
  data <- chl_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$model)), collapse = "_"), ".Rdata"))
  
})


ph_ext <- pbmcapply::pbmclapply(1:length(ph), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = ph[i],
                              name_covariates = "ph",
                              year = 2005:2014,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(ph_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_historical/"
  data <- ph_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

sss_ext <- pbmcapply::pbmclapply(1:length(sss), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = sss[i],
                              name_covariates = "so",
                              year = 2005:2014,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(sss_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_historical/"
  data <- sss_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

sst_ext <- pbmcapply::pbmclapply(1:length(sst), function(i) {
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = sst[i],
                              name_covariates = "tos",
                              year = 2005:2014,
                              type = "environment")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(sst_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_historical/"
  data <- sst_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$model)), collapse = "_"), ".Rdata"))
  
})






################## Extrapolation ################## 
year <- 2015:2100

chl_ext_extrapolation <- pbmcapply::pbmclapply(1:length(chl), function(i) {
  
  model_i <-  covariates_extract_function_ext(point_to_extract = "data/raw/covariates/habitat/mask.shp",
                                              data_to_extract_path = chl[i],
                                              name_covariates = "chl",
                                              type = "environment",
                                              year = year)
  
  model_i$model <- gsub("data/raw/covariates/environmental/", "", model_i$model)
  
  return(model_i)
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(chl_ext_extrapolation), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_extrapolation/"
  data <- chl_ext_extrapolation[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})


ph_ext_extrapolation <- pbmcapply::pbmclapply(1:length(ph), function(i) {
  
  model_i <- covariates_extract_function_ext(point_to_extract = "data/raw/covariates/habitat/mask.shp",
                                             data_to_extract_path = ph[i],
                                             name_covariates = "ph",
                                             type = "environment",
                                             year = year)
  
  model_i$model <- gsub("data/raw/covariates/environmental/", "", model_i$model)
  
  return(model_i)
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(ph_ext_extrapolation), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_extrapolation/"
  data <- ph_ext_extrapolation[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

sss_ext_extrapolation <- pbmcapply::pbmclapply(1:length(sss), function(i) {
  
  model_i <- covariates_extract_function_ext(point_to_extract = "data/raw/covariates/habitat/mask.shp",
                                             data_to_extract_path = sss[i],
                                             name_covariates = "so",
                                             type = "environment",
                                             year = year)
  
  model_i$model <- gsub("data/raw/covariates/environmental/", "", model_i$model)
  
  return(model_i)
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(sss_ext_extrapolation), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_extrapolation/"
  data <- sss_ext_extrapolation[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})


sst_ext_extrapolation <- pbmcapply::pbmclapply(1:length(sst), function(i) {
  
  model_i <- covariates_extract_function_ext(point_to_extract = "data/raw/covariates/habitat/mask.shp",
                                             data_to_extract_path = sst[i],
                                             name_covariates = "tos",
                                             type = "environment",
                                             year = year)
  
  model_i$model <- gsub("data/raw/covariates/environmental/", "", model_i$model)
  
  return(model_i)
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(sst_ext_extrapolation), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_extrapolation/"
  data <- sst_ext_extrapolation[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario), unique(data$model)), collapse = "_"), ".Rdata"))
  
})


path_data <- "data/raw/covariates/environmental_historical"
file_covariates <- list.files(path_data, full.names = TRUE)

chl <- file_covariates[which(grepl("chlos", file_covariates) == TRUE)]
ph <- file_covariates[which(grepl("phos", file_covariates) == TRUE)]
sss <- file_covariates[which(grepl("sos", file_covariates) == TRUE)]
sst <- file_covariates[which(grepl("tos", file_covariates) == TRUE)]


chl_ext_extrapolation <- pbmcapply::pbmclapply(1:length(chl), function(i) {
  
  covariates_extract_function_ext(point_to_extract = "data/raw/covariates/habitat/mask.shp",
                                  data_to_extract_path = chl[i],
                                  name_covariates = "chl",
                                  type = "environment",
                                  year = 2005:2014)
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(chl_ext_extrapolation), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_extrapolation_historical/"
  dir.create(path_to_save)
  data <- chl_ext_extrapolation[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

ph_ext_extrapolation <- pbmcapply::pbmclapply(1:length(ph), function(i) {
  
  covariates_extract_function_ext(point_to_extract = "data/raw/covariates/habitat/mask.shp",
                                  data_to_extract_path = ph[i],
                                  name_covariates = "ph",
                                  type = "environment",
                                  year = 2005:2014)
  
}, mc.cores = 1)
lapply(1:length(ph_ext_extrapolation), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_extrapolation_historical/"
  dir.create(path_to_save)
  data <- ph_ext_extrapolation[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$model)), collapse = "_"), ".Rdata"))
  
})

sss_ext_extrapolation <- pbmcapply::pbmclapply(1:length(sss), function(i) {
  
  covariates_extract_function_ext(point_to_extract = "data/raw/covariates/habitat/mask.shp",
                                  data_to_extract_path = sss[i],
                                  name_covariates = "so",
                                  type = "environment",
                                  year = 2005:2014)
  
}, mc.cores = 1)
lapply(1:length(sss_ext_extrapolation), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_extrapolation_historical/"
  dir.create(path_to_save)
  data <- sss_ext_extrapolation[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$model)), collapse = "_"), ".Rdata"))
  
})


sst_ext_extrapolation <- pbmcapply::pbmclapply(1:length(sst), function(i) {
  
  covariates_extract_function_ext(point_to_extract = "data/raw/covariates/habitat/mask.shp",
                                  data_to_extract_path = sst[i],
                                  name_covariates = "tos",
                                  type = "environment",
                                  year = 2005:2014)
  
}, mc.cores = 1)
lapply(1:length(sst_ext_extrapolation), function(i) {
  
  path_to_save <- "data/extracted/covariates/environmental_extrapolation_historical/"
  dir.create(path_to_save)
  data <- sst_ext_extrapolation[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$model)), collapse = "_"), ".Rdata"))
  
})


########################## Human covariates ##########################

file_covariates <- list.files("data/raw/covariates/social/future_gdp", full.names = TRUE)
file_covariates_small <- list.files("data/raw/covariates/social/future_gdp", full.names = FALSE)

################## Interpolation ##################

year <- seq(from = 2010, to = 2100, by = 10)

gdp_ext <- pbmcapply::pbmclapply(1:length(file_covariates), function(i) {
  
  gdp_scenario <- file_covariates[i]
  
  list_gdp_scenario <- list.files(gdp_scenario, full.names = TRUE)
  list_gdp_scenario <- list_gdp_scenario[which(grepl(pattern = paste0(as.character(year), collapse = "|"), x = list_gdp_scenario))]
  
  scenario <<- file_covariates_small[i]
  
  covariates_extract_function(point_to_extract = rls_surveys,
                              name_id = "survey_id",
                              name_longitude = "longitude",
                              name_latitude = "latitude",
                              data_to_extract_path = list_gdp_scenario,
                              name_covariates = "gdp",
                              year = year,
                              type = "social")
  
}, mc.cores = parallel::detectCores() - 1)
lapply(1:length(gdp_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/social/"
  data <- gdp_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario)), collapse = "_"), ".Rdata"))
  
})


gravity <- read.csv("data/raw/covariates/social/future_gravity/all_data_gravity.txt", header = TRUE)
id_coord <- read.csv("data/raw/covariates/social/future_gravity/cyril_crds_gravity.csv", header = TRUE)

gravity_coord <- id_coord |> 
  dplyr::inner_join(gravity) |> 
  dplyr::select(-population) |> 
  dplyr::rename(gravity = gravite)

gravity_coord[which(is.na(gravity_coord$gravity)),]$gravity <- 0

gravity_baseline <- gravity_coord |> 
  dplyr::filter(scenario == "baseline")

gravity_scenario <- gravity_coord |> 
  dplyr::filter(scenario != "baseline") |> 
  dplyr::mutate(year = sub(".*?_", "", scenario),
                scenario = gsub("_.*", "", scenario)) |> 
  tidyr::pivot_wider(names_from = year, values_from = gravity) |> 
  dplyr::group_split(scenario)

gravity_scenario_baseline <- lapply(1:length(gravity_scenario), function(i) {
  
  scenario_i <- gravity_scenario[[i]] |> 
    dplyr::inner_join(gravity_baseline |> 
                        dplyr::select(-scenario) |> 
                        dplyr::rename(`2020` = gravity)) |> 
    tidyr::pivot_longer(cols = -c("longitude", "latitude", "scenario", "id"),
                        names_to = "year",
                        values_to = "gravity")
  
})
gravity_scenario_baseline_bind <- do.call(rbind, gravity_scenario_baseline)

save(gravity_scenario_baseline_bind, file = "data/extracted/covariates/social/gravity.Rdata")



################## Extrapolation ##################

year <- seq(from = 2010, to = 2100, by = 10)

gdp_ext <- pbmcapply::pbmclapply(1:length(file_covariates), function(i) {
  
  gdp_scenario <- file_covariates[i]
  
  list_gdp_scenario <- list.files(gdp_scenario, full.names = TRUE)
  list_gdp_scenario <- list_gdp_scenario[which(grepl(pattern = paste0(as.character(year), collapse = "|"), x = list_gdp_scenario))]
  
  scenario <<- file_covariates_small[i]
  
  covariates_extract_function_ext(point_to_extract = "data/raw/covariates/habitat/mask.shp",
                                  data_to_extract_path = list_gdp_scenario,
                                  name_covariates = "gdp",
                                  type = "social",
                                  year = year,
                                  resolution_change = 6)

}, mc.cores = 5)
lapply(1:length(gdp_ext), function(i) {
  
  path_to_save <- "data/extracted/covariates/social_extrapolation/"
  data <- gdp_ext[[i]]
  save(data, file = paste0(path_to_save, paste0(c(unique(data$variable), unique(data$scenario)), collapse = "_"), ".Rdata"))
  
})


