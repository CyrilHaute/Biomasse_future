# This script load all the needed data, (covariates and fish biomass data) and 
# select the covariates used in the models

source("R/03_environmental_window_function.R")

#########################################################
############# Load environmental covariates #############
#########################################################

list_extracted_cov_fut <- list.files("data/extracted/covariates/environmental", full.names = TRUE)

chl <- list_extracted_cov_fut[which(grepl("chl", list_extracted_cov_fut) == TRUE)]
ph <- list_extracted_cov_fut[which(grepl("ph", list_extracted_cov_fut) == TRUE)]
sss <- list_extracted_cov_fut[which(grepl("so", list_extracted_cov_fut) == TRUE)]
sst <- list_extracted_cov_fut[which(grepl("to", list_extracted_cov_fut) == TRUE)]

list_extracted_cov_hist <- list.files("data/extracted/covariates/environmental_historical", full.names = TRUE)

chl_hist <- list_extracted_cov_hist[which(grepl("chl", list_extracted_cov_hist) == TRUE)]
ph_hist <- list_extracted_cov_hist[which(grepl("ph", list_extracted_cov_hist) == TRUE)]
sss_hist <- list_extracted_cov_hist[which(grepl("so", list_extracted_cov_hist) == TRUE)]
sst_hist <- list_extracted_cov_hist[which(grepl("to", list_extracted_cov_hist) == TRUE)]

year <- 2025:2100

read_var_chl_fut <- lapply(1:length(chl), function(i) {
  
  load(chl[i])
  assign(paste0("var_", i), data)
  
})
read_var_chl_hist <- lapply(1:length(chl_hist), function(i) {
  
  load(chl_hist[i])
  assign(paste0("var_", i), data)
  
})

read_var_chl_bind_hist <- do.call(rbind, read_var_chl_hist) |> 
  dplyr::filter(!model == "IPSL-CM6A-LR") |> 
  dplyr::select(survey_id, longitude, latitude, x, y, "2005":"2014", model)

read_var_chl_bind_fut <- do.call(rbind, read_var_chl_fut) |> 
  dplyr::filter(!model == "IPSL-CM6A-LR") |> 
  dplyr::select(survey_id, longitude, latitude, x, y, "2015":"2024", model, scenario)

read_var_chl_bind_hist <- read_var_chl_bind_hist |> 
  dplyr::inner_join(read_var_chl_bind_fut) |> 
  dplyr::relocate(model, .after = "2024") |> 
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(dplyr::across("2005":"2024", mean)) |> 
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(chl = rowMeans(dplyr::across("2005":"2024")))

read_var_chl_bind_fut <- do.call(rbind, read_var_chl_fut) |> 
  dplyr::filter(!model == "IPSL-CM6A-LR") |> 
  dplyr::select(survey_id, longitude, latitude, x, y, "2025":"2100", model, scenario) |>
  dplyr::ungroup() |> 
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(dplyr::across("2025":"2100", mean))

# Environmental window
chl_future_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = read_var_chl_bind_fut,
                                info = c("survey_id", "longitude", "latitude", "scenario"),
                                varname = "chl")
  
}, mc.cores = parallel::detectCores() - 1)
chl_future_window_reduce <- purrr::reduce(chl_future_window, dplyr::full_join)

chl_fut_hist <- read_var_chl_bind_hist |> 
  dplyr::inner_join(chl_future_window_reduce) |> 
  dplyr::relocate(scenario, .after = latitude)

read_var_chl_bind_hist <- chl_fut_hist[,colnames(chl_fut_hist) %in% c("survey_id", "longitude", "latitude", "scenario", "chl")]

chl_future_window_reduce <- chl_fut_hist[,!colnames(chl_fut_hist) %in% c("chl")]


read_var_ph_fut <- lapply(1:length(ph), function(i) {
  
  load(ph[i])
  assign(paste0("var_", i), data)
  
})
read_var_ph_hist <- lapply(1:length(ph_hist), function(i) {
  
  load(ph_hist[i])
  assign(paste0("var_", i), data)
  
})

read_var_ph_bind_hist <- do.call(rbind, read_var_ph_hist) |> 
  dplyr::select(survey_id, longitude, latitude, x, y, "2005":"2014", model)

read_var_ph_bind_fut <- do.call(rbind, read_var_ph_fut) |> 
  dplyr::select(survey_id, longitude, latitude, x, y, "2015":"2024", model, scenario)

read_var_ph_bind_hist <- read_var_ph_bind_hist |> 
  dplyr::inner_join(read_var_ph_bind_fut) |> 
  dplyr::relocate(model, .after = "2024") |> 
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(dplyr::across("2005":"2024", mean)) |> 
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(ph = rowMeans(dplyr::across("2005":"2024")))

read_var_ph_bind_fut <- do.call(rbind, read_var_ph_fut) |>
  dplyr::select(survey_id, longitude, latitude, x, y, "2025":"2100", model, scenario) |>
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(across("2025":"2100", mean))

# Environmental window
ph_future_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = read_var_ph_bind_fut,
                                info = c("survey_id", "longitude", "latitude", "scenario"),
                                varname = "ph")
  
}, mc.cores = parallel::detectCores() - 1)
ph_future_window_reduce <- purrr::reduce(ph_future_window, dplyr::full_join)

ph_fut_hist <- read_var_ph_bind_hist |> 
  dplyr::inner_join(ph_future_window_reduce) |> 
  dplyr::relocate(scenario, .after = latitude)

read_var_ph_bind_hist <- ph_fut_hist[,colnames(ph_fut_hist) %in% c("survey_id", "longitude", "latitude", "scenario", "ph")]

ph_future_window_reduce <- ph_fut_hist[,!colnames(ph_fut_hist) %in% c("ph")]


read_var_sss_fut <- lapply(1:length(sss), function(i) {
  
  load(sss[i])
  assign(paste0("var_", i), data)
  
})
read_var_sss_hist <- lapply(1:length(sss_hist), function(i) {
  
  load(sss_hist[i])
  assign(paste0("var_", i), data)
  
})

read_var_sss_bind_hist <- do.call(rbind, read_var_sss_hist) |> 
  dplyr::select(survey_id, longitude, latitude, x, y, "2005":"2014", model)

read_var_sss_bind_fut <- do.call(rbind, read_var_sss_fut) |> 
  dplyr::select(survey_id, longitude, latitude, x, y, "2015":"2024", model, scenario)

read_var_sss_bind_hist <- read_var_sss_bind_hist |> 
  dplyr::inner_join(read_var_sss_bind_fut) |> 
  dplyr::relocate(model, .after = "2024") |> 
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(dplyr::across("2005":"2024", mean)) |> 
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(sss = rowMeans(dplyr::across("2005":"2024")))

read_var_sss_bind_fut <- do.call(rbind, read_var_sss_fut) |>
  dplyr::select(survey_id, longitude, latitude, x, y, "2025":"2100", model, scenario) |>
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(across("2025":"2100", mean))

# Environmental window
sss_future_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = read_var_sss_bind_fut,
                                info = c("survey_id", "longitude", "latitude", "scenario"),
                                varname = "sss")
  
}, mc.cores = parallel::detectCores() - 1)
sss_future_window_reduce <- purrr::reduce(sss_future_window, dplyr::full_join)

sss_fut_hist <- read_var_sss_bind_hist |> 
  dplyr::inner_join(sss_future_window_reduce) |> 
  dplyr::relocate(scenario, .after = latitude)

read_var_sss_bind_hist <- sss_fut_hist[,colnames(sss_fut_hist) %in% c("survey_id", "longitude", "latitude", "scenario", "sss")]

sss_future_window_reduce <- sss_fut_hist[,!colnames(sss_fut_hist) %in% c("sss")]


read_var_sst_fut <- lapply(1:length(sst), function(i) {
  
  load(sst[i])
  assign(paste0("var_", i), data)
  
})
read_var_sst_hist <- lapply(1:length(sst_hist), function(i) {
  
  load(sst_hist[i])
  assign(paste0("var_", i), data)
  
})

read_var_sst_bind_hist <- do.call(rbind, read_var_sst_hist) |> 
  dplyr::select(survey_id, longitude, latitude, x, y, "2005":"2014", model)

read_var_sst_bind_fut <- do.call(rbind, read_var_sst_fut) |> 
  dplyr::select(survey_id, longitude, latitude, x, y, "2015":"2024", model, scenario)

read_var_sst_bind_hist <- read_var_sst_bind_hist |> 
  dplyr::inner_join(read_var_sst_bind_fut) |> 
  dplyr::relocate(model, .after = "2024") |> 
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(dplyr::across("2005":"2024", mean)) |> 
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(sst = rowMeans(dplyr::across("2005":"2024")))

read_var_sst_bind_fut <- do.call(rbind, read_var_sst_fut) |>
  dplyr::select(survey_id, longitude, latitude, x, y, "2025":"2100", model, scenario) |>
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(across("2025":"2100", mean))

# Environmental window
sst_future_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = read_var_sst_bind_fut,
                                info = c("survey_id", "longitude", "latitude", "scenario"),
                                varname = "sst")
  
}, mc.cores = parallel::detectCores() - 1)
sst_future_window_reduce <- purrr::reduce(sst_future_window, dplyr::full_join)

sst_fut_hist <- read_var_sst_bind_hist |> 
  dplyr::inner_join(sst_future_window_reduce) |> 
  dplyr::relocate(scenario, .after = latitude)

read_var_sst_bind_hist <- sst_fut_hist[,colnames(sst_fut_hist) %in% c("survey_id", "longitude", "latitude", "scenario", "sst")]

sst_future_window_reduce <- sst_fut_hist[,!colnames(sst_fut_hist) %in% c("sst")]

env_train_hist <- read_var_chl_bind_hist |> 
  dplyr::inner_join(read_var_ph_bind_hist) |> 
  dplyr::inner_join(read_var_sss_bind_hist) |> 
  dplyr::inner_join(read_var_sst_bind_hist)

env_train_hist$survey_id <- as.character(env_train_hist$survey_id)

save(env_train_hist, file = "data/extracted/env_train_hist2.Rdata")



load("data/raw/00_rls_surveys.Rdata")
load("data/extracted/env_train_hist2.Rdata")
rls_surveys$survey_id <- as.character(rls_surveys$survey_id)
env_train_hist$survey_id <- as.character(env_train_hist$survey_id)

###################################################
############# Load habitat covariates #############
###################################################

load("data/raw/covariates/habitat/final_habitat.Rdata")

# As the sum of habitats within a buffer do not always fit 100%, we converted percentage to relative in order to remove deep sea and land cover
# and acount only for coral habitats. First we assessed the sum of all habitat (either benthic and geomorphologic), for both buffers (500m, 10km),
# to keep the raw information of the sum of all habitat.
######################################################################################
rls_habitat <- final_new_habitat |>  
  dplyr::rowwise() |> 
  dplyr::mutate(sum_geo_10 = sum(dplyr::c_across(colnames(final_new_habitat)[25:35])), # Assess reef extent from reef geomorphology
                sum_bent_500 = sum(dplyr::c_across(colnames(final_new_habitat)[2:7])), # Assess sum of habitat either benthic and geomorphomogic for both buffers 500m and 10km
                sum_geo_500 = sum(dplyr::c_across(colnames(final_new_habitat)[8:18])),
                sum_bent_10 = sum(dplyr::c_across(colnames(final_new_habitat)[19:24])))

# Convert to relatif percentage

rls_habitat <- rls_habitat |> 
  dplyr::rowwise() |> 
  dplyr::mutate((dplyr::across(colnames(final_new_habitat)[2:7]) * 100) / sum_bent_500,
                (dplyr::across(colnames(final_new_habitat)[8:18]) * 100) / sum_geo_500,
                (dplyr::across(colnames(final_new_habitat)[19:24]) * 100) / sum_bent_10,
                (dplyr::across(colnames(final_new_habitat)[25:35]) * 100) / sum_geo_10)

rls_habitat <- rls_habitat |> 
  dplyr::rename(reef_extent = sum_geo_10)

rls_habitat$survey_id <- as.character(rls_habitat$survey_id)

rls_habitat <- rls_habitat |> 
  dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")])


##################################################
############# Load social covariates #############
##################################################

gdp <- list.files("data/extracted/covariates/social", full.names = TRUE)
gdp <- gdp[which(grepl(pattern = "gdp", gdp))]

read_var_gdp <- lapply(1:length(gdp), function(i) {
  
  load(gdp[i])
  assign(paste0("var_", i), data)
  
})
read_var_gdp_bind <- do.call(rbind, read_var_gdp) |>
  dplyr::filter(!scenario %in% c("ssp4")) |>
  dplyr::select(survey_id, longitude, latitude, x, y, "2010":"2020", model, scenario) |>
  dplyr::group_by(survey_id, longitude, latitude, scenario) |>
  dplyr::summarise(dplyr::across("2010":"2020", mean)) |>
  dplyr::group_by(survey_id, longitude, latitude, scenario) |>
  dplyr::summarise(gdp = rowMeans(dplyr::across("2010":"2020"))) |>
  dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp1" ~ "ssp126",
                                            scenario == "ssp2" ~ "ssp245",
                                            scenario == "ssp3" ~ "ssp370",
                                            scenario == "ssp5" ~ "ssp585"))

read_var_gdp_future <- do.call(rbind, read_var_gdp) |>
  dplyr::filter(!scenario %in% c("ssp4")) |>
  dplyr::select(survey_id, longitude, latitude, x, y, "2030":"2100", model, scenario) |>
  dplyr::group_by(survey_id, longitude, latitude, scenario) |>
  dplyr::summarise(dplyr::across("2030":"2100", mean)) |>
  dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp1" ~ "ssp126",
                                            scenario == "ssp2" ~ "ssp245",
                                            scenario == "ssp3" ~ "ssp370",
                                            scenario == "ssp5" ~ "ssp585"))
year <- colnames(read_var_gdp_future)[!colnames(read_var_gdp_future) %in% c("survey_id", "longitude", "latitude", "scenario")]

read_var_gdp_future_full <- read_var_gdp_future |>
  dplyr::select(survey_id:scenario) |>
  dplyr::mutate(!!!setNames(rep(list(NA), length(2023:2100)), as.character(2023:2100)))
year_full <- colnames(read_var_gdp_future_full)[!colnames(read_var_gdp_future_full) %in% c("survey_id", "longitude", "latitude", "scenario")]

read_var_gdp_future_full <- pbmcapply::pbmclapply(1:length(year_full), function(i) {
  
  year_i <- read_var_gdp_future_full |>
    dplyr::select(survey_id, longitude, latitude, scenario, year_full[i])
  
  year_i[,colnames(year_i) %in% year_full[i]] <- read_var_gdp_future[,colnames(read_var_gdp_future) %in% year[which.min(abs(as.numeric(year) - as.numeric(year_full[i])))]]
  
  return(year_i)
  
}, mc.cores = parallel::detectCores() - 1)
read_var_gdp_future_full <- purrr::reduce(read_var_gdp_future_full, dplyr::inner_join)
colnames(read_var_gdp_future_full)[colnames(read_var_gdp_future_full) %in% year_full] <- paste0("gdp_", colnames(read_var_gdp_future_full)[colnames(read_var_gdp_future_full) %in% year_full])

load("data/extracted/covariates/social/gravity.Rdata")

rls_sur_coord <- rls_surveys |> 
  dplyr::select(survey_id, latitude, longitude) |> 
  dplyr::mutate(latitude = round(latitude, 6),
                longitude = round(longitude, 6))

gravity_coord <- gravity_scenario_baseline_bind |>
  dplyr::select(latitude, longitude) |>
  unique() |> 
  dplyr::mutate(latitude = round(latitude, 6),
                longitude = round(longitude, 6)) |> 
  dplyr::inner_join(rls_sur_coord)

gravity <- gravity_scenario_baseline_bind |> 
  dplyr::select(-id) |> 
  dplyr::inner_join(gravity_coord) |>
  dplyr::mutate(scenario = dplyr::case_when(scenario == "SSP1" ~ "ssp126",
                                            scenario == "SSP2" ~ "ssp245",
                                            scenario == "SSP3" ~ "ssp370",
                                            scenario == "SSP5" ~ "ssp585"))
gravity$gravity <- log10(mean(gravity$gravity) + 1)

gravity <- gravity |> 
  tidyr::pivot_wider(names_from = year, values_from = gravity)

gravity_future <- gravity[,!colnames(gravity) %in% c("2020")]
gravity <- gravity[,!colnames(gravity) %in% c("2030":"2100")] |> 
  dplyr::rename(gravity = `2020`)

year <- colnames(gravity_future)[colnames(gravity_future) %in% c("2030":"2100")]

gravity_future_full <- gravity_future |>
  dplyr::select(!`2030`:`2100`) |>
  dplyr::mutate(!!!setNames(rep(list(NA), length(2023:2100)), as.character(2023:2100)))
year_full <- colnames(gravity_future_full)[!colnames(gravity_future_full) %in% c("survey_id", "longitude", "latitude", "scenario")]


gravity_future_full <- pbmcapply::pbmclapply(1:length(year_full), function(i) {
  
  year_i <- gravity_future_full |>
    dplyr::select(survey_id, longitude, latitude, scenario, year_full[i])
  
  year_i[,colnames(year_i) %in% year_full[i]] <- gravity_future[,colnames(gravity_future) %in% year[which.min(abs(as.numeric(year) - as.numeric(year_full[i])))]]
  
  return(year_i)
  
}, mc.cores = parallel::detectCores() - 1)
gravity_future_full <- purrr::reduce(gravity_future_full, dplyr::inner_join)
colnames(gravity_future_full)[colnames(gravity_future_full) %in% year_full] <- paste0("gravity_", colnames(gravity_future_full)[colnames(gravity_future_full) %in% year_full])


#----------------- Extract MPA data ---------------------
load("data/raw/00_rls_surveys.Rdata")

mpa_csv <- read.csv("data/raw/covariates/social/rls_mpa_MASTER_09012025.csv", header = TRUE)

## Check the date of MPA vs date of surveys
mpa_csv_rls <- rls_surveys |>
  dplyr::inner_join(dplyr::select(mpa_csv, -site_name, -latitude, -longitude, -country)) |>
  dplyr::select(survey_id, site_code, latitude, longitude, survey_date, year,
                rls_mpa, rls_protection_status_old, protected_seas_id,
                year_of_protection, level_fishing_protection, compliance,
                zone_marine_area_km, data_source, comments)

mpa_csv_rls[mpa_csv_rls == ""] <- NA
mpa_csv_rls <- mpa_csv_rls |>
  dplyr::mutate(protection_to_date = ifelse(year_of_protection < year |
                                              is.na(year_of_protection),
                                            level_fishing_protection, 0),
                age_of_MPA = year - year_of_protection )

## Recode Protection status:
mpa <- mpa_csv_rls |>
  dplyr::mutate(
    #Simple classification
    protection_status = dplyr::case_when(
      # full protection: no-take, with high or medium compliance
      protection_to_date == 5 &
        compliance %in% c("High", "Medium") ~ "full",
      # Restricted: other mpa
      (protection_to_date == 5 & compliance %in% c("Low")) |
        (protection_to_date == 5 & is.na(compliance)) |
        (protection_to_date >= 1 & protection_to_date <= 4)  ~ "restricted",
      # Out: no protection
      protection_to_date == 0 ~ "out",
      # NA if conditions doesn't match -> check the NAs.
      TRUE ~ NA_character_
    ),
    #More detailed classification -> depend on the quality of the data
    protection_status_detailed = dplyr::case_when(
      # High protection: no-take, high/medium enforcement, >10 km2, >10 years
      protection_to_date == 5 &
        compliance %in% c("High", "Medium") &
        zone_marine_area_km >= 10 &
        age_of_MPA >= 10 ~ "high",
      # Medium protection: no-take, high/medium enforcement, but either <10 km2 or <10 years
      protection_to_date == 5 &
        compliance %in% c("High", "Medium") &
        (zone_marine_area_km < 10 | age_of_MPA < 10 | is.na(zone_marine_area_km) |
           is.na(age_of_MPA)) ~ "medium",
      # Low protection: no-take with low enforcement and other MPAs
      protection_to_date == 5 & ( compliance == "Low" | is.na(compliance)) |
        (protection_to_date >= 1 & protection_to_date <= 4) ~ "low",
      # Out : no MPAs
      protection_to_date == 0 ~ "out",
      # Valeur par défaut si aucun critère ne correspond (optionnel, pour éviter les NA)
      TRUE ~ NA_character_
    )
  )

load("data/raw/covariates/social/corruption.RData")
load("data/raw/covariates/social/hdi.RData")
load("data/raw/covariates/social/marine_ecosystem_dependency.RData")
load("data/raw/covariates/social/natural_ressource_rent.RData")
load("data/raw/covariates/social/neartt.RData")
load("data/raw/covariates/social/ngo.RData")
load("data/raw/covariates/social/fishing_boat.RData")
load("data/raw/covariates/social/noviolence.RData")
load("data/raw/covariates/social/voice.RData")

ngo <- ngo[which(is.na(ngo$ngo) == FALSE),]
mpa <- mpa[,colnames(mpa) %in% c("survey_id", "latitude", "longitude", "protection_status")]

rls_soc <- corruption |> 
  dplyr::inner_join(hdi) |> 
  dplyr::inner_join(med) |> 
  dplyr::inner_join(mpa) |> 
  dplyr::inner_join(natural_ressource_rent) |> 
  dplyr::inner_join(neartt) |> 
  dplyr::inner_join(ngo) |> 
  dplyr::inner_join(fishing_boat) |> 
  dplyr::inner_join(noviolence) |> 
  dplyr::inner_join(voice)

rls_soc$protection_status <- as.factor(rls_soc$protection_status)
rls_soc$survey_id <- as.character(rls_soc$survey_id)

read_var_gdp_bind$survey_id <- as.character(read_var_gdp_bind$survey_id)

########### Covariates for training the models #########

gravity$survey_id <- as.character(gravity$survey_id)

rls_covariates_train <- env_train_hist |> 
  dplyr::ungroup() |> 
  dplyr::mutate(latitude = round(latitude, 6),
                longitude = round(longitude, 6)) |> 
  dplyr::inner_join(rls_soc |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6))) |> 
  dplyr::inner_join(read_var_gdp_bind |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6))) |>
  dplyr::inner_join(gravity |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6))) |>
  dplyr::inner_join(rls_habitat |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6)))

rls_covariates_train$survey_id <- as.character(rls_covariates_train$survey_id)

rls_covariates_train$scenario <- as.factor(rls_covariates_train$scenario)

rls_info <- unique(rls_covariates_train[colnames(rls_covariates_train) %in% c("survey_id", "longitude", "latitude")])

rls_covariates_train_split <- rls_covariates_train |> 
  dplyr::as_tibble() |> 
  dplyr::group_split(scenario)
rls_covariates_train_split <- lapply(rls_covariates_train_split, as.data.frame)

##################################################
############# Load fish biomass data #############
##################################################

load("data/raw/00_rls_surveys.Rdata")
load("data/raw/RLS_actinopterygii_data.Rdata")

RLS_actinopterygii_data <- RLS_actinopterygii_data |> 
  dplyr::rename(species_name = rls_species_name)

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)
rls_fish_data <- dplyr::inner_join(RLS_actinopterygii_data, rls_surveys)

rls_coral_fish <- rls_fish_data |>
  dplyr::filter(survey_id %in% rls_info$survey_id)

rls_coral_fish_mean_biomass <- rls_coral_fish |> 
  dplyr::group_by(survey_id, site_code, species_name, latitude, longitude, survey_date, depth) |> 
  dplyr::summarise(biomass = sum(biomass))

############################# Fish species biomass class ############################ 

rls_coral_fish_mean_biomass_class <- rls_coral_fish_mean_biomass |>
  dplyr::ungroup() |>
  dplyr::select(survey_id, species_name, biomass, latitude, longitude)

rls_coral_fish_mean_biomass_class_count <- rls_coral_fish_mean_biomass_class |>
  dplyr::group_by(species_name) |>
  dplyr::mutate(count = dplyr::n()) |>
  dplyr::filter(count >= 50) |>
  dplyr::select(survey_id, species_name, biomass, latitude, longitude)

deciles_species <- quantile(rls_coral_fish_mean_biomass_class_count$biomass, probs = seq(0, 1, by = 0.05))

rls_coral_fish_class <- rls_coral_fish_mean_biomass_class_count |> 
  dplyr::mutate(biomass_class = cut(biomass, breaks = c(-Inf, deciles_species[-1]), labels = 1:20, include.lowest = TRUE))

deciles_species_20 <- quantile(rls_coral_fish_class[rls_coral_fish_class$biomass_class == 20,]$biomass, probs = seq(0, 1, by = 0.1))

rls_coral_fish_class$biomass_class <- as.character(rls_coral_fish_class$biomass_class)

rls_coral_fish_class[rls_coral_fish_class$biomass_class == 20,]$biomass_class <- cut(rls_coral_fish_class[rls_coral_fish_class$biomass_class == 20,]$biomass, breaks = c(-Inf, deciles_species_20[-1]), labels = 20:29, include.lowest = TRUE)

decile_save <- data.frame(biomass_class = 1:29,
                          biomass = c(deciles_species[-c(1, 21)], deciles_species_20[-1]))

save(decile_save, file = "results/decile_save.Rdata")

rls_spread_coral_reef_class <- rls_coral_fish_class |>
  dplyr::select(-biomass) |> 
  tidyr::spread(species_name, biomass_class, fill = "0")

##############################################
############# Variable selection #############
##############################################

# Select environmental covariates
rls_env_selec <- rls_covariates_train_split[[1]][, colnames(env_train_hist)]

cor_env <- stats::cor(rls_env_selec[!colnames(rls_env_selec) %in% c("survey_id", "longitude", "latitude", "scenario")]) #Look at correlation between covariates
corrplot::corrplot(cor_env, type = "upper")

rls_env_selec2 <- rls_env_selec[,c("chl", "ph" , "sss", "sst")]
cor_env2 <- stats::cor(rls_env_selec2)
corrplot::corrplot(cor_env2, type = "upper")

rls_env_final <- lapply(1:length(rls_covariates_train_split), function(i) {
  
  rls_covariates_train_split[[i]][,c("survey_id", "scenario", "chl", "ph" , "sss", "sst")]
  
})

# Select social covariates
rls_soc_selec <- rls_covariates_train_split[[1]][, c(colnames(rls_soc), 
                                                     colnames(read_var_gdp_bind)[colnames(read_var_gdp_bind) %in% "gdp"],
                                                     colnames(gravity)[colnames(gravity) %in% "gravity"])]

cor_soc <- stats::cor(rls_soc_selec[!colnames(rls_soc_selec) %in% c("survey_id", "longitude", "latitude", "scenario", "protection_status")])
corrplot::corrplot(cor_soc, type = "upper")

rls_soc_selec2 <- rls_soc_selec[,c("gravity", "neartt", "gdp", "hdi", "ngo", "marine_ecosystem_dependency")]
cor_soc2 <- stats::cor(rls_soc_selec2)
corrplot::corrplot(cor_soc2, type = "upper")

rls_soc_final <- lapply(1:length(rls_covariates_train_split), function(i) {
  
  rls_covariates_train_split[[i]][,c("survey_id", "scenario", "gravity", "neartt", "gdp", "hdi", "ngo", "marine_ecosystem_dependency", "protection_status")]
  
})

# Select habitat covariates
rls_hab_selec <- rls_covariates_train_split[[1]][, colnames(rls_habitat)]
rls_hab_selec <- rls_hab_selec |> 
  dplyr::inner_join(rls_surveys[,c("survey_id", "depth")])

cor_hab <- stats::cor(rls_hab_selec[!colnames(rls_hab_selec) %in% c("survey_id", "longitude", "latitude")])
corrplot::corrplot(cor_hab, type = "upper")

rls_hab_selec2 <- rls_hab_selec[,c("depth", "reef_extent", "Reef_Slope_500m", "Back_Reef_Slope_500m", "Outer_Reef_Flat_500m",
                                   "Inner_Reef_Flat_500m", "Deep_Lagoon_500m", "Shallow_Lagoon_500m", "Reef_Crest_500m",
                                   "Terrestrial_Reef_Flat_500m", "Plateau_500m", "Patch_Reefs_500m")]
cor_hab2 <- stats::cor(rls_hab_selec2)
corrplot::corrplot(cor_hab2, type = "upper")

rls_hab_final <- lapply(1:length(rls_covariates_train_split), function(i) {
  
  hab_i <- rls_covariates_train_split[[i]][, colnames(rls_covariates_train_split[[i]]) %in% c("survey_id", "reef_extent", "Reef_Slope_500m", "Back_Reef_Slope_500m", "Outer_Reef_Flat_500m",
                                                                                              "Inner_Reef_Flat_500m", "Deep_Lagoon_500m", "Shallow_Lagoon_500m", "Reef_Crest_500m",
                                                                                              "Terrestrial_Reef_Flat_500m", "Plateau_500m", "Patch_Reefs_500m")] |> 
    dplyr::inner_join(rls_surveys[,c("survey_id", "depth")])
  
  hab_i <- hab_i |> 
    dplyr::mutate(Reef_Slope = Reef_Slope_500m + Back_Reef_Slope_500m,
                  Reef_Flat = Inner_Reef_Flat_500m + Outer_Reef_Flat_500m,
                  Lagoon = Deep_Lagoon_500m + Shallow_Lagoon_500m)
  
  hab_i <- hab_i |> 
    dplyr::select(-c(Reef_Slope_500m, Back_Reef_Slope_500m, Inner_Reef_Flat_500m, Outer_Reef_Flat_500m, Deep_Lagoon_500m, Shallow_Lagoon_500m))
  
})


#####################################################################
########  Save fish biomass and train/future covariates data  #######
#####################################################################

species_name <- colnames(rls_spread_coral_reef_class)[!colnames(rls_spread_coral_reef_class) %in% c("survey_id", "latitude", "longitude")]

rls_biomass_class <- rls_spread_coral_reef_class |>
  dplyr::inner_join(rls_surveys[,c("survey_id", "latitude", "longitude", "site_code")])

rls_biomass_class_hist <- rls_biomass_class |>
  dplyr::ungroup() |> 
  dplyr::select(survey_id,
                latitude,
                longitude,
                site_code,
                species_name)

save(rls_biomass_class_hist, file = "data/extracted/rls_biomass_class_hist.Rdata")


rls_covariates_train_ssp126 <- rls_env_final[[1]] |> 
  dplyr::inner_join(rls_soc_final[[1]]) |> 
  dplyr::inner_join(rls_hab_final[[1]])

rls_covariates_train_ssp245 <- rls_env_final[[2]] |> 
  dplyr::inner_join(rls_soc_final[[2]]) |> 
  dplyr::inner_join(rls_hab_final[[2]])

rls_covariates_train_ssp370 <- rls_env_final[[3]] |> 
  dplyr::inner_join(rls_soc_final[[3]]) |> 
  dplyr::inner_join(rls_hab_final[[3]])

rls_covariates_train_ssp585 <- rls_env_final[[4]] |> 
  dplyr::inner_join(rls_soc_final[[4]]) |> 
  dplyr::inner_join(rls_hab_final[[4]])

rls_covariates_train_hist <- rls_covariates_train_ssp126 |> 
  dplyr::full_join(rls_covariates_train_ssp245) |> 
  dplyr::full_join(rls_covariates_train_ssp370) |> 
  dplyr::full_join(rls_covariates_train_ssp585)

save(rls_covariates_train_hist, file = "data/extracted/rls_covariates_train_hist2.RData")

gravity_future_full$survey_id <- as.numeric(gravity_future_full$survey_id)

list_future <- list(chl_future_window_reduce |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6)),
                    # dhw_future_window_reduce,
                    # npp_future_window_reduce,
                    # o2_future_window_reduce,
                    ph_future_window_reduce |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6)),
                    sss_future_window_reduce |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6)),
                    sst_future_window_reduce |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6)),
                    read_var_gdp_future_full |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6)),
                    gravity_future_full |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6))
)

year <- 2025:2100

# Create future env dataframe per year
env_future_raw_hist <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  year_i <- year[i]
  
  col_name_future <- lapply(1:length(list_future), function(j) { 
    
    colnames(list_future[[j]])
    
  })
  
  which_var_year_i <- lapply(1:length(col_name_future), function(j) {
    
    col_name_future[[j]][which(grepl(year_i, col_name_future[[j]]) == TRUE)]
    
  })
  
  var_year_i <- lapply(1:length(list_future), function(j) {
    
    list_future[[j]][,c("survey_id", "longitude", "latitude", "scenario", which_var_year_i[[j]])]
    
  })
  
  var_year_i_bind <- purrr::reduce(var_year_i, dplyr::inner_join)
  
}, mc.cores = parallel::detectCores() - 1)
names(env_future_raw_hist) <- year

save(env_future_raw_hist, file = "data/extracted/env_future_raw_hist2.Rdata")
