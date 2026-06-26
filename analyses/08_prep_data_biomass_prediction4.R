
#######################################################################################################
##########################   Create species-specific dataset with historic   ##########################   
##################   baseline and future covariates. With and without migration   #####################
#######################################################################################################

### Load function and data

source("R/04_noise_function.R")
source("R/sdm_future_function.R")
load("data/extracted/rls_biomass_class_hist.Rdata")
load("data/extracted/rls_covariates_train_hist2.RData")
load("data/raw/00_rls_surveys.Rdata")
load("data/extracted/env_future_raw_hist2.Rdata")

year <- 2025:2100
names(env_future_raw_hist) <- year

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

species_name <- colnames(rls_biomass_class_hist)[!colnames(rls_biomass_class_hist) %in% c("survey_id", "site_code", "latitude", "longitude", "ecoregion")]

covariates <- rls_covariates_train_hist |>
  dplyr::group_by(survey_id) |>
  dplyr::summarise(dplyr::across(c(chl:marine_ecosystem_dependency, Plateau_500m:Lagoon), mean))
effectiveness <- rls_covariates_train_hist |>
  dplyr::select(survey_id, protection_status) |>
  unique()
covariates <- covariates |>
  dplyr::inner_join(effectiveness) |>
  dplyr::inner_join(rls_surveys[colnames(rls_surveys) %in% c("survey_id", "realm")]) |> 
  tidyr::drop_na()

covariates_ssp126 <- covariates |> 
  dplyr::mutate(scenario = "ssp126")
covariates_ssp245 <- covariates |> 
  dplyr::mutate(scenario = "ssp245")
covariates_ssp370 <- covariates |> 
  dplyr::mutate(scenario = "ssp370")
covariates_ssp585 <- covariates |> 
  dplyr::mutate(scenario = "ssp585")
covariates <- covariates_ssp126 |> 
  dplyr::full_join(covariates_ssp245) |> 
  dplyr::full_join(covariates_ssp370) |> 
  dplyr::full_join(covariates_ssp585)


env_future <- pbmcapply::pbmclapply(1:length(env_future_raw_hist), function(i) {
  
  year_i <- env_future_raw_hist[[i]]
  year_i$survey_id <- as.character(year_i$survey_id)
  
  colnames(year_i)[!colnames(year_i) %in% c("survey_id", "longitude", "latitude", "scenario")] <- gsub(paste0("_", names(env_future_raw_hist[i])), "", colnames(year_i)[!colnames(year_i) %in% c("survey_id", "longitude", "latitude", "scenario")])
  
  year_i$year <- names(env_future_raw_hist)[[i]]
  
  year_i
  
}, mc.cores = parallel::detectCores() - 1)
env_future_bind <- do.call(rbind, env_future)

# Apply a biais correction

cov_name <- c("chl", "ph", "sss", "sst", "gravity", "gdp")

baseline_future <- covariates |> 
  dplyr::mutate(year = "2024") |> 
  dplyr::select(survey_id, chl, ph, sss, sst, gravity, gdp, year, scenario) |> 
  dplyr::full_join(env_future_bind |> 
                     dplyr::select(-c(longitude, latitude)))

cov_biais <- pbmcapply::pbmclapply(1:length(cov_name), function(j) {
  
  cov_i <- baseline_future |> 
    dplyr::select(survey_id, scenario, year, .data[[cov_name[j]]]) |> 
    dplyr::arrange(survey_id)
  
  cov_i_2024 <- cov_i |> 
    dplyr::filter(year == "2024")
  
  cov_i_2025 <- cov_i |> 
    dplyr::filter(year == "2025",
                  survey_id %in% unique(cov_i_2024$survey_id))
  
  biais <- cov_i_2025[,4] - cov_i_2024[,4]
  
  year <- c("2025":"2100")
  
  cov_cor <- lapply(1:length(year), function(k) {
    
    to_return <- cov_i[cov_i$year == year[k] & cov_i$survey_id %in% unique(cov_i_2024$survey_id),]
    
    to_return[,4] <- to_return[,4] - biais
    
    to_return
    
  })
  cov_cor_bind <- do.call(rbind, cov_cor)
  
}, mc.cores = parallel::detectCores() - 1)
cov_biais_bind <- purrr::reduce(cov_biais, dplyr::full_join)

info <- env_future_bind |> 
  dplyr::select(survey_id, longitude, latitude) |> 
  unique()

cov_biais_bind <- cov_biais_bind |> 
  dplyr::inner_join(info)


base_dir <- "results/biomass_baseline_future_hist8/"

require("sf")

unlist(pbmcapply::pbmclapply(1:length(species_name), function(i) {
  
  rls_biomass_i <- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", species_name[i])]
  
  sp <- sdm_abs_bio5(biomass = rls_biomass_i,
                     n_subsample = 1,
                     species_name = species_name[i],
                     covariates = covariates,
                     future_covariates = cov_biais_bind)
  
  dir.create(base_dir)
  
  save(sp, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))
