
############################################################################################################
####################################         Future predictions         ####################################
############################################################################################################

### Load function and data

source("R/sdm_future_function.R")
load("data/extracted/rls_biomass_class.Rdata")

species_name <- colnames(rls_biomass_class)[!colnames(rls_biomass_class) %in% c("survey_id", "site_code", "latitude", "longitude", "ecoregion")]

#################################### Biomass class species_specific model, future, full (env + hum) #####################################

species_biomass <- list.files("results/biomass_baseline_future_hist", full.names = TRUE)

### Random Forest ###

base_dir <- "results/biomass_class_future_rf_hist"
scenario <- "full"
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(pbmcapply::pbmclapply(1:100, function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  all_pred <- sdm_future_function3(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "rf",
                                   scenario = scenario,
                                   mc.cores = 1)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))

biomass_class_future_rf_hist_full <- list.files("results/biomass_class_future_rf_hist_full", full.names = TRUE)
biomass_class_future_rf_hist_full <- lapply(1:length(biomass_class_future_rf_hist_full), function(i) {
  
  load(biomass_class_future_rf_hist_full[i])
  assign(as.character(i), all_pred)
  
})

############################################## Evolution of biomass at global, realm and ecoregion scale for all species ############################################## 
source("R/future_pred_scale_function.R")
source("R/06_score_function.R")
source("R/delta_plot_function.R")

load("data/raw/00_rls_surveys.Rdata")
load("results/decile_save.Rdata")
load("results/performance_bind.Rdata")

decile_save[30,] <- c(0, 0)
decile_save <- decile_save[order(decile_save$biomass_class),]

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")

color_scale <- palette.colors()
color_scale <- as.character(c(color_scale[4], color_scale[3], color_scale[5], color_scale[7]))

rf_plot <- plot_future_pred_scale_function(data = future_pred_scale_function2(data = biomass_class_future_rf_hist_full,
                                                                             unit = "kg",
                                                                             time = as.character(c("2024":"2070"))) |> 
                                             dplyr::group_by(scenario) |> 
                                             dplyr::mutate(lower = (lower / dplyr::first(biomass)) * 100,
                                                           upper = (upper / dplyr::first(biomass)) * 100,
                                                           biomass = (biomass / dplyr::first(biomass)) * 100) |> 
                                             dplyr::ungroup(),
                                           title = "",
                                           x_name = "",
                                           y_name = "Biomass change (%)",
                                           color_lab = "",
                                           breaks = c(2024, 2050, 2070),
                                           color_scale = color_scale)
