####################################################################################################
####################################      Create ensemble model     ################################ 
####################################              and               ################################
####################################    plot biomass projection     ################################ 
####################################################################################################

source("R/future_pred_scale_function.R")
source("R/06_score_function.R")
source("R/delta_plot_function.R")

load("data/raw/00_rls_surveys.Rdata")
load("results/decile_save.Rdata")
load("results/performance_bind.Rdata")

performance_bind <- performance_bind_save

decile_save[30,] <- c(0, 0)
decile_save <- decile_save[order(decile_save$biomass_class),]

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")

color_scale <- palette.colors()
color_scale <- as.character(c(color_scale[4], color_scale[3], color_scale[5], color_scale[7]))

biomass_class_future_rf_hist_withoutmigration8_full <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_full", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_full <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_full), function(i) {
  
  load(biomass_class_future_rf_hist_withoutmigration8_full[i])
  assign(as.character(i), all_pred)
  
})
biomass_class_future_rf_hist_withoutmigration8_env <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_env", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_env <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_env), function(i) {
  
  load(biomass_class_future_rf_hist_withoutmigration8_env[i])
  assign(as.character(i), all_pred)
  
})
biomass_class_future_rf_hist_withoutmigration8_hum <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_hum", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_hum <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_hum), function(i) {
  
  load(biomass_class_future_rf_hist_withoutmigration8_hum[i])
  assign(as.character(i), all_pred)
  
})


delta_species_full <- pbmcapply::pbmclapply(1:length(biomass_class_future_rf_hist_withoutmigration8_full), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = biomass_class_future_rf_hist_withoutmigration8_full[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = FALSE,
                                                  biais = FALSE) |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(biomass_class_future_rf_hist_withoutmigration8_full[[i]][[1]]$species_name))
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_full_bind <- do.call(rbind, delta_species_full) |> 
  dplyr::mutate(model = "full")


delta_species_env <- pbmcapply::pbmclapply(1:length(biomass_class_future_rf_hist_withoutmigration8_env), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = biomass_class_future_rf_hist_withoutmigration8_env[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = FALSE,
                                                  biais = FALSE) |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(biomass_class_future_rf_hist_withoutmigration8_env[[i]][[1]]$species_name))
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_env_bind <- do.call(rbind, delta_species_env) |> 
  dplyr::mutate(model = "env")

delta_species_hum <- pbmcapply::pbmclapply(1:length(biomass_class_future_rf_hist_withoutmigration8_hum), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = biomass_class_future_rf_hist_withoutmigration8_hum[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = FALSE,
                                                  biais = FALSE) |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(biomass_class_future_rf_hist_withoutmigration8_hum[[i]][[1]]$species_name))
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_hum_bind <- do.call(rbind, delta_species_hum) |> 
  dplyr::mutate(model = "hum")

delta_species_all <- delta_species_full_bind |> 
  dplyr::full_join(delta_species_env_bind) |>
  dplyr::full_join(delta_species_hum_bind)

summary_delta <- delta_species_all |> 
  dplyr::group_by(model, scenario) |> 
  dplyr::summarise(mean_delta = mean(delta),
                   max_delta = max(delta),
                   min_delta = min(delta),
                   sd_delta = sd(delta))


delta_plot_model <- delta_plot_function_model(delta_species_all = delta_species_all) +
  scale_y_continuous(limits = c(-100, 100))
ggsave("figures/delta_plot_model_hist.png", delta_plot_model, width = 9, height = 5)