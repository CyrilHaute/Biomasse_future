source("R/future_pred_scale_function.R")
source("R/06_score_function.R")
source("R/delta_plot_function.R")

load("results/performance_bind.Rdata")
load("results/decile_save.Rdata")
load("data/raw/00_rls_surveys.Rdata")

decile_save[30,] <- c(0, 0)
decile_save <- decile_save[order(decile_save$biomass_class),]
scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")

color_scale <- palette.colors()
color_scale <- as.character(c(color_scale[4], color_scale[3], color_scale[5], color_scale[7]))

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

load("results/ensemble_model_hist_full.Rdata")

delta_species_survey <- pbmcapply::pbmclapply(1:length(ensemble_model_hist), function(i) {
  
  future_pred_unlog <- future_pred_scale_survey_function(data = ensemble_model_hist[i],
                                                         unit = "kg",
                                                         time = as.character(c("2025":"2075")), # Define the non-analogous year
                                                         score = TRUE)
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_survey_bind <- do.call(rbind, delta_species_survey) |> 
  dplyr::mutate(model = "full") |> 
  dplyr::inner_join(rls_surveys |> 
                      dplyr::select(survey_id, site_code, depth))

delta_species_survey_bind <- delta_species_survey_bind[!is.infinite(delta_species_survey_bind$delta),]

site_biomass_change <- delta_species_survey_bind |> 
  dplyr::group_by(scenario, site_code, depth) |> 
  dplyr::summarise(delta = mean(delta))

species_depth_biomass_change <- delta_species_survey_bind |> 
  dplyr::group_by(scenario, species_name) |> 
  dplyr::summarise(delta = mean(delta),
                   depth = mean(depth))

library(ggplot2)

site_biomass_change |> 
  ggplot(aes(x = depth, y = delta, color = scenario)) + 
  geom_point() +
  # geom_line(stat = "smooth", method = "lm") +
  theme_bw()

species_depth_biomass_change |> 
  ggplot(aes(x = depth, y = delta, color = scenario)) + 
  geom_point() +
  geom_line(stat = "smooth", method = "lm") +
  theme_bw()
