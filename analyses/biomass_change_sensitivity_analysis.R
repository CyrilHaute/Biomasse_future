source("R/future_pred_scale_function.R")
source("R/06_score_function.R")
source("R/delta_plot_function.R")

load("results/performance_bind.Rdata")
load("results/decile_save.Rdata")

performance_bind <- performance_bind_save

decile_save[30,] <- c(0, 0)
decile_save <- decile_save[order(decile_save$biomass_class),]
scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")

color_scale <- palette.colors()
color_scale <- as.character(c(color_scale[4], color_scale[3], color_scale[5], color_scale[7]))

migration_ensemble <- readRDS("results/migration_ensemble8.rds")

# species_to_remove <- c("Acanthurus grammoptilus",
#                        "Amblyglyphidodon curacao", 
#                        "Chaetodon kleinii", "Chaetodon ornatissimus",
#                        "Chromis cyanea", "Epinephelus daemelii", "Fistularia commersonii", 
#                        "Gnathodentex aureolineatus", "Goniistius vestitus", "Hologymnosus annulatus", 
#                        "Hypoplectrus nigricans", "Lutjanus carponotatus", 
#                        "Neoglyphidodon nigroris", "Neoglyphidodon polyacanthus", "Novaculichthys taeniourus", 
#                        "Parupeneus spilurus", "Plectorhinchus multivittatus", 
#                        "Pomacentrus milleri", "Pomacentrus reidi", "Pseudanthias huchtii", 
#                        "Pterocaesio pisang", "Stegastes nigricans", 
#                        "Thalassoma amblycephalum")
load("data/extracted/species_to_remove_dtf.Rdata")
species_to_remove <- species_to_remove_dtf
species_to_remove <- unique(species_to_remove$species)
species_to_remove <- stringr::str_replace_all(species_to_remove, " ", "_")
list_species <- gsub(".Rdata", "", list.files("results/biomass_class_future_rf_hist_migration8_full", full.names = FALSE))

which_species_to_remove <- which(list_species %in% species_to_remove)

migration_ensemble <- migration_ensemble[-which_species_to_remove]

mean_species <- performance_bind |> 
  dplyr::group_by(species_name) |> 
  dplyr::summarise(mean_r2 = mean(r2))

perf_test_0.5 <- mean_species |> 
  dplyr::filter(mean_r2 > quantile(mean_r2, probs = 0.5))
perf_test_0.75 <- mean_species|> 
  dplyr::filter(mean_r2 > quantile(mean_r2, probs = 0.75))
perf_test_0.95 <- mean_species |> 
  dplyr::filter(mean_r2 > quantile(mean_r2, probs = 0.95))



ensemble_model_scale <- future_pred_scale_function(data = migration_ensemble,
                                                   unit = "kg",
                                                   time = as.character(c("2024":"2075")),
                                                   score = TRUE,
                                                   biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup()
ensemble_model_plot_fig1 <- plot_future_pred_scale_function(data = ensemble_model_scale |> 
                                                              dplyr::mutate(scenario = dplyr::recode(scenario,
                                                                                                     "ssp126" = "SSP1-2.6",
                                                                                                     "ssp245" = "SSP2-4.5",
                                                                                                     "ssp370" = "SSP3-7.0",
                                                                                                     "ssp585" = "SSP5-8.5"),
                                                                            model = "Ensemble model"),
                                                            title = paste0("All species, n = ", length(migration_ensemble), ", R²min-max = ", round(min(mean_species$mean_r2), 3), "-", round(max(mean_species$mean_r2), 3)),
                                                            x_name = "",
                                                            y_name = "Biomass change (%)",
                                                            color_lab = "",
                                                            breaks = c(2024, 2050, 2075),
                                                            # y_scale = c(-42, 3),
                                                            model_facet = FALSE,
                                                            color_scale = color_scale)


species <- sapply(1:length(migration_ensemble), function(i) {
  
  unique(migration_ensemble[[i]][[1]]$species_name)
  
})
species_0.5 <- which(species %in% perf_test_0.5$species_name)
ensemble_model_scale_0.5 <- future_pred_scale_function(data = migration_ensemble[species_0.5],
                                                       unit = "kg",
                                                       time = as.character(c("2024":"2075")),
                                                       score = TRUE,
                                                       biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup()
ensemble_model_0.5_plot <- plot_future_pred_scale_function(data = ensemble_model_scale_0.5 |> 
                                                             dplyr::mutate(scenario = dplyr::recode(scenario,
                                                                                                    "ssp126" = "SSP1-2.6",
                                                                                                    "ssp245" = "SSP2-4.5",
                                                                                                    "ssp370" = "SSP3-7.0",
                                                                                                    "ssp585" = "SSP5-8.5"),
                                                                           model = "Ensemble model"),
                                                           title = paste0("Quantile 0.5, n = ", length(species_0.5), ", R²min-max = ", round(min(perf_test_0.5$mean_r2), 3), "-", round(max(perf_test_0.5$mean_r2), 3)),
                                                           x_name = "",
                                                           y_name = "Biomass change (%)",
                                                           color_lab = "",
                                                           breaks = c(2024, 2050, 2075),
                                                           # y_scale = c(-42, 3),
                                                           model_facet = FALSE,
                                                           color_scale = color_scale)

species_0.75 <- which(species %in% perf_test_0.75$species_name)
ensemble_model_scale_0.75 <- future_pred_scale_function(data = migration_ensemble[species_0.75],
                                                        unit = "kg",
                                                        time = as.character(c("2024":"2075")),
                                                        score = TRUE,
                                                        biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup()
ensemble_model_0.75_plot <- plot_future_pred_scale_function(data = ensemble_model_scale_0.75 |> 
                                                             dplyr::mutate(scenario = dplyr::recode(scenario,
                                                                                                    "ssp126" = "SSP1-2.6",
                                                                                                    "ssp245" = "SSP2-4.5",
                                                                                                    "ssp370" = "SSP3-7.0",
                                                                                                    "ssp585" = "SSP5-8.5"),
                                                                           model = "Ensemble model"),
                                                           title = paste0("Quantile 0.75, n = ", length(species_0.75), ", R²min-max = ", round(min(perf_test_0.75$mean_r2), 3), "-", round(max(perf_test_0.75$mean_r2), 3)),
                                                           x_name = "",
                                                           y_name = "Biomass change (%)",
                                                           color_lab = "",
                                                           breaks = c(2024, 2050, 2075),
                                                           # y_scale = c(-42, 3),
                                                           model_facet = FALSE,
                                                           color_scale = color_scale)


species_0.95 <- which(species %in% perf_test_0.95$species_name)
ensemble_model_scale_0.95 <- future_pred_scale_function(data = migration_ensemble[species_0.95],
                                                        unit = "kg",
                                                        time = as.character(c("2024":"2075")),
                                                        score = TRUE,
                                                        biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup()
ensemble_model_0.95_plot <- plot_future_pred_scale_function(data = ensemble_model_scale_0.95 |> 
                                                              dplyr::mutate(scenario = dplyr::recode(scenario,
                                                                                                     "ssp126" = "SSP1-2.6",
                                                                                                     "ssp245" = "SSP2-4.5",
                                                                                                     "ssp370" = "SSP3-7.0",
                                                                                                     "ssp585" = "SSP5-8.5"),
                                                                            model = "Ensemble model"),
                                                            title = paste0("Quantile 0.95, n = ", length(species_0.95), ", R²min-max = ", round(min(perf_test_0.95$mean_r2), 3), "-", round(max(perf_test_0.95$mean_r2), 3)),
                                                            x_name = "",
                                                            y_name = "Biomass change (%)",
                                                            color_lab = "",
                                                            breaks = c(2024, 2050, 2075),
                                                            # y_scale = c(-42, 3),
                                                            model_facet = FALSE,
                                                            color_scale = color_scale)


sensitivity_performance <- patchwork::wrap_plots(ensemble_model_plot_fig1 / ensemble_model_0.5_plot / ensemble_model_0.75_plot / ensemble_model_0.95_plot)
ggsave(sensitivity_performance, filename = "figures/sensitivity_performance.png", width = 8.5, height = 10)
