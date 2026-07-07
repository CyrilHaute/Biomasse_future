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

species_to_remove <- c("Acanthurus achilles", "Acanthurus grammoptilus", "Acanthurus olivaceus", "Acanthurus pyroferus", 
                       "Amblyglyphidodon curacao",  "Canthigaster rostrata", "Centropyge bispinosa", "Centropyge vrolikii",
                       "Chaetodon kleinii", "Chaetodon ornatissimus", "Cheilinus chlorourus", "Cheilinus fasciatus", 
                       "Chromis cyanea", "Epinephelus daemelii", "Fistularia commersonii", "Gnathodentex aureolineatus", "Goniistius vestitus",
                       "Hologymnosus annulatus", "Hypoplectrus nigricans", "Lutjanus carponotatus", "Lutjanus kasmira", "Lutjanus lemniscatus", 
                       "Neoglyphidodon nigroris", "Neoglyphidodon polyacanthus", "Novaculichthys taeniourus", "Parupeneus spilurus", 
                       "Plectorhinchus flavomaculatus", "Plectorhinchus multivittatus", "Plectropomus maculatus", "Pomacentrus milleri", 
                       "Pomacentrus reidi", "Pseudanthias huchtii", "Pterocaesio pisang", "Pterocaesio tile", "Pempheris schwenkii", 
                       "Scarus oviceps", "Stegastes nigricans", "Thalassoma amblycephalum")
species_to_remove <- stringr::str_replace_all(species_to_remove, " ", "_")
list_species <- gsub(".Rdata", "", list.files("results/biomass_class_future_rf_hist_migration8_full", full.names = FALSE))

which_species_to_remove <- which(list_species %in% species_to_remove)


### Load models projection

biomass_class_future_rf_hist_migration8_full <- list.files("results/biomass_class_future_rf_hist_migration8_full", full.names = TRUE)
biomass_class_future_rf_hist_migration8_full <- lapply(1:length(biomass_class_future_rf_hist_migration8_full), function(i) {
  
  load(biomass_class_future_rf_hist_migration8_full[i])
  assign(as.character(i), all_pred)
  
})
biomass_class_future_rf_hist_withoutmigration8_full <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_full", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_full <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_full), function(i) {
  
  load(biomass_class_future_rf_hist_withoutmigration8_full[i])
  assign(as.character(i), all_pred)
  
})


biomass_class_future_xgboost_hist_migration9_full <- list.files("results/biomass_class_future_xgboost_hist_migration9_full", full.names = TRUE)
biomass_class_future_xgboost_hist_migration9_full <- lapply(1:length(biomass_class_future_xgboost_hist_migration9_full), function(i) {
  
  load(biomass_class_future_xgboost_hist_migration9_full[i])
  assign(as.character(i), all_pred)
  
})
biomass_class_future_xgboost_hist_withoutmigration9_full <- list.files("results/biomass_class_future_xgboost_hist_withoutmigration9_full", full.names = TRUE)
biomass_class_future_xgboost_hist_withoutmigration9_full <- lapply(1:length(biomass_class_future_xgboost_hist_withoutmigration9_full), function(i) {
  
  load(biomass_class_future_xgboost_hist_withoutmigration9_full[i])
  assign(as.character(i), all_pred)
  
})


biomass_class_future_dnn_hist_migration9_full <- list.files("results/biomass_class_future_dnn_hist_migration9_full", full.names = TRUE)
biomass_class_future_dnn_hist_migration9_full <- lapply(1:length(biomass_class_future_dnn_hist_migration9_full), function(i) {
  
  load(biomass_class_future_dnn_hist_migration9_full[i])
  assign(as.character(i), all_pred)
  
})
biomass_class_future_dnn_hist_withoutmigration9_full <- list.files("results/biomass_class_future_dnn_hist_withoutmigration9_full", full.names = TRUE)
biomass_class_future_dnn_hist_withoutmigration9_full <- lapply(1:length(biomass_class_future_dnn_hist_withoutmigration9_full), function(i) {
  
  load(biomass_class_future_dnn_hist_withoutmigration9_full[i])
  assign(as.character(i), all_pred)
  
})


### Create ensemble model

migration_ensemble <- score_function(pred_biomass_data = list(biomass_class_future_rf_hist_migration8_full,
                                                              biomass_class_future_xgboost_hist_migration9_full,
                                                              biomass_class_future_dnn_hist_migration9_full),
                                     score_data = performance_bind)
without_migration_ensemble <- score_function(pred_biomass_data = list(biomass_class_future_rf_hist_withoutmigration8_full,
                                                                      biomass_class_future_xgboost_hist_withoutmigration9_full,
                                                                      biomass_class_future_dnn_hist_withoutmigration9_full),
                                             score_data = performance_bind)
# saveRDS(migration_ensemble, "results/migration_ensemble8.rds")
# saveRDS(without_migration_ensemble, "results/withoutmigration_ensemble8.rds")


migration_ensemble <- readRDS("results/migration_ensemble8.rds")
without_migration_ensemble <- readRDS("results/withoutmigration_ensemble8.rds")

#### Plot figure 1

without_migration_ensemble_plot <- plot_future_pred_scale_function(data = future_pred_scale_function(data = without_migration_ensemble[-which_species_to_remove],
                                                                                                     unit = "kg",
                                                                                                     time = as.character(c("2024":"2075")),
                                                                                                     score = TRUE,
                                                                                                     biais = FALSE) |> 
                                                                     dplyr::group_by(scenario) |> 
                                                                     dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                                                   upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                                                   biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                                                                     dplyr::ungroup() |> 
                                                                     dplyr::mutate(model = "Without range shift"),
                                                                   title = "A.",
                                                                   x_name = "",
                                                                   y_name = "Biomass change (%)",
                                                                   color_lab = "",
                                                                   breaks = c(2024, 2050, 2075),
                                                                   model_facet = TRUE,
                                                                   color_scale = color_scale) +
  theme(legend.position = c(0.7, 0.8),
        legend.direction = "vertical") +
  scale_y_continuous(limits = c(-46, 5))
migration_ensemble_plot <- plot_future_pred_scale_function(data = future_pred_scale_function(data = migration_ensemble[-which_species_to_remove],
                                                                                             unit = "kg",
                                                                                             time = as.character(c("2024":"2075")),
                                                                                             score = TRUE,
                                                                                             biais = TRUE,
                                                                                             mc.cores = parallel::detectCores() - 1) |> 
                                                             dplyr::group_by(scenario) |> 
                                                             dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                                           upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                                           biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                                                             dplyr::ungroup() |> 
                                                             dplyr::mutate(model = "With range shift"),
                                                           title = "",
                                                           x_name = "",
                                                           y_name = "Biomass change (%)",
                                                           color_lab = "",
                                                           breaks = c(2024, 2050, 2075),
                                                           model_facet = TRUE,
                                                           color_scale = color_scale) +
  theme(legend.position = c(0.7, 0.8),
        legend.direction = "vertical") +
  scale_y_continuous(limits = c(-46, 5))

with_vs_without <- patchwork::wrap_plots(without_migration_ensemble_plot + migration_ensemble_plot)
ggsave(with_vs_without, file = "figures/with_vs_without_pred8.png", width = 10, height = 5)



#### Plot figure SX

rf_plot <- plot_future_pred_scale_function(data = future_pred_scale_function(data = biomass_class_future_rf_hist_migration8_full[-which_species_to_remove],
                                                                             unit = "kg",
                                                                             time = as.character(c("2024":"2075")),
                                                                             biais = TRUE) |> 
                                             dplyr::group_by(scenario) |> 
                                             dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                           upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                           biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                                             dplyr::ungroup(),
                                           title = "With migration",
                                           x_name = "",
                                           y_name = "Biomass change (%)",
                                           color_lab = "",
                                           breaks = c(2024, 2050, 2075),
                                           color_scale = color_scale,
                                           delta = FALSE,
                                           y_scale = c(-70, 15))
rf_plot_wm <- plot_future_pred_scale_function(data = future_pred_scale_function(data = biomass_class_future_rf_hist_withoutmigration8_full[-which_species_to_remove],
                                                                                unit = "kg",
                                                                                time = as.character(c("2024":"2075")),
                                                                                biais = FALSE) |> 
                                                dplyr::group_by(scenario) |> 
                                                dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                              upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                              biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                                                dplyr::ungroup(),
                                              title = "Without migration",
                                              x_name = "",
                                              y_name = "Biomass change (%)",
                                              color_lab = "",
                                              breaks = c(2024, 2050, 2075),
                                              color_scale = color_scale,
                                              delta = FALSE,
                                              y_scale = c(-70, 15))
dnn_plot <- plot_future_pred_scale_function(data = future_pred_scale_function(data = biomass_class_future_dnn_hist_migration9_full[-which_species_to_remove],
                                                                              unit = "kg",
                                                                              time = as.character(c("2024":"2075")),
                                                                              biais = TRUE) |> 
                                              dplyr::group_by(scenario) |> 
                                              dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                            upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                            biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                                              dplyr::ungroup(),
                                            title = "",
                                            x_name = "",
                                            y_name = "Biomass change (%)",
                                            color_lab = "",
                                            breaks = c(2024, 2050, 2075),
                                            color_scale = color_scale,
                                            delta = FALSE,
                                            y_scale = c(-70, 15))
dnn_plot_wm <- plot_future_pred_scale_function(data = future_pred_scale_function(data = biomass_class_future_dnn_hist_withoutmigration9_full[-which_species_to_remove],
                                                                                 unit = "kg",
                                                                                 time = as.character(c("2024":"2075")),
                                                                                 biais = FALSE) |> 
                                                 dplyr::group_by(scenario) |> 
                                                 dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                               upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                               biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                                                 dplyr::ungroup(),
                                               title = "",
                                               x_name = "",
                                               y_name = "Biomass change (%)",
                                               color_lab = "",
                                               breaks = c(2024, 2050, 2075),
                                               color_scale = color_scale,
                                               delta = FALSE,
                                               y_scale = c(-70, 15))
xgboost_plot <- plot_future_pred_scale_function(data = future_pred_scale_function(data = biomass_class_future_xgboost_hist_migration9_full[-which_species_to_remove],
                                                                                  unit = "kg",
                                                                                  time = as.character(c("2024":"2075")),
                                                                                  biais = TRUE) |> 
                                                  dplyr::group_by(scenario) |> 
                                                  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                                                  dplyr::ungroup(),
                                                title = "",
                                                x_name = "",
                                                y_name = "Biomass change (%)",
                                                color_lab = "",
                                                breaks = c(2024, 2050, 2075),
                                                color_scale = color_scale,
                                                delta = FALSE,
                                                y_scale = c(-70, 15))
xgboost_plot_wm <- plot_future_pred_scale_function(data = future_pred_scale_function(data = biomass_class_future_xgboost_hist_withoutmigration9_full[-which_species_to_remove],
                                                                                     unit = "kg",
                                                                                     time = as.character(c("2024":"2075")),
                                                                                     biais = FALSE) |> 
                                                     dplyr::group_by(scenario) |> 
                                                     dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                                   upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                                   biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                                                     dplyr::ungroup(),
                                                   title = "",
                                                   x_name = "",
                                                   y_name = "Biomass change (%)",
                                                   color_lab = "",
                                                   breaks = c(2024, 2050, 2075),
                                                   color_scale = color_scale,
                                                   delta = FALSE,
                                                   y_scale = c(-70, 15))


without_migration_ensemble_plot_all <- plot_future_pred_scale_function(data = future_pred_scale_function(data = without_migration_ensemble[-which_species_to_remove],
                                                                                                         unit = "kg",
                                                                                                         time = as.character(c("2024":"2075")),
                                                                                                         score = TRUE,
                                                                                                         biais = FALSE) |> 
                                                                         dplyr::group_by(scenario) |> 
                                                                         dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                                                       upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                                                       biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                                                                         dplyr::ungroup() |> 
                                                                         dplyr::mutate(model = "Ensemble model"),
                                                                       title = "",
                                                                       x_name = "",
                                                                       y_name = "Biomass change (%)",
                                                                       color_lab = "",
                                                                       breaks = c(2024, 2050, 2075),
                                                                       y_scale = c(-70, 15),
                                                                       model_facet = TRUE,
                                                                       delta = FALSE,
                                                                       color_scale = color_scale)
migration_ensemble_plot_all <- plot_future_pred_scale_function(data = future_pred_scale_function(data = migration_ensemble[-which_species_to_remove],
                                                                                                 unit = "kg",
                                                                                                 time = as.character(c("2024":"2075")),
                                                                                                 score = TRUE,
                                                                                                 biais = TRUE,
                                                                                                 mc.cores = parallel::detectCores() - 1) |> 
                                                                 dplyr::group_by(scenario) |> 
                                                                 dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                                               upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                                               biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                                                                 dplyr::ungroup() |> 
                                                                 dplyr::mutate(model = "Ensemble model"),
                                                               title = "",
                                                               x_name = "",
                                                               y_name = "Biomass change (%)",
                                                               color_lab = "",
                                                               breaks = c(2024, 2050, 2075),
                                                               y_scale = c(-70, 15),
                                                               model_facet = TRUE,
                                                               delta = FALSE,
                                                               color_scale = color_scale)

without_migration <- patchwork::wrap_plots(rf_plot_wm / xgboost_plot_wm / dnn_plot_wm / without_migration_ensemble_plot_all)
with_migration <- patchwork::wrap_plots(rf_plot / xgboost_plot / dnn_plot / migration_ensemble_plot_all)

with_vs_without <- patchwork::wrap_plots(without_migration + with_migration) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "horizontal")
ggsave(with_vs_without, file = "figures/with_vs_without_pred8_all.png", width = 10, height = 12)


############################################## Evolution of biomass at global scale for one species ##############################################
migration_ensemble <- readRDS("results/migration_ensemble8.rds")

new_dir <- "figures/species_specific_migration8/"
dir.create(new_dir)
unlist(pbmcapply::pbmclapply(1:length(migration_ensemble), function(i) {
  
  future_pred_unlog <- plot_future_pred_scale_function(data = future_pred_scale_function(data = migration_ensemble[i],
                                                                                         unit = "kg",
                                                                                         time = as.character(c("2024":"2100")),
                                                                                         score = TRUE,
                                                                                         biais = TRUE,
                                                                                         mc.cores = 1) |>
                                                         dplyr::group_by(scenario) |>
                                                         dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                                       upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                                       biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |>
                                                         dplyr::ungroup(),
                                                       group_name = unique(migration_ensemble[[i]][[1]]$species_name),
                                                       title = unique(migration_ensemble[[i]][[1]]$species_name),
                                                       x_name = "",
                                                       y_name = "Biomass (kg)",
                                                       color_lab = "",
                                                       model_facet = FALSE,
                                                       face = "italic",
                                                       breaks = c(2024, 2050, 2100),
                                                       color_scale = color_scale)
  
  ggsave(future_pred_unlog, filename = paste0(new_dir, unique(migration_ensemble[[i]][[1]]$species_name), ".png"))
  
}, mc.cores = parallel::detectCores() - 1))



without_migration_ensemble <- readRDS("results/withoutmigration_ensemble8.rds")

new_dir <- "figures/species_specific_without_migration8/"
dir.create(new_dir)
unlist(pbmcapply::pbmclapply(1:length(without_migration_ensemble), function(i) {

  future_pred_unlog <- plot_future_pred_scale_function(data = future_pred_scale_function(data = without_migration_ensemble[i],
                                                                                         unit = "kg",
                                                                                         time = as.character(c("2024":"2100")),
                                                                                         score = TRUE,
                                                                                         biais = FALSE) |>
                                                         dplyr::group_by(scenario) |>
                                                         dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                                                       upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                                                       biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |>
                                                         dplyr::ungroup(),
                                                       group_name = unique(without_migration_ensemble[[i]][[1]]$species_name),
                                                       title = unique(without_migration_ensemble[[i]][[1]]$species_name),
                                                       x_name = "",
                                                       y_name = "Biomass (kg)",
                                                       color_lab = "",
                                                       model_facet = FALSE,
                                                       face = "italic",
                                                       breaks = c(2024, 2050, 2100),
                                                       color_scale = color_scale)

  ggsave(future_pred_unlog, filename = paste0(new_dir, unique(without_migration_ensemble[[i]][[1]]$species_name), ".png"))

}, mc.cores = parallel::detectCores() - 1))


#### Extract projection for Cheilinus undulatus, Anampses twistii and Abudefduf vaigiensis

sapply(1:length(migration_ensemble), function(i) {  unique(migration_ensemble[[i]][[1]]$species_name) })

saved_species <- c(162, 49, 6)

plot_species <- pbmcapply::pbmclapply(1:length(saved_species), function(i) {
  
  future_pred_unlog <- plot_future_pred_scale_function(data = future_pred_scale_function(data = migration_ensemble[saved_species[i]],
                                                                                         unit = "kg",
                                                                                         time = as.character(c("2024":"2075")),
                                                                                         score = TRUE,
                                                                                         biais = TRUE,
                                                                                         mc.cores = parallel::detectCores() - 1) |> 
                                                         dplyr::group_by(scenario) |> 
                                                         dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * -100,
                                                                       upper = (1 - (upper / dplyr::first(biomass))) * -100,
                                                                       biomass = (1 - (biomass / dplyr::first(biomass))) * -100) |> 
                                                         dplyr::ungroup(),
                                                       ncol = 3,
                                                       group_name = unique(migration_ensemble[[saved_species[i]]][[1]]$species_name),
                                                       title = unique(migration_ensemble[[saved_species[i]]][[1]]$species_name),
                                                       x_name = "",
                                                       y_name = "Biomass change (%)",
                                                       color_lab = "",
                                                       breaks = c(2024, 2050, 2075),
                                                       face = "italic",
                                                       model_facet = FALSE,
                                                       color_scale = color_scale)

}, mc.cores = 1)

c_un <- magick::image_read("data/raw/image_fish/cheilinus_undulatus.jpg")
a_tw <- magick::image_read("data/raw/image_fish/anampses_twistii.jpg")
a_va <- magick::image_read("data/raw/image_fish/abudefduf_vaigiensis.jpg")

library(ggplot2)

c_un_plot <- ggplot() + annotation_custom(grid::rasterGrob(c_un), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + theme_void()
a_tw_plot <- ggplot() + annotation_custom(grid::rasterGrob(a_tw), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + theme_void()
a_va_plot <- ggplot() + annotation_custom(grid::rasterGrob(a_va), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + theme_void()

plot_species_patch <- patchwork::wrap_plots(((plot_species[[1]] + c_un_plot) / 
                                               (plot_species[[2]] + a_tw_plot) /
                                               (plot_species[[3]] + a_va_plot)))

ggsave(plot_species_patch, filename = "figures/plot_species_patch_migration8.png", width = 7, height = 10)



delta_realm_migration <- future_pred_scale_function(data = migration_ensemble[-c(375, 380)],
                                                    unit = "kg",
                                                    time = as.character(c("2024":"2075")),
                                                    scale = "realm",
                                                    to_remove = 0,
                                                    score = TRUE)
delta_realm_s_migration <- delta_realm_migration |> 
  dplyr::ungroup() |> 
  dplyr::select(scale, delta, scenario) |> 
  unique() |> 
  dplyr::group_split(scenario)

delta_realm_without_migration <- future_pred_scale_function(data = without_migration_ensemble[-c(375, 380)],
                                                            unit = "kg",
                                                            time = as.character(c("2024":"2075")),
                                                            scale = "realm",
                                                            to_remove = 0,
                                                            score = TRUE)
delta_realm_s_without_migration <- delta_realm_without_migration |> 
  dplyr::ungroup() |> 
  dplyr::select(scale, delta, scenario) |> 
  unique() |> 
  dplyr::group_split(scenario)



load("results/ensemble_model_hist_full.Rdata")
load("results/ensemble_model_env_hist.Rdata")
load("results/ensemble_model_hum_hist.Rdata")



model_full <- future_pred_scale_function(data = ensemble_model_hist,
                                         unit = "kg",
                                         time = as.character(c("2024":"2075")),
                                         score = TRUE)
model_full$model_scenario <- "full"
model_env <- future_pred_scale_function(data = ensemble_model_env_hist,
                                        unit = "kg",
                                        time = as.character(c("2024":"2075")),
                                        score = TRUE)
model_env$model_scenario <- "env"
model_hum <- future_pred_scale_function(data = ensemble_model_hum_hist,
                                        unit = "kg",
                                        time = as.character(c("2024":"2075")),
                                        score = TRUE)
model_hum$model_scenario <- "hum"

model_total <- model_full |> 
  dplyr::group_by(scenario, model_scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup() |> 
  dplyr::full_join(model_env |> 
                     dplyr::group_by(scenario, model_scenario) |> 
                     dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                   upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                   biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                     dplyr::ungroup()) |> 
  dplyr::full_join(model_hum |> 
                     dplyr::group_by(scenario, model_scenario) |> 
                     dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                                   upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                                   biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
                     dplyr::ungroup())

model_plot_scenario <- plot_future_pred_scale_function(data = model_total,
                                                       title = "",
                                                       x_name = "",
                                                       y_name = "Biomass change (%)",
                                                       color_lab = "Model type",
                                                       breaks = c(2024, 2050, 2075),
                                                       model_facet = FALSE,
                                                       model_scenario = TRUE,
                                                       color_scale = c(RColorBrewer::brewer.pal(n = 9, name = "Set1")[3], PNWColors::pnw_palette("Bay", 2, type = "discrete")))
ggsave("figures/model_plot_scenario.png", model_plot_scenario, width = 11, height = 7)


delta_species_full <- pbmcapply::pbmclapply(1:length(ensemble_model_hist), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = ensemble_model_hist[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE) |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(ensemble_model_hist[[i]][[1]]$species_name))
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_full_bind <- do.call(rbind, delta_species_full) |> 
  dplyr::mutate(model = "full")


delta_species_env <- pbmcapply::pbmclapply(1:length(ensemble_model_env_hist), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = ensemble_model_env_hist[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE) |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(ensemble_model_env_hist[[i]][[1]]$species_name))
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_env_bind <- do.call(rbind, delta_species_env) |> 
  dplyr::mutate(model = "env")

delta_species_hum <- pbmcapply::pbmclapply(1:length(ensemble_model_hum_hist), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = ensemble_model_hum_hist[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE) |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(ensemble_model_hum_hist[[i]][[1]]$species_name))
  
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


