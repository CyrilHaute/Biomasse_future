source("R/future_pred_scale_function.R")
source("R/06_score_function.R")

load("results/decile_save.Rdata")
load("results/performance_bind.Rdata")

performance_bind <- performance_bind_save

decile_save[30,] <- c(0, 0)
decile_save <- decile_save[order(decile_save$biomass_class),]

scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")

color_scale <- palette.colors()
color_scale <- as.character(c(color_scale[4], color_scale[3], color_scale[5], color_scale[7]))

# min.node.size = 1
biomass_class_future_rf_hist_withoutmigration8_mns_1_full <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_mns_1_full", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_mns_1_full <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_mns_1_full), function(i) {
  
  load(biomass_class_future_rf_hist_withoutmigration8_mns_1_full[i])
  assign(as.character(i), all_pred)
  
})
# min.node.size = 5
biomass_class_future_rf_hist_withoutmigration8_mns_5_full <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_mns_5_full", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_mns_5_full <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_mns_5_full), function(i) {
  
  load(biomass_class_future_rf_hist_withoutmigration8_mns_5_full[i])
  assign(as.character(i), all_pred)
  
})
# min.node.size = 25
biomass_class_future_rf_hist_withoutmigration8_mns_25_full <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_mns_25_full", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_mns_25_full <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_mns_25_full), function(i) {
  
  load(biomass_class_future_rf_hist_withoutmigration8_mns_25_full[i])
  assign(as.character(i), all_pred)
  
})
# min.node.size = 50
biomass_class_future_rf_hist_withoutmigration8_mns_50_full <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_full", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_mns_50_full <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_mns_50_full), function(i) {
  
  load(biomass_class_future_rf_hist_withoutmigration8_mns_50_full[i])
  assign(as.character(i), all_pred)
  
})


rf_plot_wm_1 <- future_pred_scale_function(data = biomass_class_future_rf_hist_withoutmigration8_mns_1_full,
                                           unit = "kg",
                                           time = as.character(c("2024":"2075")),
                                           biais = FALSE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(model = "min.node.size = 1")

rf_plot_wm_5 <- future_pred_scale_function(data = biomass_class_future_rf_hist_withoutmigration8_mns_5_full,
                                           unit = "kg",
                                           time = as.character(c("2024":"2075")),
                                           biais = FALSE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(model = "min.node.size = 5")

rf_plot_wm_25 <- future_pred_scale_function(data = biomass_class_future_rf_hist_withoutmigration8_mns_25_full,
                                            unit = "kg",
                                            time = as.character(c("2024":"2075")),
                                            biais = FALSE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(model = "min.node.size = 25")

rf_plot_wm_50 <- future_pred_scale_function(data = biomass_class_future_rf_hist_withoutmigration8_mns_50_full,
                                            unit = "kg",
                                            time = as.character(c("2024":"2075")),
                                            biais = FALSE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(model = "min.node.size = 50")

rf_plot_mw_mns <- rf_plot_wm_1 |> 
  dplyr::full_join(rf_plot_wm_5) |> 
  dplyr::full_join(rf_plot_wm_25) |> 
  dplyr::full_join(rf_plot_wm_50)

library(ggplot2)

rf_plot_mw_mns_plot <- rf_plot_mw_mns |> 
  dplyr::mutate(model = forcats::fct_relevel(model, "min.node.size = 1", "min.node.size = 5", "min.node.size = 25", "min.node.size = 50")) |> 
  ggplot(aes(x = year, y = biomass, group = scenario, color = scenario)) + 
  labs(x = "", y = "Biomass change (%)", title = "", color = "") +
  theme_bw() + 
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 15, face = "plain"),
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10, face = "plain"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = c(2024, 2050, 2075)) +
  scale_y_continuous(limits = c(-73, 5)) + 
  geom_line(size = 1) +
  facet_wrap(~ model, nrow = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, size = 0.1) +
  scale_discrete_manual(aesthetics = "color", values = color_scale)

ggsave("figures/rf_plot_mw_mns_plot.png", rf_plot_mw_mns_plot, width = 9, height = 5)
