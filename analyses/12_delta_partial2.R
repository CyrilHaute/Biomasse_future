source("R/future_pred_scale_function.R")
source("R/06_score_function.R")
source("R/delta_plot_function.R")

load("results/performance_bind.Rdata")
load("results/decile_save.Rdata")

decile_save[30,] <- c(0, 0)
decile_save <- decile_save[order(decile_save$biomass_class),]
scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")

color_scale <- palette.colors()
color_scale <- as.character(c(color_scale[4], color_scale[3], color_scale[5], color_scale[7]))

migration_ensemble <- readRDS("results/migration_ensemble8.rds")

species_to_remove <- c("Acanthurus achilles", "Acanthurus grammoptilus", "Acanthurus olivaceus", "Acanthurus pyroferus", 
                       "Amblyglyphidodon curacao", "Canthigaster rostrata", "Centropyge bispinosa", "Centropyge vrolikii", 
                       "Chaetodon kleinii", "Chaetodon ornatissimus", "Cheilinus chlorourus", "Cheilinus fasciatus",
                       "Chromis cyanea", "Epinephelus daemelii", "Fistularia commersonii", 
                       "Gnathodentex aureolineatus", "Goniistius vestitus", "Hologymnosus annulatus", 
                       "Hypoplectrus nigricans", "Lutjanus carponotatus", "Lutjanus kasmira", "Lutjanus lemniscatus", 
                       "Neoglyphidodon nigroris", "Neoglyphidodon polyacanthus", "Novaculichthys taeniourus", 
                       "Parupeneus spilurus", "Plectorhinchus flavomaculatus", "Plectorhinchus multivittatus", 
                       "Plectropomus maculatus", "Pomacentrus milleri", "Pomacentrus reidi", "Pseudanthias huchtii", 
                       "Pterocaesio pisang", "Pterocaesio tile", "Scarus oviceps", "Stegastes nigricans", 
                       "Thalassoma amblycephalum")
species_to_remove <- stringr::str_replace_all(species_to_remove, " ", "_")
list_species <- gsub(".Rdata", "", list.files("results/biomass_class_future_rf_hist_migration8_full", full.names = FALSE))

which_species_to_remove <- which(list_species %in% species_to_remove)

migration_ensemble <- migration_ensemble[-which_species_to_remove]

delta_species <- pbmcapply::pbmclapply(1:length(migration_ensemble), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = migration_ensemble[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE,
                                                  biais = TRUE,
                                                  mc.cores = 1) |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(migration_ensemble[[i]][[1]]$species_name))
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_bind <- do.call(rbind, delta_species) |> 
  dplyr::mutate(model = "full")

delta_plot <- delta_plot_function(delta_species_bind = delta_species_bind)
ggsave("figures/delta_plot_plot_hist_migration.png", delta_plot, width = 9, height = 5)

without_migration_ensemble <- readRDS("results/withoutmigration_ensemble8.rds")
without_migration_ensemble <- without_migration_ensemble[-which_species_to_remove]

delta_species_without <- pbmcapply::pbmclapply(1:length(without_migration_ensemble), function(i) {

  future_pred_unlog <- future_pred_scale_function(data = without_migration_ensemble[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE,
                                                  biais = FALSE) |>
    dplyr::select(delta, scenario) |>
    unique() |>
    dplyr::mutate(species_name = unique(without_migration_ensemble[[i]][[1]]$species_name))

}, mc.cores = parallel::detectCores() - 1)
delta_species_without_bind <- do.call(rbind, delta_species_without) |>
  dplyr::mutate(model = "full")

delta_plot_without <- delta_plot_function(delta_species_bind = delta_species_without_bind)
ggsave("figures/delta_plot_plot_hist_without_migration.png", delta_plot_without, width = 9, height = 5)


load("data/raw/1c_species_traits_final.Rdata")
species_traits_final <- species_traits_final |> 
  dplyr::ungroup() |> 
  dplyr::rename(species_name = rls_species_name) |> 
  dplyr::select(species_name, trophic_guild, Length, IUCN_inferred_Loiseau23, geographic_range_Albouy19, family, K)
species_traits_final <- species_traits_final[complete.cases(species_traits_final),]
species_traits_final[species_traits_final$trophic_guild == "Herbivores Microvores Detritivores",]$trophic_guild <- "herbivores"

delta_species_trait <- delta_species_bind |> 
  dplyr::inner_join(species_traits_final)

species_traits_final |> 
  dplyr::filter(species_name %in% unique(delta_species_trait$species_name)) |> 
  dplyr::group_by(IUCN_inferred_Loiseau23) |> 
  dplyr::summarise(n = dplyr::n())

sp_baseline_file <- list.files("results/biomass_baseline_future_hist8", full.names = TRUE)
sp_baseline_file_small <- list.files("results/biomass_baseline_future_hist8", full.names = FALSE)

sp_baseline <- pbmcapply::pbmclapply(1:length(sp_baseline_file), function(i) {
  
  load(sp_baseline_file[i])
  
  sp_name <- stringr::str_replace_all(sp_baseline_file_small[i], c("_" = " ", ".Rdata" = ""))
  
  sp_baseline <- sp |> 
    dplyr::filter(year == 2024) |> 
    dplyr::select(survey_id, x, y) |> 
    dplyr::mutate(species_name = sp_name)

  sp_future <- sp |> 
    dplyr::filter(year == 2025,
                  !survey_id %in% sp_baseline$survey_id) |> 
    dplyr::select(survey_id, x, y) |> 
    dplyr::mutate(species_name = sp_name) |> 
    unique()
  
  n_sp_future <- data.frame(species_name = sp_name,
                            n_future = length(unique(sp_future$survey_id)))
  
}, mc.cores = parallel::detectCores() - 1)
sp_baseline_bind <- do.call(rbind, sp_baseline)

delta_species_trait <- delta_species_trait |> 
  dplyr::inner_join(sp_baseline_bind)

summary(lm(n_future ~ geographic_range_Albouy19, data = delta_species_trait))
  

# delta_species_hum_trait <- delta_species_hum_bind |> 
#   dplyr::inner_join(species_traits_final)

# delta_species_trait[,colnames(delta_species_trait) %in% c("Length", "geographic_range_Albouy19", "K")] <- scale(delta_species_trait[,colnames(delta_species_trait) %in% c("Length", "geographic_range_Albouy19", "K")], center = TRUE, scale = TRUE)

delta_model <- ranger::ranger(x = delta_species_trait[!colnames(delta_species_trait) %in% c("delta", "species_name", "model", "IUCN_inferred_Loiseau23")],
                              y = unlist(delta_species_trait[colnames(delta_species_trait) %in% "delta"]),
                              num.trees = 1000,
                              importance = "impurity")

delta_model_sensitivity_analysis <- ranger::ranger(x = delta_species_trait[!colnames(delta_species_trait) %in% c("delta", "species_name", "model", "IUCN_inferred_Loiseau23", "n_future")],
                                                   y = unlist(delta_species_trait[colnames(delta_species_trait) %in% "delta"]),
                                                   num.trees = 1000,
                                                   importance = "impurity")

var_imp <- data.frame(delta = delta_model$variable.importance,
                      covariates = unique(names(c(delta_model$variable.importance))))
var_imp[var_imp$covariates == "K",]$covariates <- "K (Growth rate)"
var_imp[var_imp$covariates == "Length",]$covariates <- "Maximum body length"
var_imp[var_imp$covariates == "trophic_guild",]$covariates <- "Trophic groups"
var_imp[var_imp$covariates == "n_future",]$covariates <- "Range shift"
var_imp[var_imp$covariates == "geographic_range_Albouy19",]$covariates <- "Geographic range"
var_imp[var_imp$covariates == "family",]$covariates <- "Family"
var_imp[var_imp$covariates == "scenario",]$covariates <- "Scenario"

var_imp_sa <- data.frame(delta = delta_model_sensitivity_analysis$variable.importance,
                         covariates = unique(names(c(delta_model_sensitivity_analysis$variable.importance))))
var_imp_sa[var_imp_sa$covariates == "K",]$covariates <- "K (Growth rate)"
var_imp_sa[var_imp_sa$covariates == "Length",]$covariates <- "Maximum body length"
var_imp_sa[var_imp_sa$covariates == "trophic_guild",]$covariates <- "Trophic groups"
var_imp_sa[var_imp_sa$covariates == "geographic_range_Albouy19",]$covariates <- "Geographic range"
var_imp_sa[var_imp_sa$covariates == "family",]$covariates <- "Family"
var_imp_sa[var_imp_sa$covariates == "scenario",]$covariates <- "Scenario"

library(ggplot2)

var_imp <- var_imp |> 
  dplyr::mutate(delta_pr = (delta * 100) / sum(delta))

delta_imp_var <- ggplot(var_imp) +
  geom_col(aes(x = reorder(covariates, delta_pr), y = delta_pr, fill = delta_pr)) +
  scale_fill_viridis_c(direction = -1) +
  theme_bw() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.position = "none") + 
  labs(x = "",
       y = "Variable importance (%)",
       title = paste0("A.", "  Biomass change R² = ", round(delta_model$r.squared, 2)))


var_imp_sa <- var_imp_sa |> 
  dplyr::mutate(delta_pr = (delta * 100) / sum(delta))

delta_imp_var_sa <- ggplot(var_imp_sa) +
  geom_col(aes(x = reorder(covariates, delta_pr), y = delta_pr, fill = delta_pr)) +
  scale_fill_viridis_c(direction = -1) +
  theme_bw() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.position = "none") + 
  labs(x = "",
       y = "Variable importance (%)",
       title = paste0("  Biomass change R² = ", round(delta_model_sensitivity_analysis$r.squared, 2)))


scenario <- unique(delta_species_trait$scenario)

partial_delta_range <- lapply(1:length(scenario), function(i) {
  
  ssp <- delta_species_trait[delta_species_trait$scenario == scenario[i],]
  
  pdp::partial(delta_model,
               train = ssp,
               pred.var = c("geographic_range_Albouy19")) |>
    dplyr::rename(delta = yhat) |>
    dplyr::mutate(scenario = scenario[i])
  
})
partial_delta_range_bind <- do.call(rbind, partial_delta_range)

partial_delta_length <- lapply(1:length(scenario), function(i) {
  
  ssp <- delta_species_trait[delta_species_trait$scenario == scenario[i],]
  
  pdp::partial(delta_model,
               train = ssp,
               pred.var = c("Length")) |>
    dplyr::rename(delta = yhat) |>
    dplyr::mutate(scenario = scenario[i])
  
})
partial_delta_length_bind <- do.call(rbind, partial_delta_length)

partial_delta_n <- lapply(1:length(scenario), function(i) {
  
  ssp <- delta_species_trait[delta_species_trait$scenario == scenario[i],]
  
  pdp::partial(delta_model,
               train = ssp,
               pred.var = c("n_future")) |>
    dplyr::rename(delta = yhat) |>
    dplyr::mutate(scenario = scenario[i])
  
})
partial_delta_n_bind <- do.call(rbind, partial_delta_n)

partial_range_plot <- partial_delta_range_bind |>
  ggplot() +
  geom_point(aes(x = geographic_range_Albouy19, y = delta, color = scenario), size = 1) + 
  geom_line(aes(x = geographic_range_Albouy19, y = delta, color = scenario), size = 1.2, stat = "smooth", method = "gam") +
  labs(y = "", x = "Geographic range (km²)", title = "", color = "") +
  theme_bw() +
  scale_color_manual(values = color_scale) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))

partial_length_plot <- partial_delta_length_bind |>
  ggplot() +
  geom_point(aes(x = Length, y = delta, color = scenario), size = 1) + 
  geom_line(aes(x = Length, y = delta, color = scenario), size = 1.2, stat = "smooth", method = "gam") +
  labs(y = "", x = "Maximum body length", title = "", color = "") +
  theme_bw() +
  scale_color_manual(values = color_scale) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))

partial_n_plot <- partial_delta_n_bind |>
  ggplot() +
  geom_point(aes(x = n_future, y = delta, color = scenario), size = 1) + 
  geom_line(aes(x = n_future, y = delta, color = scenario), size = 1.2, stat = "smooth", method = "gam") +
  labs(y = "", x = "Range shift (number of new sites)", title = "B. Biomass change (%)", color = "") +
  theme_bw() +
  scale_color_manual(values = color_scale) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))

partial_plot <- patchwork::wrap_plots(partial_n_plot + partial_range_plot + partial_length_plot) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12))

delta_imp_partial <- patchwork::wrap_plots(delta_imp_var / partial_plot)

ggsave("figures/delta_imp_partial_plot_hist_migration8.png", delta_imp_partial, width = 13, height = 12)
ggsave("figures/delta_imp_sensitivity_analysis.png", delta_imp_var_sa, width = 10, height = 6)











delta_species_trait2 <- delta_species_bind |> 
  dplyr::inner_join(species_traits_final) |> 
  dplyr::filter(scenario %in% c("SSP1-2.6", "SSP5-8.5"))

delta_species_trait2 <- delta_species_trait2 |> 
  dplyr::mutate(trophic_guild2 = paste0(trophic_guild, "_", scenario))

c("herbivores_SSP1-2.6",
  "macroinvertivore_SSP1-2.6",
  "corallivore_SSP1-2.6",
  "planktivore_SSP1-2.6",
  "sessile invertivores_SSP1-2.6",
  "microinvertivore_SSP1-2.6",
  "crustacivore_SSP1-2.6",
  "piscivore_SSP1-2.6",
  " ",
  "herbivores_SSP5-8.5",
  "macroinvertivore_SSP5-8.5",
  "corallivore_SSP5-8.5",
  "planktivore_SSP5-8.5",
  "piscivore_SSP5-8.5",
  "sessile invertivores_SSP5-8.5",
  "microinvertivore_SSP5-8.5",
  "crustacivore_SSP5-8.5")

delta_text <- data.frame(label = c("SSP1-2.6", "SSP5-8.5"),
                         x = c(4.5, 13.5),
                         y = c(160, 160))

delta_species_trait2$trophic_guild2 <- factor(delta_species_trait2$trophic_guild2, levels = c("herbivores_SSP1-2.6",
                                                                                              "macroinvertivore_SSP1-2.6",
                                                                                              "corallivore_SSP1-2.6",
                                                                                              "planktivore_SSP1-2.6",
                                                                                              "sessile invertivores_SSP1-2.6",
                                                                                              "microinvertivore_SSP1-2.6",
                                                                                              "crustacivore_SSP1-2.6",
                                                                                              "piscivore_SSP1-2.6",
                                                                                              " ",
                                                                                              "herbivores_SSP5-8.5",
                                                                                              "macroinvertivore_SSP5-8.5",
                                                                                              "corallivore_SSP5-8.5",
                                                                                              "planktivore_SSP5-8.5",
                                                                                              "piscivore_SSP5-8.5",
                                                                                              "sessile invertivores_SSP5-8.5",
                                                                                              "microinvertivore_SSP5-8.5",
                                                                                              "crustacivore_SSP5-8.5"))

delta_species_plot <- delta_species_trait2 |> 
  ggplot() + 
  geom_point(aes(x = trophic_guild2, y = delta), size = 0.6, position = "jitter") +
  geom_boxplot(aes(x = trophic_guild2, y = delta, fill = trophic_guild), alpha = 0.8, size = 0.4, outlier.shape = NA) +
  geom_text(data = delta_text, aes(x = x, y = y, label = label), size = 5) + 
  theme_bw() + 
  scale_x_discrete(drop = FALSE) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 8, name = "Dark2")) +
  labs(x = "", y = "Delta Biomass (%)", fill = "") +
  theme(axis.text.x = element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 10),
        strip.text.y = element_text(size = 10))

# Test ANOVA
anova_data <- delta_species_trait2 |> 
  dplyr::mutate(scenario = stringr::str_replace_all(scenario, "-", ""))
anova_result <- aov(delta ~ interaction(trophic_guild, scenario), data = anova_data)

# Test post-hoc Tukey
tukey_result <- TukeyHSD(anova_result)
tukey_table <- as.data.frame(tukey_result$`interaction(trophic_guild, scenario)`)

diff <- tukey_table$`p adj` < 0.05
names(diff) <- rownames(tukey_table)

# Convert to letters
significance_letters <- multcompView::multcompLetters(diff,
                                                      Letters = letters)$Letters

# Choose y axis value to plot the letters
y <- anova_data |>
  dplyr::group_by(scenario, trophic_guild) |>
  dplyr::summarise(y = 110) |>
  dplyr::ungroup() |> 
  dplyr::mutate(interaction = paste0(trophic_guild, ".", scenario))

label_data <- data.frame(
  interaction = names(significance_letters),
  Letter = significance_letters
) |>
  dplyr::inner_join(y)

label_data <- label_data |> 
  dplyr::arrange(scenario) |> 
  dplyr::mutate(x = stringr::str_replace(interaction, ".SSP", "_SSP"),
                x = stringr::str_replace_all(x, "SSP1", "SSP1-"),
                x = stringr::str_replace_all(x, "SSP5", "SSP5-"))

delta_species_plot_text <- delta_species_plot + 
  geom_text(data = label_data,
            aes(x = x, y = y, label = Letter),
            inherit.aes = FALSE,
            size = 4)

ggsave("figures/delta_trait_plot_migration.png", delta_species_plot_text, width = 11, height = 6)


delta_species_bind |> 
  dplyr::inner_join(species_traits_final) |> 
  dplyr::filter(scenario %in% c("SSP1-2.6")) |> 
  ggplot() + 
  geom_point(aes(x = forcats::fct_reorder(trophic_guild, delta), y = delta), size = 0.6, position = "jitter") +
  geom_boxplot(aes(x = forcats::fct_reorder(trophic_guild, delta), y = delta, fill = trophic_guild), alpha = 0.8, size = 0.4, outlier.shape = NA) +
  theme_bw() + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 8, name = "Dark2")) +
  labs(x = "", y = "Delta Biomass (%)", fill = "") +
  theme(axis.text.x = element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 10),
        strip.text.y = element_text(size = 10))

test <- aov(delta ~ interaction(trophic_guild, scenario), data = delta_species_bind |> 
              dplyr::inner_join(species_traits_final) |> 
              dplyr::filter(scenario %in% c("SSP5-8.5")))

summary(test)

test_test <- TukeyHSD(test)

test_test_table <- as.data.frame(test_test$`interaction(trophic_guild, scenario)`)

diff <- test_test_table$`p adj` < 0.05
