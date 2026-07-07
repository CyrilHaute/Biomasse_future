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


load("data/raw/1c_species_traits_final.Rdata")
species_traits_final <- species_traits_final |> 
  dplyr::ungroup() |> 
  dplyr::rename(species_name = rls_species_name) |> 
  dplyr::select(species_name, trophic_guild, Length, IUCN_category, geographic_range_Albouy19, family, K)
species_traits_final <- species_traits_final[complete.cases(species_traits_final),]
species_traits_final[species_traits_final$trophic_guild == "Herbivores Microvores Detritivores",]$trophic_guild <- "herbivores"

delta_species_trait <- delta_species_bind |> 
  dplyr::inner_join(species_traits_final)

length(unique(delta_species_trait$species_name))

baseline_uicn <- species_traits_final |>
  dplyr::filter(species_name %in% unique(delta_species_bind$species_name)
                # ,
                # IUCN_category %in% c("LC", "NT")
                # IUCN_category != c("DD")
  ) |>
  dplyr::rename(IUCN = IUCN_category) |>
  dplyr::group_by(IUCN) |>
  dplyr::summarise(freq = dplyr::n())
cr <- c("CR", 0)
baseline_uicn <- rbind(baseline_uicn, cr)

baseline_uicn$freq <- as.integer(baseline_uicn$freq)

baseline_uicn <- baseline_uicn |>
  dplyr::mutate(scenario = "Baseline",
                stratum = round((freq * 100) / sum(freq)))

delta_species_trait$scenario <- as.character(delta_species_trait$scenario)

delta_species_trait_new <- delta_species_trait |> 
  dplyr::filter(IUCN_category %in% c("LC", "NT", "DD")) |> 
  dplyr::mutate(
    IUCN = dplyr::case_when(
      IUCN_category %in% c("DD") ~ "DD",
      delta > 0 ~ "LC",
      delta > -30 ~ "NT",
      delta <= -80 ~ "CR",
      delta <= -50 ~ "EN",
      delta <= -30 ~ "VU")) |> 
  dplyr::group_by(scenario, IUCN) |> 
  dplyr::summarise(freq = dplyr::n()) |> 
  dplyr::mutate(stratum = round((freq * 100) / sum(freq))) |> 
  dplyr::ungroup() |> 
  dplyr::full_join(baseline_uicn) |> 
  dplyr::group_by(IUCN) |> 
  dplyr::mutate(alluvium = dplyr::cur_group_id())

delta_species_trait_new <- delta_species_trait_new |> 
  dplyr::mutate(stratum = dplyr::case_when(stratum < 1 ~ NA,
                                           stratum >= 1 ~ stratum))

library(ggplot2)
library(ggalluvial)

iucn_alluvial <- delta_species_trait_new |> 
  dplyr::ungroup() |> 
  dplyr::mutate(IUCN = forcats::fct_relevel(IUCN, c("CR", "EN", "VU", "NT", "LC", "DD")),
                stratum = dplyr::case_when(is.na(stratum) ~ NA,
                                           !is.na(stratum) ~ paste0(stratum, "%"))) |> 
  ggplot(aes(x = scenario, stratum = IUCN, alluvium = alluvium,
             y = freq, fill = IUCN, label = stratum)) +
  scale_fill_manual(values = c("CR" = "#D81E05", 
                               "EN" = "#FC7F3F", 
                               "VU" = "#F9E814", 
                               "NT" = "#CCE226",
                               "LC" = "#60C659",
                               "DD" = "#D1D1C6")) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", color = "darkgray") +
  geom_stratum(size = 0.1) +
  geom_text(stat = "stratum", size = 3) + 
  theme_bw() +
  labs(x = "", y = "Number of species", fill = "")

ggsave(iucn_alluvial, file = "figures/iucn_alluvial_hist_migration.png", width = 7.5, height = 7)



without_migration_ensemble <- readRDS("results/withoutmigration_ensemble8.rds")
without_migration_ensemble <- without_migration_ensemble[-which_species_to_remove]

delta_species <- pbmcapply::pbmclapply(1:length(without_migration_ensemble), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = without_migration_ensemble[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE,
                                                  biais = FALSE) |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(without_migration_ensemble[[i]][[1]]$species_name))
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_bind <- do.call(rbind, delta_species) |> 
  dplyr::mutate(model = "full")

load("data/raw/1c_species_traits_final.Rdata")
species_traits_final <- species_traits_final |> 
  dplyr::ungroup() |> 
  dplyr::rename(species_name = rls_species_name) |> 
  dplyr::select(species_name, trophic_guild, Length, IUCN_category, geographic_range_Albouy19, family, K)
species_traits_final <- species_traits_final[complete.cases(species_traits_final),]
species_traits_final[species_traits_final$trophic_guild == "Herbivores Microvores Detritivores",]$trophic_guild <- "herbivores"

delta_species_trait <- delta_species_bind |> 
  dplyr::inner_join(species_traits_final)

length(unique(delta_species_trait$species_name))

baseline_uicn <- species_traits_final |>
  dplyr::filter(species_name %in% unique(delta_species_bind$species_name)
                # ,
                # IUCN_category %in% c("LC", "NT")
                # IUCN_category != c("DD")
  ) |>
  dplyr::rename(IUCN = IUCN_category) |>
  dplyr::group_by(IUCN) |>
  dplyr::summarise(freq = dplyr::n())
cr <- c("CR", 0)
baseline_uicn <- rbind(baseline_uicn, cr)

baseline_uicn$freq <- as.integer(baseline_uicn$freq)

baseline_uicn <- baseline_uicn |>
  dplyr::mutate(scenario = "Baseline",
                stratum = round((freq * 100) / sum(freq), 1))

delta_species_trait$scenario <- as.character(delta_species_trait$scenario)

delta_species_trait_new <- delta_species_trait |> 
  dplyr::filter(IUCN_category %in% c("LC", "NT", "DD")) |> 
  dplyr::mutate(
    IUCN = dplyr::case_when(
      IUCN_category %in% c("DD") ~ "DD",
      delta > 0 ~ "LC",
      delta > -30 ~ "NT",
      delta <= -80 ~ "CR",
      delta <= -50 ~ "EN",
      delta <= -30 ~ "VU")) |> 
  dplyr::group_by(scenario, IUCN) |> 
  dplyr::summarise(freq = dplyr::n()) |> 
  dplyr::mutate(stratum = round((freq * 100) / sum(freq), 1)) |> 
  dplyr::ungroup() |> 
  dplyr::full_join(baseline_uicn) |> 
  dplyr::group_by(IUCN) |> 
  dplyr::mutate(alluvium = dplyr::cur_group_id())

library(ggplot2)
library(ggalluvial)

iucn_alluvial_without_migration <- delta_species_trait_new |> 
  dplyr::ungroup() |> 
  dplyr::mutate(IUCN = forcats::fct_relevel(IUCN, c("CR", "EN", "VU", "NT", "LC", "DD")),
                stratum = paste0(stratum, "%")) |> 
  ggplot(aes(x = scenario, stratum = IUCN, alluvium = alluvium,
             y = freq, fill = IUCN, label = stratum)) +
  scale_fill_manual(values = c("CR" = "#D81E05", 
                               "EN" = "#FC7F3F", 
                               "VU" = "#F9E814", 
                               "NT" = "#CCE226",
                               "LC" = "#60C659",
                               "DD" = "#D1D1C6")) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", color = "darkgray") +
  geom_stratum(size = 0.1) +
  geom_text(stat = "stratum", size = 3, check_overlap = TRUE) + 
  theme_bw() +
  labs(x = "", y = "Number of species", fill = "")

ggsave(iucn_alluvial_without_migration, file = "figures/iucn_alluvial_hist_without_migration.png")









mean_species <- performance_bind |> 
  dplyr::group_by(species_name) |> 
  dplyr::summarise(mean_r2 = mean(r2))

perf_test_0.75 <- mean_species |> 
  dplyr::filter(mean_r2 > quantile(mean_r2, probs = 0.75))

baseline_uicn_0.75 <- species_traits_final |>
  dplyr::filter(species_name %in% perf_test_0.75$species_name
                # ,
                # IUCN_category %in% c("LC", "NT")
                # IUCN_category != c("DD")
  ) |>
  dplyr::rename(IUCN = IUCN_category) |>
  dplyr::group_by(IUCN) |>
  dplyr::summarise(freq = dplyr::n())
cr <- c("CR", 0)
baseline_uicn_0.75 <- rbind(baseline_uicn_0.75, cr)

baseline_uicn_0.75$freq <- as.integer(baseline_uicn_0.75$freq)

baseline_uicn_0.75 <- baseline_uicn_0.75 |>
  dplyr::mutate(scenario = "Baseline",
                stratum = round((freq * 100) / sum(freq), 1))

delta_species_trait_new_0.75 <- delta_species_trait |> 
  dplyr::filter(species_name %in% perf_test_0.75$species_name,
                IUCN_category %in% c("LC", "NT", "DD")) |> 
  dplyr::mutate(
    IUCN = dplyr::case_when(
      IUCN_category %in% c("DD") ~ "DD",
      delta > 0 ~ "LC",
      delta > -30 ~ "NT",
      delta <= -80 ~ "CR",
      delta <= -50 ~ "EN",
      delta <= -30 ~ "VU")) |> 
  dplyr::group_by(scenario, IUCN) |> 
  dplyr::summarise(freq = dplyr::n()) |> 
  dplyr::mutate(stratum = round((freq * 100) / sum(freq))) |> 
  dplyr::ungroup() |> 
  dplyr::full_join(baseline_uicn_0.75) |> 
  dplyr::group_by(IUCN) |> 
  dplyr::mutate(alluvium = dplyr::cur_group_id())

delta_species_trait_new_0.75 <- delta_species_trait_new_0.75 |> 
  dplyr::mutate(stratum = dplyr::case_when(stratum < 1 ~ NA,
                                           stratum >= 1 ~ stratum))

library(ggplot2)
library(ggalluvial)

iucn_alluvial_0.75 <- delta_species_trait_new_0.75 |> 
  dplyr::ungroup() |> 
  dplyr::mutate(IUCN = forcats::fct_relevel(IUCN, c("CR", "EN", "VU", "NT", "LC", "DD")),
                stratum = dplyr::case_when(is.na(stratum) ~ NA,
                                           !is.na(stratum) ~ paste0(stratum, "%"))) |> 
  ggplot(aes(x = scenario, stratum = IUCN, alluvium = alluvium,
             y = freq, fill = IUCN, label = stratum)) +
  scale_fill_manual(values = c("CR" = "#D81E05", 
                               "EN" = "#FC7F3F", 
                               "VU" = "#F9E814", 
                               "NT" = "#CCE226",
                               "LC" = "#60C659",
                               "DD" = "#D1D1C6")) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", color = "darkgray") +
  geom_stratum(size = 0.1) +
  geom_text(stat = "stratum", size = 3) + 
  theme_bw() +
  labs(x = "", y = "Number of species", fill = "")

ggsave(iucn_alluvial, file = "figures/iucn_alluvial_hist_migration.png", width = 7.5, height = 7)
