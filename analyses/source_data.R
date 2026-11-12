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

load("data/extracted/species_to_remove_dtf.Rdata")
species_to_remove <- species_to_remove_dtf
species_to_remove <- unique(species_to_remove$species)
species_to_remove <- stringr::str_replace_all(species_to_remove, " ", "_")
list_species <- gsub(".Rdata", "", list.files("results/biomass_class_future_rf_hist_migration8_full", full.names = FALSE))

which_species_to_remove <- which(list_species %in% species_to_remove)

species_example <- c("Chaetodon kleinii", "Lutjanus carponotatus", "Plectorhinchus multivittatus")
species_example <- stringr::str_replace_all(species_example, " ", "_")

which_species_example <- which(list_species %in% species_example)

migration_ensemble <- readRDS("results/migration_ensemble8.rds")
without_migration_ensemble <- readRDS("results/withoutmigration_ensemble8.rds")

figure1_without <- future_pred_scale_function(data = without_migration_ensemble[-which_species_to_remove],
                                  unit = "kg",
                                  time = as.character(c("2024":"2075")),
                                  score = TRUE,
                                  biais = FALSE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(model = "Without range shift")
figure1_with <- future_pred_scale_function(data = migration_ensemble[-which_species_to_remove],
                                  unit = "kg",
                                  time = as.character(c("2024":"2075")),
                                  score = TRUE,
                                  biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(model = "With range shift")
figure1 <- figure1_without |> 
  dplyr::full_join(figure1_with)
dir.create("data/source_data/")
write.csv(figure1, file = "data/source_data/figure1.csv", row.names = FALSE)



load("data/extracted/rls_biomass_class.Rdata")

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

rls_surveys <- rls_surveys |> 
  dplyr::filter(survey_id %in% rls_biomass_class$survey_id)

test <- rls_surveys |> 
  dplyr::select(realm) |> 
  unique()

migration_ensemble <- migration_ensemble[-which_species_to_remove]

future_pred_ecoregion_by_realm <- lapply(1:length(unique(test$realm)), function(i) {
  
  realm_i <- unique(test$realm)[i]
  
  ensemble_model_realm <- pbmcapply::pbmclapply(1:length(migration_ensemble), function(j) {
    
    species_j <- migration_ensemble[[j]]
    
    species_j_realm <- lapply(1:length(species_j), function(k) {
      
      sp_k <- species_j[[k]] |> 
        dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "realm")]) |> 
        dplyr::filter(realm == realm_i) |> 
        dplyr::select(-realm)
      
    })
    
  }, mc.cores = parallel::detectCores() - 1)
  
  data <- future_pred_scale_function(data = ensemble_model_realm,
                                     unit = "kg",
                                     time = as.character(c("2024":"2075")),
                                     scale = "realm",
                                     score = TRUE,
                                     biais = TRUE) |> 
    dplyr::filter(year %in% c(2024, 2030, 2050, 2075)) |> 
    dplyr::group_by(scenario, scale) |> 
    dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                  upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                  biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |>
    dplyr::ungroup() |>
    dplyr::filter(year %in% c(2030, 2050, 2075))

})
figure1_realm <- do.call(rbind, future_pred_ecoregion_by_realm)
write.csv(figure1_realm, file = "data/source_data/figure1_realm.csv", row.names = FALSE)


without_migration_ensemble <- without_migration_ensemble[-which_species_to_remove]

future_pred_ecoregion_by_realm_without <- lapply(1:length(unique(test$realm)), function(i) {
  
  realm_i <- unique(test$realm)[i]
  
  ensemble_model_realm <- pbmcapply::pbmclapply(1:length(without_migration_ensemble), function(j) {
    
    species_j <- without_migration_ensemble[[j]]
    
    species_j_realm <- lapply(1:length(species_j), function(k) {
      
      sp_k <- species_j[[k]] |> 
        dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "realm")]) |> 
        dplyr::filter(realm == realm_i) |> 
        dplyr::select(-realm)
      
    })
    
  }, mc.cores = parallel::detectCores() - 1)
  
  data <- future_pred_scale_function(data = ensemble_model_realm,
                                     unit = "kg",
                                     time = as.character(c("2024":"2075")),
                                     scale = "realm",
                                     score = TRUE,
                                     biais = TRUE) |> 
    dplyr::filter(year %in% c(2024, 2030, 2050, 2075)) |> 
    dplyr::group_by(scenario, scale) |> 
    dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                  upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                  biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |>
    dplyr::ungroup() |>
    dplyr::filter(year %in% c(2030, 2050, 2075))
  
})
figureS1_realm <- do.call(rbind, future_pred_ecoregion_by_realm_without)
write.csv(figureS1_realm, file = "data/source_data/figureS1_realm.csv", row.names = FALSE)



delta_species <- pbmcapply::pbmclapply(1:length(migration_ensemble), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = migration_ensemble[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE,
                                                  biais = TRUE)
  
  future_pred_unlog$biomass <- log10(future_pred_unlog$biomass)
  
  delta_biomass <- future_pred_unlog[future_pred_unlog$year == "2075",]$biomass - future_pred_unlog[future_pred_unlog$year == "2024",]$biomass
  delta_biomass <- data.frame(delta_biomass = delta_biomass,
                              scenario = unique(future_pred_unlog$scenario),
                              species_name = unique(migration_ensemble[[i]][[1]]$species_name))
  
  future_pred_unlog <- future_pred_unlog |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(migration_ensemble[[i]][[1]]$species_name)) |> 
    dplyr::full_join(delta_biomass) |> 
    dplyr::rename(delta_pr = delta)
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_bind <- do.call(rbind, delta_species) |> 
  dplyr::mutate(model = "full")

max(delta_species_bind$delta_pr)



delta_species_bind$delta_round <- round(delta_species_bind$delta_pr)
stab_y <- c(0.56, 0.8, 0.68)

figure2_with <- delta_species_bind |> 
  dplyr::mutate(stability = ifelse(delta_round %in% c(-5:5), "Stable",
                                   ifelse(delta_round < -5, "Decrease",
                                          ifelse(delta_round > 5, "Increase", NA)))) |> 
  dplyr::mutate(range_shift = "with")

delta_species_without <- pbmcapply::pbmclapply(1:length(without_migration_ensemble), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = without_migration_ensemble[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE,
                                                  biais = TRUE)
  
  future_pred_unlog$biomass <- log10(future_pred_unlog$biomass + 1)
  
  delta_biomass <- future_pred_unlog[future_pred_unlog$year == "2075",]$biomass - future_pred_unlog[future_pred_unlog$year == "2024",]$biomass
  delta_biomass <- data.frame(delta_biomass = delta_biomass,
                              scenario = unique(future_pred_unlog$scenario),
                              species_name = unique(migration_ensemble[[i]][[1]]$species_name))
  
  future_pred_unlog <- future_pred_unlog |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(migration_ensemble[[i]][[1]]$species_name)) |> 
    dplyr::full_join(delta_biomass) |> 
    dplyr::rename(delta_pr = delta)
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_without_bind <- do.call(rbind, delta_species_without) |>
  dplyr::mutate(model = "full")

delta_species_without_bind$delta_round <- round(delta_species_without_bind$delta_pr)
stab_y <- c(0.2, 0.4, 0.3)

figure2_without <- delta_species_without_bind |> 
  dplyr::mutate(stability = ifelse(delta_round %in% c(-5:5), "Stable",
                                   ifelse(delta_round < -5, "Decrease",
                                          ifelse(delta_round > 5, "Increase", NA)))) |> 
  dplyr::mutate(range_shift = "without")
figure2 <- figure2_with |> 
  dplyr::full_join(figure2_without)
write.csv(figure2, file = "data/source_data/figure2.csv", row.names = FALSE)



delta_species <- pbmcapply::pbmclapply(1:length(migration_ensemble), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = migration_ensemble[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE,
                                                  biais = TRUE) |> 
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
  dplyr::mutate(step = "1")

test <- delta_species_trait |> 
  dplyr::filter(!IUCN_category %in% c("LC", "NT", "DD")) |> 
  dplyr::mutate(IUCN = IUCN_category) |> 
  dplyr::group_by(scenario, IUCN) |> 
  dplyr::summarise(freq = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(step = "2")

test2 <- delta_species_trait_new |> 
  dplyr::full_join(test) |>
  dplyr::group_by(scenario, IUCN) |> 
  dplyr::summarise(freq = sum(freq)) |> 
  dplyr::mutate(stratum = round((freq * 100) / sum(freq), 1)) |> 
  dplyr::ungroup() |>
  dplyr::full_join(baseline_uicn) |>
  dplyr::group_by(IUCN) |>
  dplyr::mutate(alluvium = dplyr::cur_group_id())

figure3_with <- test2 |> 
  dplyr::ungroup() |> 
  dplyr::mutate(IUCN = forcats::fct_relevel(IUCN, c("CR", "EN", "VU", "NT", "LC", "DD")),
                stratum = dplyr::case_when(is.na(stratum) ~ NA,
                                           !is.na(stratum) ~ paste0(stratum, "%")),
                range = "with")


delta_species_without_migration <- pbmcapply::pbmclapply(1:length(without_migration_ensemble), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = without_migration_ensemble[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE,
                                                  biais = FALSE) |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(without_migration_ensemble[[i]][[1]]$species_name))
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_without_migration_bind <- do.call(rbind, delta_species_without_migration) |> 
  dplyr::mutate(model = "full")

delta_species_trait <- delta_species_without_migration_bind |> 
  dplyr::inner_join(species_traits_final)

baseline_uicn <- species_traits_final |>
  dplyr::filter(species_name %in% unique(delta_species_without_migration_bind$species_name)
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
  dplyr::mutate(step = "1")

test <- delta_species_trait |> 
  dplyr::filter(!IUCN_category %in% c("LC", "NT", "DD")) |> 
  dplyr::mutate(IUCN = IUCN_category) |> 
  dplyr::group_by(scenario, IUCN) |> 
  dplyr::summarise(freq = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(step = "2")

test2 <- delta_species_trait_new |> 
  dplyr::full_join(test) |>
  dplyr::group_by(scenario, IUCN) |> 
  dplyr::summarise(freq = sum(freq)) |> 
  dplyr::mutate(stratum = round((freq * 100) / sum(freq), 1)) |> 
  dplyr::ungroup() |>
  dplyr::full_join(baseline_uicn) |>
  dplyr::group_by(IUCN) |>
  dplyr::mutate(alluvium = dplyr::cur_group_id())

figure3_without <- test2 |> 
  dplyr::ungroup() |> 
  dplyr::mutate(IUCN = forcats::fct_relevel(IUCN, c("CR", "EN", "VU", "NT", "LC", "DD")),
                stratum = paste0(stratum, "%"),
                range = "without")
figure3 <- figure3_with |> 
  dplyr::full_join(figure3_without)
write.csv(figure3, file = "data/source_data/figure3.csv", row.names = FALSE)
