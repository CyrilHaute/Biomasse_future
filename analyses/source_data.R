######################################################################################
################################      Data source     ################################ 
######################################################################################

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


################################      Figure 1/S1     ################################ 


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
write.csv(figure1, file = "data/source_data/figure1AB.csv", row.names = FALSE)



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
write.csv(figure1_realm, file = "data/source_data/figure1C.csv", row.names = FALSE)


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
write.csv(figureS1_realm, file = "data/source_data/figureS1.csv", row.names = FALSE)


################################      Figure 2    ################################ 


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


delta_species_bind$delta_round <- round(delta_species_bind$delta_pr)

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

figure2_without <- delta_species_without_bind |> 
  dplyr::mutate(stability = ifelse(delta_round %in% c(-5:5), "Stable",
                                   ifelse(delta_round < -5, "Decrease",
                                          ifelse(delta_round > 5, "Increase", NA)))) |> 
  dplyr::mutate(range_shift = "without")
figure2 <- figure2_with |> 
  dplyr::full_join(figure2_without)
write.csv(figure2, file = "data/source_data/figure2.csv", row.names = FALSE)


################################      Figure 3    ################################ 


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


################################      Figure 4    ################################ 


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

delta_species_trait <- delta_species_bind |> 
  dplyr::inner_join(species_traits_final) |> 
  dplyr::inner_join(sp_baseline_bind)

delta_model <- ranger::ranger(x = delta_species_trait[!colnames(delta_species_trait) %in% c("delta_pr", "delta_biomass", "species_name", "model", "IUCN_category")],
                              y = unlist(delta_species_trait[colnames(delta_species_trait) %in% "delta_pr"]),
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

figure4A <- var_imp |> 
  dplyr::mutate(delta_pr = (delta * 100) / sum(delta))
write.csv(figure4A, file = "data/source_data/figure4A.csv", row.names = FALSE)


scenario <- unique(delta_species_trait$scenario)

partial_delta_range <- lapply(1:length(scenario), function(i) {
  
  ssp <- delta_species_trait[delta_species_trait$scenario == scenario[i],]
  
  pdp::partial(delta_model,
               train = ssp,
               pred.var = c("geographic_range_Albouy19")) |>
    dplyr::rename(delta = yhat,
                  var = geographic_range_Albouy19) |>
    dplyr::mutate(scenario = scenario[i],
                  var_name = "geographic_range")
  
})
partial_delta_range_bind <- do.call(rbind, partial_delta_range)

partial_delta_length <- lapply(1:length(scenario), function(i) {
  
  ssp <- delta_species_trait[delta_species_trait$scenario == scenario[i],]
  
  pdp::partial(delta_model,
               train = ssp,
               pred.var = c("Length")) |>
    dplyr::rename(delta = yhat,
                  var = Length) |>
    dplyr::mutate(scenario = scenario[i],
                  var_name = "Length")
  
})
partial_delta_length_bind <- do.call(rbind, partial_delta_length)

partial_delta_n <- lapply(1:length(scenario), function(i) {
  
  ssp <- delta_species_trait[delta_species_trait$scenario == scenario[i],]
  
  pdp::partial(delta_model,
               train = ssp,
               pred.var = c("n_future")) |>
    dplyr::rename(delta = yhat,
                  var = n_future) |>
    dplyr::mutate(scenario = scenario[i],
                  var_name = "range_shift")
  
})
partial_delta_n_bind <- do.call(rbind, partial_delta_n)

figure4BCD <- partial_delta_range_bind |> 
  dplyr::full_join(partial_delta_length_bind) |> 
  dplyr::full_join(partial_delta_n_bind)
write.csv(figure4BCD, file = "data/source_data/figure4BCD.csv", row.names = FALSE)


################################      Figure S3    ################################ 


saved_species <- c("Caranx_melampygus", "Anampses_twistii", "Abudefduf_vaigiensis")
which_saved_species <- which(list_species %in% saved_species)

plot_species <- pbmcapply::pbmclapply(1:length(which_saved_species), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = migration_ensemble[which_saved_species[i]],
                                                                                         unit = "kg",
                                                                                         time = as.character(c("2024":"2075")),
                                                                                         score = TRUE,
                                                                                         biais = TRUE) |>
                                                         dplyr::group_by(scenario) |>
                                                         dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * -100,
                                                                       upper = (1 - (upper / dplyr::first(biomass))) * -100,
                                                                       biomass = (1 - (biomass / dplyr::first(biomass))) * -100,
                                                                       species_name = list_species[which_saved_species[i]]) |>
                                                         dplyr::ungroup()
}, mc.cores = 1)
figureS3 <- do.call(rbind, plot_species)
write.csv(figureS3, file = "data/source_data/figureS3.csv", row.names = FALSE)


################################      Figure S4    ################################ 


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

delta_species_trait2 <- delta_species_bind |> 
  dplyr::inner_join(species_traits_final) |> 
  dplyr::filter(scenario %in% c("SSP1-2.6", "SSP5-8.5"))

figureS4 <- delta_species_trait2 |> 
  dplyr::mutate(trophic_guild2 = paste0(trophic_guild, "_", scenario))
write.csv(figureS4, file = "data/source_data/figureS4.csv", row.names = FALSE)


################################      Figure S5    ################################ 


load("data/raw/00_rls_surveys.Rdata")
rls_surveys$survey_id <- as.character(rls_surveys$survey_id)
load("data/extracted/env_train_hist.Rdata")

figureS5 <- rls_surveys |> 
  dplyr::inner_join(env_train_hist |> 
                      dplyr::ungroup() |> 
                      dplyr::select(c(longitude, latitude)) |> 
                      unique()) |> 
  dplyr::ungroup() |> 
  dplyr::select(latitude, longitude) |> 
  unique()
write.csv(figureS5, file = "data/source_data/figureS5.csv", row.names = FALSE)


################################      Figure S9    ################################ 


species_biomass <- list.files("results/biomass_baseline_future_hist8", full.names = TRUE)


species_var_imp <- pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  
  baseline <- sp |> 
    dplyr::filter(year == 2024)
  
  sp_cov <- baseline |> 
    dplyr::filter(year == "2024") |>
    dplyr::select(-c(year, scenario)) |>
    tidyr::drop_na() |>
    dplyr::as_tibble()
  
  ### FITTING MODELS 
  
  # fit the random forests
  
  model_fit <- ranger::ranger(x = sp_cov[!colnames(sp_cov) %in% c("survey_id", "site_code", "biomass", "x", "y", "realm")],
                              y = unlist(sp_cov[colnames(sp_cov) %in% "biomass"]),
                              num.trees = 100,
                              mtry = length(colnames(sp_cov)[!colnames(sp_cov) %in% c("survey_id", "site_code", "biomass")]) / 3,
                              importance = "permutation")
  
  var_imp <- data.frame(imp = model_fit$variable.importance,
                        covariates = unique(names(c(model_fit$variable.importance))),
                        r2 = round(model_fit$r.squared, 2),
                        species = stringr::str_replace_all(gsub(".Rdata", "", gsub(pattern = "results/biomass_baseline_future_hist8/", "", species_biomass[i])), "_", " "))
  
}, mc.cores = parallel::detectCores() - 1)
species_var_imp_bind <- do.call(rbind, species_var_imp)


figureS9 <- species_var_imp_bind |> 
  dplyr::group_by(covariates) |> 
  dplyr::summarise(med_imp = median(imp),
                   q_05_imp = quantile(imp, probs = 0.05),
                   q_95_imp = quantile(imp, probs = 0.95),
                   sd_imp = sd(imp)) |> 
  dplyr::mutate(var_type = dplyr::case_when(covariates %in% c("gdp", "gravity", "hdi", "marine_ecosystem_dependency",
                                                              "neartt", "ngo", "protection_status") ~ "HUM",
                                            covariates %in% c("Lagoon", "Patch_Reefs_500m", "Plateau_500m", "Reef_Crest_500m",
                                                              "Reef_Flat", "Reef_Slope", "Terrestrial_Reef_Flat_500m", "depth",
                                                              "reef_extent") ~ "HAB",
                                            covariates %in% c("chl", "ph", "sss", "sst") ~ "ENV"))
write.csv(figureS9, file = "data/source_data/figureS9.csv", row.names = FALSE)


################################      Figure S13    ################################ 


species_removed_example <- pbmcapply::pbmclapply(1:length(migration_ensemble[which_species_example]), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = migration_ensemble[which_species_example][i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2100")),
                                                  score = TRUE,
                                                  biais = TRUE) |>
    dplyr::group_by(scenario) |>
    dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                  upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                  biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                  species_name = list_species[which_species_example[i]]) |>
    dplyr::ungroup()
  
}, mc.cores = parallel::detectCores() - 1)
figureS13 <- do.call(rbind, species_removed_example)
write.csv(figureS13, file = "data/source_data/figureS13.csv", row.names = FALSE)


################################      Figure S14    ################################ 


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


rf_plot <- future_pred_scale_function(data = biomass_class_future_rf_hist_migration8_full[-which_species_to_remove],
                                      unit = "kg",
                                      time = as.character(c("2024":"2075")),
                                      biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                range_shift = "Migration") |> 
  dplyr::ungroup()
rf_plot_wm <- future_pred_scale_function(data = biomass_class_future_rf_hist_withoutmigration8_full[-which_species_to_remove],
                                         unit = "kg",
                                         time = as.character(c("2024":"2075")),
                                         biais = FALSE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                range_shift = "Without migration") |> 
  dplyr::ungroup()
dnn_plot <- future_pred_scale_function(data = biomass_class_future_dnn_hist_migration9_full[-which_species_to_remove],
                                       unit = "kg",
                                       time = as.character(c("2024":"2075")),
                                       biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                range_shift = "Migration") |> 
  dplyr::ungroup()
dnn_plot_wm <- future_pred_scale_function(data = biomass_class_future_dnn_hist_withoutmigration9_full[-which_species_to_remove],
                                          unit = "kg",
                                          time = as.character(c("2024":"2075")),
                                          biais = FALSE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                range_shift = "Without migration") |> 
  dplyr::ungroup()
xgboost_plot <- future_pred_scale_function(data = biomass_class_future_xgboost_hist_migration9_full[-which_species_to_remove],
                                           unit = "kg",
                                           time = as.character(c("2024":"2075")),
                                           biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                range_shift = "Migration") |> 
  dplyr::ungroup()
xgboost_plot_wm <- future_pred_scale_function(data = biomass_class_future_xgboost_hist_withoutmigration9_full[-which_species_to_remove],
                                              unit = "kg",
                                              time = as.character(c("2024":"2075")),
                                              biais = FALSE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                range_shift = "Without migration") |> 
  dplyr::ungroup()


without_migration_ensemble_plot_all <- future_pred_scale_function(data = without_migration_ensemble[-which_species_to_remove],
                                                                  unit = "kg",
                                                                  time = as.character(c("2024":"2075")),
                                                                  score = TRUE,
                                                                  biais = FALSE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(model = "Ensemble model",
                range_shift = "Without migration")
migration_ensemble_plot_all <- future_pred_scale_function(data = migration_ensemble[-which_species_to_remove],
                                                          unit = "kg",
                                                          time = as.character(c("2024":"2075")),
                                                          score = TRUE,
                                                          biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(model = "Ensemble model",
                range_shift = "Migration")

figureS14 <- rf_plot |> 
  dplyr::full_join(rf_plot_wm) |> 
  dplyr::full_join(dnn_plot) |> 
  dplyr::full_join(dnn_plot_wm) |> 
  dplyr::full_join(xgboost_plot) |> 
  dplyr::full_join(xgboost_plot_wm) |> 
  dplyr::full_join(without_migration_ensemble_plot_all) |> 
  dplyr::full_join(migration_ensemble_plot_all)
write.csv(figureS14, file = "data/source_data/figureS14.csv", row.names = FALSE)


################################      Figure S15    ################################ 


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
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                Q = "100%") |> 
  dplyr::ungroup()

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
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                Q = "50%") |> 
  dplyr::ungroup()

species_0.75 <- which(species %in% perf_test_0.75$species_name)
ensemble_model_scale_0.75 <- future_pred_scale_function(data = migration_ensemble[species_0.75],
                                                        unit = "kg",
                                                        time = as.character(c("2024":"2075")),
                                                        score = TRUE,
                                                        biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                Q = "75%") |> 
  dplyr::ungroup()

species_0.95 <- which(species %in% perf_test_0.95$species_name)
ensemble_model_scale_0.95 <- future_pred_scale_function(data = migration_ensemble[species_0.95],
                                                        unit = "kg",
                                                        time = as.character(c("2024":"2075")),
                                                        score = TRUE,
                                                        biais = TRUE) |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                biomass = (1 - (biomass / dplyr::first(biomass))) * - 100,
                Q = "95%") |> 
  dplyr::ungroup()

figureS15 <- ensemble_model_scale |> 
  dplyr::full_join(ensemble_model_scale_0.5) |> 
  dplyr::full_join(ensemble_model_scale_0.75) |> 
  dplyr::full_join(ensemble_model_scale_0.95)
write.csv(figureS15, file = "data/source_data/figureS15.csv", row.names = FALSE)
