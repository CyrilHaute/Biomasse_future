########################################################################################################
################################        Test future predictions         ################################
################################     sensitivity to hyperparameters     ################################ 
########################################################################################################

### Load function and data

source("R/sdm_future_function.R")
load("data/extracted/rls_biomass_class_hist.Rdata")
load("data/raw/00_rls_surveys.Rdata")

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

species_name <- colnames(rls_biomass_class_hist)[!colnames(rls_biomass_class_hist) %in% c("survey_id", "site_code", "latitude", "longitude", "ecoregion")]


species_biomass <- list.files("results/biomass_baseline_future_hist8", full.names = TRUE)

base_dir <- "results/biomass_class_future_rf_hist_withoutmigration8_mns_1"
scenario <- "full"
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "rf",
                                   scenario = scenario,
                                   mc.cores = 1,
                                   migration = FALSE,
                                   min.node.size = 1)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))

biomass_class_future_rf_hist_withoutmigration8_mns_1_full <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_mns_1_full", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_mns_1_full <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_mns_1_full), function(i) {

  load(biomass_class_future_rf_hist_withoutmigration8_mns_1_full[i])
  assign(as.character(i), all_pred)

})
save(biomass_class_future_rf_hist_withoutmigration8_mns_1_full, file = "results/biomass_class_future_rf_hist_withoutmigration8_mns_1_full.Rdata")


base_dir <- "results/biomass_class_future_rf_hist_withoutmigration8_mns_5"
scenario <- "full"
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "rf",
                                   scenario = scenario,
                                   mc.cores = 1,
                                   migration = FALSE,
                                   min.node.size = 5)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))

biomass_class_future_rf_hist_withoutmigration8_mns_5_full <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_mns_5_full", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_mns_5_full <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_mns_5_full), function(i) {

  load(biomass_class_future_rf_hist_withoutmigration8_mns_5_full[i])
  assign(as.character(i), all_pred)

})
save(biomass_class_future_rf_hist_withoutmigration8_mns_5_full, file = "results/biomass_class_future_rf_hist_withoutmigration8_mns_5_full.Rdata")



base_dir <- "results/biomass_class_future_rf_hist_withoutmigration8_mns_25"
scenario <- "full"
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "rf",
                                   scenario = scenario,
                                   mc.cores = 1,
                                   migration = FALSE,
                                   min.node.size = 25)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))

biomass_class_future_rf_hist_withoutmigration8_mns_25_full <- list.files("results/biomass_class_future_rf_hist_withoutmigration8_mns_25_full", full.names = TRUE)
biomass_class_future_rf_hist_withoutmigration8_mns_25_full <- lapply(1:length(biomass_class_future_rf_hist_withoutmigration8_mns_25_full), function(i) {

  load(biomass_class_future_rf_hist_withoutmigration8_mns_25_full[i])
  assign(as.character(i), all_pred)

})
save(biomass_class_future_rf_hist_withoutmigration8_mns_25_full, file = "results/biomass_class_future_rf_hist_withoutmigration8_mns_25_full.Rdata")
