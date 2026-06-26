############################################################################################################
####################################         Future predictions         #################################### 
####################################    with and withtout migration     #################################### 
############################################################################################################

### Load function and data

source("R/sdm_future_function.R")
load("data/extracted/rls_biomass_class_hist.Rdata")
load("data/raw/00_rls_surveys.Rdata")

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

species_name <- colnames(rls_biomass_class_hist)[!colnames(rls_biomass_class_hist) %in% c("survey_id", "site_code", "latitude", "longitude", "ecoregion")]

#################################### Biomass class species_specific model, future, full (env + hum) #####################################

species_biomass <- list.files("results/biomass_baseline_future_hist8", full.names = TRUE)


### Random Forest ##

base_dir <- "results/biomass_class_future_rf_hist_migration8"
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
                                   migration = TRUE,
                                   min.node.size = 50)

  dir.create(base_dir)

  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))

}, mc.cores = parallel::detectCores() - 1))


base_dir <- "results/biomass_class_future_rf_hist_withoutmigration8"
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
                                   migration = FALSE)

  dir.create(base_dir)

  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))

}, mc.cores = parallel::detectCores() - 1))


### Xgboost ###

base_dir <- "results/biomass_class_future_xgboost_hist_migration9"
scenario <- "full"
base_dir <- paste0(base_dir, "_", scenario, "/")

for(i in 1:length(species_biomass)){

  load(species_biomass[i])
  rls_biomass_i <- sp

  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "xgboost",
                                   scenario = scenario,
                                   mc.cores = 1,
                                   migration = TRUE)

  dir.create(base_dir)

  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))

}


base_dir <- "results/biomass_class_future_xgboost_hist_withoutmigration9"
scenario <- "full"
base_dir <- paste0(base_dir, "_", scenario, "/")

for(i in 1:length(species_biomass)){
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "xgboost",
                                   scenario = scenario,
                                   mc.cores = 1,
                                   migration = FALSE)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}


### DNN ###

base_dir <- "results/biomass_class_future_dnn_hist_migration9"
scenario <- "full"
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "dnn",
                                   scenario = scenario,
                                   mc.cores = parallel::detectCores() - 1,
                                   migration = TRUE)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))


base_dir <- "results/biomass_class_future_dnn_hist_withoutmigration9"
scenario <- "full"
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "dnn",
                                   scenario = scenario,
                                   mc.cores = parallel::detectCores() - 1,
                                   migration = FALSE)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))

biomass_class_future_dnn_hist_withoutmigration9_full <- list.files("results/biomass_class_future_dnn_hist_withoutmigration9_full", full.names = TRUE)
biomass_class_future_dnn_hist_withoutmigration9_full <- lapply(1:length(biomass_class_future_dnn_hist_withoutmigration9_full), function(i) {

  load(biomass_class_future_dnn_hist_withoutmigration9_full[i])
  assign(as.character(i), all_pred)

})
save(biomass_class_future_dnn_hist_withoutmigration9_full, file = "results/biomass_class_future_dnn_hist_withoutmigration9_full.Rdata")
