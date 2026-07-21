############################################################################################################
####################################         Future predictions         #################################### 
####################################     importance of hum and env      #################################### 
############################################################################################################

### Load function and data

source("R/sdm_future_function.R")
load("data/extracted/rls_biomass_class_hist.Rdata")
load("data/raw/00_rls_surveys.Rdata")

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

species_name <- colnames(rls_biomass_class_hist)[!colnames(rls_biomass_class_hist) %in% c("survey_id", "site_code", "latitude", "longitude", "ecoregion")]

#################################### Biomass class species_specific model, future #####################################

species_biomass <- list.files("results/biomass_baseline_future_hist8", full.names = TRUE)


### Random Forest ##

base_dir <- "results/biomass_class_future_rf_hist_withoutmigration8"
scenario <- "env" # environmental covariates evolve and human covariates are fixed
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  rls_biomass_i <- fix_cov_function(data = rls_biomass_i,
                                    which_var_to_fix = "hum")
  
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
scenario <- "hum" # human covariates evolve and environmental covariates are fixed
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  rls_biomass_i <- fix_cov_function(data = rls_biomass_i,
                                    which_var_to_fix = "env")
  
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


### Xgboost ###

base_dir <- "results/biomass_class_future_xgboost_hist_withoutmigration9"
scenario <- "env"
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(lapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  rls_biomass_i <- fix_cov_function(data = rls_biomass_i,
                                    which_var_to_fix = "hum",
                                    mc.cores = parallel::detectCores() - 1)
  
  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "xgboost",
                                   scenario = scenario,
                                   mc.cores = 1,
                                   migration = FALSE)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}))


base_dir <- "results/biomass_class_future_xgboost_hist_withoutmigration9"
scenario <- "hum"
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(lapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  rls_biomass_i <- fix_cov_function(data = rls_biomass_i,
                                    which_var_to_fix = "env",
                                    mc.cores = parallel::detectCores() - 1)
  
  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "xgboost",
                                   scenario = scenario,
                                   mc.cores = 1,
                                   migration = FALSE)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}))


### DNN ###

base_dir <- "results/biomass_class_future_dnn_hist_withoutmigration9"
scenario <- "env"
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  rls_biomass_i <- fix_cov_function(data = rls_biomass_i,
                                    which_var_to_fix = "hum",
                                    mc.cores = parallel::detectCores() - 1)
  
  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "dnn",
                                   scenario = scenario,
                                   mc.cores = parallel::detectCores() - 1,
                                   migration = FALSE)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = 1))


base_dir <- "results/biomass_class_future_dnn_hist_withoutmigration9"
scenario <- "hum"
base_dir <- paste0(base_dir, "_", scenario, "/")

unlist(pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  rls_biomass_i <- sp
  
  rls_biomass_i <- fix_cov_function(data = rls_biomass_i,
                                    which_var_to_fix = "env",
                                    mc.cores = parallel::detectCores() - 1)
  
  all_pred <- sdm_future_function5(biomass = rls_biomass_i,
                                   species_name = species_name[i],
                                   model = "dnn",
                                   scenario = scenario,
                                   mc.cores = parallel::detectCores() - 1,
                                   migration = FALSE)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = 1))
