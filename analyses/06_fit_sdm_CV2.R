############################################################################################################
####################################    Test models predictive power   #####################################
################################    and sensitivity to hyperparameters    ##################################
############################################################################################################

### Load function and data

source("R/04_noise_function.R")
source("R/03_cross_validation_function.R")
source("R/sdm_cv_function.R")
load("data/extracted/rls_covariates_train_hist2.RData")
load("data/raw/00_rls_surveys.Rdata")
load("data/extracted/rls_biomass_class_hist.Rdata")

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

species_name <- colnames(rls_biomass_class_hist)[!colnames(rls_biomass_class_hist) %in% c("survey_id", "site_code", "latitude", "longitude", "ecoregion")]

covariates <- rls_covariates_train_hist |>
  dplyr::group_by(survey_id) |>
  dplyr::summarise(dplyr::across(c(chl:marine_ecosystem_dependency, Plateau_500m:Lagoon), mean))
effectiveness <- rls_covariates_train_hist |>
  dplyr::select(survey_id, protection_status) |> 
  unique()
covariates <- covariates |>
  dplyr::inner_join(effectiveness) |> 
  tidyr::drop_na()


###### Run models ######

### Random Forest ###

base_dir <- "results/biomass_class_test_rf_hist_ns1/"
unlist(pbmcapply::pbmclapply(1:length(species_name), function(i) {
  
  rls_biomass_i <- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <- sdm_cv_function(biomass = rls_biomass_i,
                              covariates = covariates,
                              species_name = species_name[i],
                              cv = 5,
                              model = "rf",
                              mc.cores = 1,
                              min.node.size = 1)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))


base_dir <- "results/biomass_class_test_rf_hist_ns5/"
unlist(pbmcapply::pbmclapply(1:length(species_name), function(i) {
  
  rls_biomass_i <- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <- sdm_cv_function(biomass = rls_biomass_i,
                              covariates = covariates,
                              species_name = species_name[i],
                              cv = 5,
                              model = "rf",
                              mc.cores = 1,
                              min.node.size = 5)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))


base_dir <- "results/biomass_class_test_rf_hist_ns25/"
unlist(pbmcapply::pbmclapply(1:length(species_name), function(i) {
  
  rls_biomass_i <- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <- sdm_cv_function(biomass = rls_biomass_i,
                              covariates = covariates,
                              species_name = species_name[i],
                              cv = 5,
                              model = "rf",
                              mc.cores = 1,
                              min.node.size = 25)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))


base_dir <- "results/biomass_class_test_rf_hist_ns50/"
unlist(pbmcapply::pbmclapply(1:length(species_name), function(i) {
  
  rls_biomass_i <- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <- sdm_cv_function(biomass = rls_biomass_i,
                              covariates = covariates,
                              species_name = species_name[i],
                              cv = 5,
                              model = "rf",
                              mc.cores = 1,
                              min.node.size = 50)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))


### Xgboost ###

base_dir <- "results/biomass_class_test_xgboost_hist_md6_mcw1_msl0/"
for(i in 1:length(species_name)){
  
  rls_biomass_i <<- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <<- sdm_cv_function(biomass = rls_biomass_i,
                               covariates = covariates,
                               species_name = species_name[i],
                               cv = 5,
                               model = "xgboost",
                               mc.cores = 1,
                               max_depth = 6,
                               min_child_weight = 1,
                               min_split_loss = 0)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}


base_dir <- "results/biomass_class_test_xgboost_hist_md5_mcw5_msl2/"
for(i in 1:length(species_name)){
  
  rls_biomass_i <<- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <<- sdm_cv_function(biomass = rls_biomass_i,
                               covariates = covariates,
                               species_name = species_name[i],
                               cv = 5,
                               model = "xgboost",
                               mc.cores = 1,
                               max_depth = 5,
                               min_child_weight = 5,
                               min_split_loss = 2)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}


base_dir <- "results/biomass_class_test_xgboost_hist_md4_mcw25_msl3/"
for(i in 1:length(species_name)){
  
  rls_biomass_i <<- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <<- sdm_cv_function(biomass = rls_biomass_i,
                               covariates = covariates,
                               species_name = species_name[i],
                               cv = 5,
                               model = "xgboost",
                               mc.cores = 1,
                               max_depth = 4,
                               min_child_weight = 25,
                               min_split_loss = 3)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}


base_dir <- "results/biomass_class_test_xgboost_hist_md3_mcw50_msl5/"
for(i in 1:length(species_name)){
  
  rls_biomass_i <<- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <<- sdm_cv_function(biomass = rls_biomass_i,
                               covariates = covariates,
                               species_name = species_name[i],
                               cv = 5,
                               model = "xgboost",
                               mc.cores = 1,
                               max_depth = 3,
                               min_child_weight = 50,
                               min_split_loss = 5)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}


### DNN ###

base_dir <- "results/biomass_class_test_dnn_hist_h11_lr005_ld001/"
unlist(pbmcapply::pbmclapply(1:length(species_name), function(i) {
  
  rls_biomass_i <- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <- sdm_cv_function(biomass = rls_biomass_i,
                              covariates = covariates,
                              species_name = species_name[i],
                              cv = 5,
                              model = "dnn",
                              mc.cores = 1,
                              hidden = 11,
                              lr = 0.005,
                              lambda = 0.001)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))


base_dir <- "results/biomass_class_test_dnn_hist_h10_lr003_ld005/"
unlist(pbmcapply::pbmclapply(1:length(species_name), function(i) {
  
  rls_biomass_i <- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <- sdm_cv_function(biomass = rls_biomass_i,
                              covariates = covariates,
                              species_name = species_name[i],
                              cv = 5,
                              model = "dnn",
                              mc.cores = 1,
                              hidden = 10,
                              lr = 0.003,
                              lambda = 0.005)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))


base_dir <- "results/biomass_class_test_dnn_hist_h9_lr002_ld01/"
unlist(pbmcapply::pbmclapply(1:length(species_name), function(i) {
  
  rls_biomass_i <- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <- sdm_cv_function(biomass = rls_biomass_i,
                              covariates = covariates,
                              species_name = species_name[i],
                              cv = 5,
                              model = "dnn",
                              mc.cores = 1,
                              hidden = 9,
                              lr = 0.002,
                              lambda = 0.01)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))


base_dir <- "results/biomass_class_test_dnn_hist_h8_lr001_ld1/"
unlist(pbmcapply::pbmclapply(1:length(species_name), function(i) {
  
  rls_biomass_i <- rls_biomass_class_hist[, c("survey_id", "longitude", "latitude", "site_code", species_name[i])]
  
  all_pred <- sdm_cv_function(biomass = rls_biomass_i,
                              covariates = covariates,
                              species_name = species_name[i],
                              cv = 5,
                              model = "dnn",
                              mc.cores = 1,
                              hidden = 8,
                              lr = 0.001,
                              lambda = 0.1)
  
  dir.create(base_dir)
  
  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name[i], " ", "_"), ".Rdata"))
  
}, mc.cores = parallel::detectCores() - 1))
