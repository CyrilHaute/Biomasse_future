source("R/future_pred_scale_function.R")
source("R/06_score_function.R")

migration_ensemble <- readRDS("results/migration_ensemble8.rds")
without_migration_ensemble <- readRDS("results/withoutmigration_ensemble8.rds")

scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")

delta_species <- pbmcapply::pbmclapply(1:length(migration_ensemble), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = migration_ensemble[i],
                                                  unit = "kg",
                                                  time = as.character(c("2025":"2075")), # Define the non-analogous year
                                                  score = TRUE,
                                                  biais = TRUE,
                                                  mc.cores = 1) |> 
    dplyr::mutate(sp = unique(migration_ensemble[[i]][[1]]$species_name))
  
  any_true_i <- lapply(1:4, function(i) {
    
    future_pred_unlog_i <- future_pred_unlog[future_pred_unlog$scenario == unique(future_pred_unlog$scenario)[i],]
    
    future_pred_unlog_i$var_biomass_t <- c(NA, diff(future_pred_unlog_i$biomass))

    med <- median(future_pred_unlog_i$var_biomass_t[!is.na(future_pred_unlog_i$var_biomass_t)])
    mad_v <- mad(future_pred_unlog_i$var_biomass_t[!is.na(future_pred_unlog_i$var_biomass_t)])
    
    # Seuil de détection
    seuil <- 20 * mad_v
    
    # Détection des variations anormales
    future_pred_unlog_i$saut_anormal <- abs(future_pred_unlog_i$var_biomass_t - med) > seuil
    
    any_true <- data.frame(sp = unique(future_pred_unlog_i$sp),
                           scenario = as.character(unique(future_pred_unlog_i$scenario)),
                           any_true = nrow(future_pred_unlog_i[future_pred_unlog_i$saut_anormal %in% TRUE, ]))
    
  })
  any_true_i_bind <- do.call(rbind, any_true_i)

}, mc.cores = parallel::detectCores() - 1)
delta_species_bind <- do.call(rbind, delta_species)

delta_species_bind <- delta_species_bind |> 
  dplyr::filter(any_true > 0)


delta_species_without <- pbmcapply::pbmclapply(1:length(without_migration_ensemble), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = without_migration_ensemble[i],
                                                  unit = "kg",
                                                  time = as.character(c("2025":"2075")), # Define the non-analogous year
                                                  score = TRUE,
                                                  biais = TRUE,
                                                  mc.cores = 1) |> 
    dplyr::mutate(sp = unique(without_migration_ensemble[[i]][[1]]$species_name))
  
  any_true_i <- lapply(1:4, function(i) {
    
    future_pred_unlog_i <- future_pred_unlog[future_pred_unlog$scenario == unique(future_pred_unlog$scenario)[i],]
    
    future_pred_unlog_i$var_biomass_t <- c(NA, diff(future_pred_unlog_i$biomass))
    
    med <- median(future_pred_unlog_i$var_biomass_t[!is.na(future_pred_unlog_i$var_biomass_t)])
    mad_v <- mad(future_pred_unlog_i$var_biomass_t[!is.na(future_pred_unlog_i$var_biomass_t)])
    
    # Seuil de détection
    seuil <- 20 * mad_v
    
    # Détection des variations anormales
    future_pred_unlog_i$saut_anormal <- abs(future_pred_unlog_i$var_biomass_t - med) > seuil
    
    any_true <- data.frame(sp = unique(future_pred_unlog_i$sp),
                           scenario = as.character(unique(future_pred_unlog_i$scenario)),
                           any_true = nrow(future_pred_unlog_i[future_pred_unlog_i$saut_anormal %in% TRUE, ]))
    
  })
  any_true_i_bind <- do.call(rbind, any_true_i)

}, mc.cores = parallel::detectCores() - 1)
delta_species_without_bind <- do.call(rbind, delta_species_without)

delta_species_without_bind <- delta_species_without_bind |> 
  dplyr::filter(any_true > 0)

species_to_remove_with_dtf <- data.frame(species = delta_species_bind$sp,
                                         scenario = delta_species_bind$scenario,
                                         range_shift = "with")
species_to_remove_without_dtf <- data.frame(species = delta_species_without_bind$sp,
                                            scenario = delta_species_without_bind$scenario,
                                            range_shift = "without")

species_to_remove_dtf <- species_to_remove_with_dtf |> 
  dplyr::full_join(species_to_remove_without_dtf)

save(species_to_remove_dtf, file = "data/extracted/species_to_remove_dtf.Rdata")
