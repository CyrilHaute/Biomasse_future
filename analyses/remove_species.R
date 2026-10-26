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
  
  # plot_future_pred_scale_function(data = future_pred_unlog,
  #                                 group_name = unique(migration_ensemble[[i]][[1]]$species_name),
  #                                 title = unique(migration_ensemble[[i]][[1]]$species_name),
  #                                 x_name = "",
  #                                 y_name = "Biomass (kg)",
  #                                 color_lab = "",
  #                                 model_facet = FALSE,
  #                                 face = "italic",
  #                                 breaks = c(2025, 2050, 2100),
  #                                 color_scale = color_scale)
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_bind <- do.call(rbind, delta_species)

delta_species_bind <- delta_species_bind |> 
  dplyr::filter(any_true > 0)

length(unique(delta_species_bind$sp))


sp_list <- sapply(1:length(migration_ensemble), function(i) {
  
  unique(migration_ensemble[[i]][[1]]$species_name)
  
})
