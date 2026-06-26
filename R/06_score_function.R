
score_function <- function(pred_biomass_data,
                           score_data){
  
  rescale_01 <- function(x){(x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))}
  
  score_data$pearson <- rescale_01(score_data$pearson)
  
  score_data$spearman <- rescale_01(score_data$spearman)
  
  species <- mean(sapply(pred_biomass_data, length))
  
  score <- pbmcapply::pbmclapply(1:species, function(i) {
    
    sp_i <- lapply(pred_biomass_data, '[[', i)
    
    sp_i <- lapply(1:length(sp_i), function(j) {
      
      model_j <- sp_i[[j]]
      
      model_k <- lapply(1:length(model_j), function(k) {
        
        model_jk <- model_j[[k]]
        
        model_jk <- model_jk[,c("survey_id", "species_name", "model", "validation_predict", colnames(model_jk)[which(grepl(scenarii[k], colnames(model_jk)))])]
        
        colnames(model_jk)[which(grepl(scenarii[k], colnames(model_jk)))] <- gsub(pattern = paste0("_", scenarii[k]), replacement = "", colnames(model_jk)[which(grepl(scenarii[k], colnames(model_jk)))])
        
        colnames(model_jk)[colnames(model_jk) == "validation_predict"] <- "2024"

        model_jk <- model_jk |> 
          dplyr::mutate(dplyr::across(`2024`:`2100`, ~ dplyr::case_when(. > max(decile_save$biomass_class) ~ 29, 
                                                                        . < min(decile_save$biomass_class) ~ 0,
                                                                        TRUE ~ .)))
        
        model_jk <- model_jk |> 
          dplyr::mutate(dplyr::across(where(is.numeric), ~ decile_save$biomass[match(., decile_save$biomass_class)] * 10^-3))
        
        model_jk <- model_jk |> 
          dplyr::mutate(scenario = scenarii[k])
        
        model_jk
        
      })
      
      model_k_bind <- do.call(rbind, model_k)
      
    })
    
    sp_i <- do.call(rbind, sp_i)
    
    perf_species_j <- score_data[score_data$species_name == unique(sp_i$species_name),]

    dtf_score <- perf_species_j[, colnames(perf_species_j) %in% c("species_name", "model", "pearson")] |> 
      dplyr::inner_join(sp_i) |> 
      dplyr::mutate(dplyr::across(`2024`:`2100`) * pearson) |> 
      dplyr::group_by(species_name, survey_id, scenario) |> 
      dplyr::summarise(dplyr::across(`2024`:`2100`, mean)) |> 
      dplyr::mutate(model = "scored_model") |> 
      dplyr::ungroup() |> 
      dplyr::group_split(scenario)

  }, mc.cores = parallel::detectCores() - 1)

}
