
bray_curtis_function <- function(data) {

  bray_curstis_scenario <- lapply(1:length(scenarii), function(i) {
    
    sc_i <- lapply(data, '[[', i)
    
    sc_i_bind <- do.call(rbind, sc_i)
    
    year <- 2022:2100
    
    sc_i_split_year <- pbmcapply::pbmclapply(1:length(year), function(j) {
      
      year_j <- as.character(year[j])
      
      sc_j <- sc_i_bind[, colnames(sc_i_bind) %in% c("survey_id", "species_name", "scenario", "model", year_j)]
      
      sc_j_spread <- sc_j |> 
        dplyr::select(-model) |> 
        tidyr::pivot_wider(names_from = species_name,
                           values_from = year_j,
                           values_fn = mean,
                           values_fill = 0) |> 
        dplyr::mutate(year = year_j)
      
    }, mc.cores = parallel::detectCores() - 1)
    
    bray_curstis_dist_year <- pbmcapply::pbmclapply(1:length(sc_i_split_year), function(j){
      
      baseline_matrix <- sc_i_split_year[[1]][!colnames(sc_i_split_year[[1]]) %in% c("scenario", "year")]
      
      matrix_year <- sc_i_split_year[[j]][!colnames(sc_i_split_year[[j]]) %in% c("scenario", "year")]
      
      bray_curtis_dist <- abs(as.matrix(matrix_year[,-1]) - as.matrix(baseline_matrix[,-1])) / (as.matrix(matrix_year[,-1]) + as.matrix(baseline_matrix[,-1]))
      
      bray_curtis_dist <- bray_curtis_dist |> 
        as.data.frame()
      
      bray_curtis_dist <- bray_curtis_dist |> 
        colMeans(na.rm = TRUE)
      
      bray_curtis_dist <- data.frame(species_name = names(bray_curtis_dist),
                                     scenario = unique(sc_i_split_year[[1]]$scenario),
                                     bray_curtis = bray_curtis_dist)
      
      colnames(bray_curtis_dist)[colnames(bray_curtis_dist) %in% "bray_curtis"] <- paste0(colnames(bray_curtis_dist)[colnames(bray_curtis_dist) %in% "bray_curtis"], "_", unique(sc_i_split_year[[j]]$year))
      
      bray_curtis_dist
      
    }, mc.cores = parallel::detectCores() - 1)
    
    bray_curstis_dist_year_bind <- purrr::reduce(bray_curstis_dist_year, dplyr::inner_join)
    
  })

}