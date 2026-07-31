
sdm_abs_bio5 <- function(biomass,
                         n_subsample,
                         species_name,
                         covariates,
                         future_covariates){
  
  # First, select only zero within the living area of the species considered
  
  sp <- biomass
  colnames(sp)[colnames(sp) %in% species_name] <- "biomass" # Rename the species name for consistency
  sp$biomass <- as.numeric(sp$biomass)
  
  sp <- sp |>
    dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "ecoregion")])
  
  pres <- sp |>
    dplyr::filter(biomass != 0)
  abs <- sp |>
    dplyr::filter(biomass == 0) |>
    dplyr::filter(ecoregion %in% unique(pres$ecoregion)) # Do so by considering only 0 where presences are recorded within its ecoregions
  
  sp <- rbind(pres, abs) |>
    dplyr::select(! ecoregion) |> # merge presences and selected absences
    dplyr::rename(x = longitude,
                  y = latitude)
  
  # Even with the absences selection above, it can still result in to much absences in the dataset resulting in zero inflation.
  # Thus, select randomly as much absences as presences
  
  biomass_only <- sp[which(sp[, "biomass"] > 0),]
  
  n_subsample <- nrow(sp[which(sp[, "biomass"] > 0),]) * n_subsample
  
  absence <- sp[which(sp[, "biomass"] == 0),]
  
  if(nrow(absence) > 0) {
    
    replacement <- ifelse(length(which(sp[, "biomass"] == 0)) < n_subsample, T, F)
    
    absence <- absence[sample(which(absence[, "biomass"] == 0), n_subsample, replace = replacement),]
    
  }
  
  # Combine absences and presences
  sp <- rbind(biomass_only, absence)
  
  covariates_sp <- covariates |>
    dplyr::filter(survey_id %in% sp$survey_id) |>
    dplyr::as_tibble()
  
  # Merge covariates and biomass dataset
  sp_cov <- dplyr::inner_join(sp, covariates_sp, by = "survey_id") |>
    dplyr::mutate(year = "2024") |> 
    dplyr::select(-scenario) |> 
    unique() |> 
    dplyr::mutate(scenario = NA)
  
  sp_migration <- sp_cov |>
    dplyr::select(c(survey_id, x, y, biomass)) |>
    dplyr::inner_join(rls_surveys |>
                        dplyr::select(c(survey_id, ecoregion))) |> 
    unique()
  
  survey_migration <- migration_function3(sp_biomass = sp_migration,
                                          all_biomass = rls_biomass_class_hist,
                                          ecoregion_shp = "data/raw/Ecoregions/MEOW/meow_ecos.shp",
                                          baseline_sst = unique(covariates_sp[,colnames(covariates_sp) %in% c("survey_id", "sst")]),
                                          future_sst = future_covariates[,colnames(future_covariates) %in% c("survey_id", "scenario", "sst", "year")])
  
  new_survey <- unique(survey_migration$survey_id)[which(unique(survey_migration$survey_id) %in% unique(sp_migration$survey_id) == FALSE)]
  
  if(length(new_survey) > nrow(sp_cov)) {
    
    new_survey <- sample(new_survey, nrow(sp_migration))
    
  }else{
    
    new_survey <- new_survey
    
  }

  baseline_future_immi <- future_covariates |>
    dplyr::filter(survey_id %in% survey_migration$survey_id) |>
    dplyr::inner_join(covariates[,!colnames(covariates) %in% c("chl", "ph", "sss", "sst", "gdp", "gravity")]) |>
    dplyr::rename(x = longitude,
                  y = latitude) |>
    dplyr::mutate(biomass = NA) |>
    dplyr::relocate(colnames(sp_cov))
  baseline_future_immi <- rbind(sp_cov, baseline_future_immi)

  baseline_future_immi <- baseline_future_immi |>
    noise_function(
      avoid = c("survey_id", "x", "y", "scenario", "protection_status", "biomass", "realm", "year"),
      limit = nrow(baseline_future_immi) * 0.1)
  
  baseline_future_immi[!colnames(baseline_future_immi) %in% c("survey_id", "biomass", "x", "y", "scenario", "protection_status", "year", "realm")] <- scale(baseline_future_immi[!colnames(baseline_future_immi) %in% c("survey_id", "biomass", "x", "y", "scenario", "protection_status", "year", "realm")], center = TRUE, scale = TRUE)
  
  return(baseline_future_immi)
  
}


migration_function3 <- function(sp_biomass,
                                all_biomass,
                                ecoregion_shp,
                                baseline_sst,
                                future_sst) {

  ecoregion_shp <- terra::vect(ecoregion_shp)

  cent_eco <- terra::centroids(ecoregion_shp)
  coord_cent <- terra::crds(cent_eco, df = TRUE)
  cent_eco <- cent_eco |>
    as.data.frame() |>
    cbind(coord_cent)

  sst_2075 <- future_sst |>
    dplyr::filter(year == "2075") |>
    dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "ecoregion")]) |>
    dplyr::group_by(year, scenario, ecoregion) |>
    dplyr::summarise(sst = mean(sst)) |>
    dplyr::ungroup() |>
    dplyr::group_by(year, ecoregion) |>
    dplyr::summarise(sst = min(sst))

  sst_baseline <- baseline_sst |>
    dplyr::select(c(survey_id, sst)) |>
    dplyr::mutate(year = "2024") |>
    dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "ecoregion")]) |>
    dplyr::group_by(year) |>
    dplyr::summarise(sst_min = min(sst),
                     sst_max = max(sst)) |>
    dplyr::ungroup()

  eco_ext <- ecoregion_shp[ecoregion_shp$ECOREGION %in% unique(sp_biomass$ecoregion),]
  eco_ext <- terra::ext(eco_ext)

  ecoregion_sst <- cent_eco |>
    dplyr::select(c(ECOREGION, x, y)) |>
    dplyr::rename(ecoregion = ECOREGION) |>
    dplyr::inner_join(sst_2075) |>
    dplyr::filter(year == "2075")

  ecoregion_all_sp <- terra::crop(ecoregion_shp, terra::ext(c(eco_ext[1], eco_ext[2], -35, 35)))
  ecoregion_all_sp <- ecoregion_all_sp[ecoregion_all_sp$ECOREGION %in% unique(ecoregion_sst$ecoregion),]

  # Geographical boundaries

  world_sv <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sv")

  r <- terra::rast(terra::ext(ecoregion_all_sp), res = 0.1)
  r_terre <- terra::rasterize(world_sv, r, field = 1)
  r_mer <- is.na(r_terre)  # TRUE en mer, FALSE sur terre

  # 2. Convertir en raster de 1 (mer) / NA (terre)
  r_mer <- terra::ifel(r_mer, 1, NA)

  # 3. Convertir en RasterLayer (car gdistance ne prend pas SpatRaster)
  r_mer_r <- raster::raster(r_mer)

  tr <- gdistance::transition(r_mer_r, transitionFunction = mean, directions = 8)
  tr_corr <- gdistance::geoCorrection(tr, type = "c")


  ecoregion_all_sp2 <- terra::erase(ecoregion_all_sp, world_sv)
  pts <- lapply(1:length(ecoregion_all_sp2$ECOREGION), function(k) {

    eco_k <- ecoregion_all_sp2[k,]

    one_point <- terra::spatSample(eco_k, size = 1)

  })
  pts <- do.call(rbind, pts)
  pts_sp <- as(pts, "Spatial")

  marine_distances <- gdistance::costDistance(tr_corr, pts_sp)
  ncol(marine_distances)
  nrow(marine_distances)

  marine_distances <- as.matrix(marine_distances)
  colnames(marine_distances) <- pts_sp$ECOREGION
  row.names(marine_distances) <- pts_sp$ECOREGION

  # Ecoregion continuity

  adj_eco <- terra::adjacent(ecoregion_all_sp2, type = "queen", pairs = FALSE)
  colnames(adj_eco) <- ecoregion_all_sp2$ECOREGION
  row.names(adj_eco) <- ecoregion_all_sp2$ECOREGION

  g <- igraph::graph_from_adjacency_matrix(adj_eco, mode = "undirected")
  comps <- igraph::components(g)

  ecoregion_all_sp2$cluster <- comps$membership

  # terra::plot(ecoregion_all_sp2, col = ecoregion_all_sp2$cluster)

  # library(ggplot2)
  #
  # ggplot() +
  #   geom_sf(data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"), color = NA) +
  #   geom_sf(data = sf::st_as_sf(ecoregion_all_sp2), alpha = 0.5, aes(fill = ECOREGION)) +
  #   geom_text(data = ecoregion_sst, aes(x = x, y = y, label = round(sst, 1))) +
  #   coord_sf(xlim = c(eco_ext[1], eco_ext[2]), ylim = c(-35, 35), expand = FALSE) +
  #   theme_bw()

  cluster <- data.frame(ecoregion = ecoregion_all_sp2$ECOREGION,
                        cluster = ecoregion_all_sp2$cluster)
  
  # if(length(unique()))

  migr_adj <- lapply(1:length(unique(sp_biomass$ecoregion)), function(j) {

    eco_i <- unique(sp_biomass$ecoregion)[j]

    # Filter

    # 1. Adjacent ecoregion

    eco_i_cluster <- cluster[cluster$ecoregion == eco_i,]$cluster

    adj_eco_i <- cluster |>
      dplyr::filter(cluster == eco_i_cluster)
    adj_eco_i <- adj_eco_i |>
      dplyr::inner_join(ecoregion_sst |>
                          dplyr::select(c(ecoregion, sst)))

    sst_eco_i <- adj_eco_i[adj_eco_i$ecoregion == eco_i,]$sst

    if(nrow(adj_eco_i) == 1) {

      adj_eco_i_sst <- adj_eco_i
      cluster_i <- adj_eco_i_sst |>
        dplyr::select(-sst)

    }else{

      adj_eco_i_sst <- adj_eco_i[which(dplyr::between(adj_eco_i$sst, left = sst_baseline$sst_min, right = sst_baseline$sst_max)),]

      if(nrow(adj_eco_i_sst) %in% c(0, 1)) {

        cluster_i <- cluster[cluster$ecoregion == eco_i,]

      }else{

        #Est-ce que   adj_eco_i_sst$ecoregion est continu ?

        adj_eco3 <- adj_eco[,colnames(adj_eco) %in% c(eco_i, adj_eco_i_sst$ecoregion)]
        adj_eco3 <- adj_eco3[row.names(adj_eco3) %in% c(eco_i, adj_eco_i_sst$ecoregion),]

        g_adj_eco3 <- igraph::graph_from_adjacency_matrix(adj_eco3, mode = "undirected")
        comps_adj_eco3 <- igraph::components(g_adj_eco3)

        cluster_i <- data.frame(ecoregion = names(comps_adj_eco3$membership),
                                cluster = comps_adj_eco3$membership)
        cluster_final <- cluster_i[cluster_i$ecoregion == eco_i,]$cluster
        cluster_i <- cluster_i |>
          dplyr::filter(cluster == cluster_final)

      }

    }

    # 3. Geographic barrier

    marine_distances_eco <- marine_distances[,colnames(marine_distances) %in% eco_i]

    geo_bar <- names(marine_distances_eco[which(is.infinite(marine_distances_eco))])

    cluster_i_geo <- cluster_i |>
      dplyr::filter(!ecoregion %in% geo_bar)

    if(nrow(cluster_i_geo) == 1) {

      cluster_i_geo <- cluster_i_geo

    }else{

      cluster_i_geo <- cluster_i_geo[cluster_i_geo$ecoregion != eco_i,]

    }

    cluster_i_geo <- data.frame(from = eco_i,
                                to = cluster_i_geo$ecoregion)

  })
  migr_adj_bind <- do.call(rbind, migr_adj)

  migr_adj_bind <- migr_adj_bind |>
    dplyr::select(from, to) |>
    unique()

  old_ecoregion <- unique(sp_biomass$ecoregion)
  new_ecoregion <- unique(c(migr_adj_bind$from, migr_adj_bind$to))
  new_ecoregion <- new_ecoregion[!new_ecoregion %in% old_ecoregion]


  rls_point <- all_biomass |>
    dplyr::select(c(survey_id, latitude, longitude))

  new_sp_biomass <- sp_biomass |>
    dplyr::full_join(rls_point |>
                       dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "ecoregion")]) |>
                       dplyr::filter(ecoregion %in% new_ecoregion) |>
                       dplyr::rename(x = longitude,
                                     y = latitude) |>
                       dplyr::mutate(biomass = 0)) |>
    dplyr::select(-ecoregion)

}


sdm_future_function5 <- function(biomass,
                                 species_name,
                                 model,
                                 scenario,
                                 mc.cores,
                                 migration,
                                 min.node.size){
  
  sp_cov <- biomass |> 
    dplyr::filter(year == "2024") |>
    dplyr::select(-c(year, scenario)) |>
    tidyr::drop_na() |>
    dplyr::as_tibble()
  
  if(migration == TRUE) {
    
    future <- biomass |> 
      dplyr::filter(year != "2024") |>
      dplyr::group_split(year)
    
  }else{
    
    sp_baseline <- rls_biomass_class_hist |>
      dplyr::select(survey_id, species_name)
    
    colnames(sp_baseline)[colnames(sp_baseline) %in% species_name] <- "biomass" # Rename the species name for consistency
    sp_baseline$biomass <- as.numeric(sp_baseline$biomass)
    
    sp_baseline <- sp_baseline |>
      dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "ecoregion")])
    
    pres <- sp_baseline |>
      dplyr::filter(biomass != 0)
    abs <- sp_baseline |>
      dplyr::filter(biomass == 0) |>
      dplyr::filter(ecoregion %in% unique(pres$ecoregion)) # Do so by considering only 0 where presences are recorded within its ecoregions
    
    sp_baseline <- rbind(pres, abs) |>
      dplyr::select(! ecoregion)
    
    sp_cov <- sp_cov |>
      dplyr::filter(survey_id %in% sp_baseline$survey_id)
    
    future <- biomass |>
      dplyr::filter(year != "2024",
                    survey_id %in% sp_baseline$survey_id) |>
      dplyr::group_split(year)

  }

  if(model == "xgboost"){
    
    sp_cov$protection_status <- as.character(sp_cov$protection_status)
    sp_cov[sp_cov$protection_status == "out",]$protection_status <- "0"
    sp_cov[sp_cov$protection_status == "restricted",]$protection_status <- "1"
    sp_cov[sp_cov$protection_status == "full",]$protection_status <- "2"
    sp_cov$protection_status <- as.numeric(sp_cov$protection_status)
    
    sp_cov$realm <- as.numeric(as.factor(sp_cov$realm))
    
  }
  
  ### FITTING MODELS 
  
  # fit the random forests
  
  if(model == "rf"){
    
    model_fit <- ranger::ranger(x = sp_cov[!colnames(sp_cov) %in% c("survey_id", "site_code", "biomass")],
                                y = unlist(sp_cov[colnames(sp_cov) %in% "biomass"]),
                                num.trees = 100,
                                mtry = length(colnames(sp_cov)[!colnames(sp_cov) %in% c("survey_id", "site_code", "biomass")]) / 3,
                                min.node.size = min.node.size)

    # Make validation predictions
    
    validation_predict <- predict(object = model_fit, sp_cov)$predictions
    
    validation_predict <- data.frame(survey_id = sp_cov$survey_id,
                                     validation_predict = ifelse(round(validation_predict) < 0, 0, floor(validation_predict)),
                                     biomass = sp_cov$biomass)

  }
  
  # fit the deep neural network
  
  if(model == "dnn") {

    model_fit <- cito::dnn(X = sp_cov |> 
                             dplyr::select(-c(survey_id, biomass, x, y, realm)) |> 
                             as.data.frame(),
                           Y = sp_cov |> 
                             dplyr::select(biomass) |> 
                             as.data.frame(),
                           epochs = 500,
                           lr = 0.002,
                           lr_scheduler = cito::config_lr_scheduler("reduce_on_plateau", 
                                                                    patience = 5, 
                                                                    factor = 0.5),
                           hidden = c(9),
                           loss = "mse",
                           validation = 0.2,
                           batchsize = round(0.2 * nrow(sp_cov)),
                           bootstrap = 5,
                           bootstrap_parallel = 1,
                           early_stopping = 10,
                           lambda = 0.01,
                           alpha = 0.2,
                           verbose = FALSE)
    
    # Make validation predictions
    
    validation_predict <- lapply(1:5, function(i) {
      
      as.data.frame(predict(model_fit$models[[i]], newdata = sp_cov))
      
    })
    
    validation_predict <- Reduce("+", validation_predict)
    validation_predict <- unlist(validation_predict/5)
    
    validation_predict <- data.frame(survey_id = sp_cov$survey_id,
                                     validation_predict = ifelse(round(validation_predict) < 0, 0, floor(validation_predict)),
                                     biomass = sp_cov$biomass)
    
  }
  
  # fit the xgboost
  
  if(model == "xgboost"){

    model_fit <- xgboost::xgboost(x = as.matrix(sp_cov[!colnames(sp_cov) %in% c("survey_id", "site_code", "biomass")]),
                                  y = unlist(sp_cov[colnames(sp_cov) %in% c("biomass")]),
                                  nrounds = 1000,
                                  max_depth = 4,
                                  min_child_weight = 25,
                                  min_split_loss = 3,
                                  colsample_bytree = 0.8,
                                  subsample = 0.5,
                                  learning_rate = 0.01,
                                  early_stopping_rounds = 5,
                                  objective = "reg:squarederror",
                                  nthread = parallel::detectCores() - 1)
    
    # Make validation predictions
    
    validation_predict <- predict(object = model_fit, as.matrix(sp_cov[!colnames(sp_cov) %in% c("survey_id", "site_code", "biomass")]))
    
    validation_predict <- data.frame(survey_id = sp_cov$survey_id,
                                     validation_predict = ifelse(round(validation_predict) < 0, 0, floor(validation_predict)),
                                     biomass = sp_cov$biomass)
    
  }

  # Make future predictions, per year from 2023 to 2100, for each of the three scenario
  
  # year <- "2023":"2100"
  year <- "2025":"2100"

  # Prediction loop per year
  
  validation_predict_future <- lapply(1:length(year), function(k) {
    
    year_k <- future[[k]] # Select the kth future covariates dataset (k = 1 = 2023 ; k = 2 = 2024 ; k = 3 = 2025 ; etc)
    year_k$survey_id <- as.character(year_k$survey_id)
    
    scenario <- lapply(1:length(unique(year_k$scenario)), function(l) { # Select the lth scenario dataset (l = 1 = ssp245 ; l = 2 = ssp370 ; l = 3 = ssp585)
      
      scenario_l <- year_k[year_k$scenario == unique(year_k$scenario)[l],] |> 
        dplyr::relocate(colnames(sp_cov))
      
      # Rename covariates

      if(model == "xgboost"){
        
        scenario_l$protection_status <- as.character(scenario_l$protection_status)
        scenario_l[scenario_l$protection_status == "out",]$protection_status <- "0"
        scenario_l[scenario_l$protection_status == "restricted",]$protection_status <- "1"
        scenario_l[scenario_l$protection_status == "full",]$protection_status <- "2"
        scenario_l$protection_status <- as.numeric(scenario_l$protection_status)
        
        scenario_l$realm <- as.numeric(as.factor(scenario_l$realm))
        
        # Make future predictions
        
        future_prediction <- predict(object = model_fit, as.matrix(scenario_l[!colnames(scenario_l) %in% c("survey_id", "year", "scenario", "biomass")]))
        
      }else{
        
        # Make future predictions
        
        future_prediction <- predict(object = model_fit, scenario_l)
        
      }
      
      if(model == "rf") {
        
        validation_predict_future <- data.frame(validation_predict = future_prediction$predictions)
        
      }else{
        
        validation_predict_future <- data.frame(validation_predict = future_prediction)
        
      }

      future_prediction <- validation_predict_future$validation_predict
      
      future_prediction <- data.frame(survey_id = scenario_l$survey_id,
                                      future_prediction = ifelse(round(future_prediction) < 0, 0, floor(future_prediction)))
      colnames(future_prediction)[2] <- paste0(year[k], "_", unique(year_k$scenario)[l])
      
      future_prediction
      
    })
    
  })
  
  # For each year, extract predictions for scenario 1 (ssp126)
  ssp126 <- lapply(1:length(validation_predict_future), function(i){
    
    if(i == 1){
      
      validation_predict_future[[i]][[1]]
      
    }else{
      
      validation_predict_future[[i]][[1]][2]
      
    }
    
  })
  ssp126_bind <- do.call(cbind, ssp126)
  ssp126_bind <- validation_predict |> 
    dplyr::full_join(ssp126_bind, multiple = "first") |>
    dplyr::mutate(species_name = species_name,
                  model = model,
                  scenario = scenario) |> 
    dplyr::relocate(species_name, .after = biomass) |> 
    dplyr::relocate(model, .after = species_name) |> 
    dplyr::relocate(scenario, .after = model)
  ssp126_bind[is.na(ssp126_bind)] <- 0
  
  # For each year, extract predictions for scenario 2 (ssp245)
  ssp245 <- lapply(1:length(validation_predict_future), function(i){
    
    if(i == 1){
      
      validation_predict_future[[i]][[2]]
      
    }else{
      
      validation_predict_future[[i]][[2]][2]
      
    }
    
  })
  ssp245_bind <- do.call(cbind, ssp245)
  ssp245_bind <- validation_predict |> 
    dplyr::full_join(ssp245_bind, multiple = "first") |>
    dplyr::mutate(species_name = species_name,
                  model = model,
                  scenario = scenario) |> 
    dplyr::relocate(species_name, .after = biomass) |> 
    dplyr::relocate(model, .after = species_name) |> 
    dplyr::relocate(scenario, .after = model)
  ssp245_bind[is.na(ssp245_bind)] <- 0
  
  # For each year, extract predictions for scenario 3 (ssp370)
  ssp370 <- lapply(1:length(validation_predict_future), function(i){
    
    if(i == 1){
      
      validation_predict_future[[i]][[3]]
      
    }else{
      
      validation_predict_future[[i]][[3]][2]
      
    }
    
  })
  ssp370_bind <- do.call(cbind, ssp370)
  ssp370_bind <- validation_predict |> 
    dplyr::full_join(ssp370_bind, multiple = "first") |>
    dplyr::mutate(species_name = species_name,
                  model = model,
                  scenario = scenario) |> 
    dplyr::relocate(species_name, .after = biomass) |> 
    dplyr::relocate(model, .after = species_name) |> 
    dplyr::relocate(scenario, .after = model)
  ssp370_bind[is.na(ssp370_bind)] <- 0
  
  # For each year, extract predictions for scenario 4 (ssp585)
  ssp585 <- lapply(1:length(validation_predict_future), function(i){
    
    if(i == 1){
      
      validation_predict_future[[i]][[4]]
      
    }else{
      
      validation_predict_future[[i]][[4]][2]
      
    }
    
  })
  ssp585_bind <- do.call(cbind, ssp585)
  ssp585_bind <- validation_predict |> 
    dplyr::full_join(ssp585_bind, multiple = "first") |>
    dplyr::mutate(species_name = species_name,
                  model = model,
                  scenario = scenario) |> 
    dplyr::relocate(species_name, .after = biomass) |> 
    dplyr::relocate(model, .after = species_name) |> 
    dplyr::relocate(scenario, .after = model)
  ssp585_bind[is.na(ssp585_bind)] <- 0
  
  all_pred <- list(ssp126_bind, ssp245_bind, ssp370_bind, ssp585_bind)

}


fix_cov_function <- function(data,
                             which_var_to_fix,
                             mc.cores = 1) {
  
  year <- 2024:2100
  
  if(which_var_to_fix == "hum") {
    
    env_future_env <- pbmcapply::pbmclapply(1:length(year), function(i) {
      
      year_i <- year[i]
      year_i <- data |> 
        dplyr::filter(year == year_i)
      
      baseline <- data |> 
        dplyr::filter(year == 2024)
      
      hum_i <-  baseline[colnames(baseline) %in% c("survey_id", "longitude", "latitude", colnames(baseline)[which(grepl(pattern = paste0(c("gdp", "gravity"), collapse = "|"), x = colnames(baseline)) == TRUE)])]
      
      env_i <- year_i[!colnames(year_i) %in% colnames(year_i)[which(grepl(pattern = paste0(c("gdp", "gravity"), collapse = "|"), x = colnames(year_i)) == TRUE)]]
      
      new_year_i <- env_i |>
        dplyr::inner_join(hum_i)
      
    }, mc.cores = mc.cores)
    env_future_env_bind <- do.call(rbind, env_future_env)
    
    return(env_future_env_bind)
    
  }
  
  if(which_var_to_fix == "env") {
    
    env_future_hum <- pbmcapply::pbmclapply(1:length(year), function(i) {
      
      year_i <- year[i]
      year_i <- data |> 
        dplyr::filter(year == year_i)
      
      baseline <- data |> 
        dplyr::filter(year == 2024)
      
      env_i <-  baseline[colnames(baseline) %in% c("survey_id", "longitude", "latitude", colnames(baseline)[which(grepl(pattern = paste0(c("chl", "ph", "sss", "sst"), collapse = "|"), x = colnames(baseline)) == TRUE)])]
      
      hum_i <- year_i[!colnames(year_i) %in% colnames(year_i)[which(grepl(pattern = paste0(c("chl", "ph", "sss", "sst"), collapse = "|"), x = colnames(year_i)) == TRUE)]]
      
      new_year_i <- hum_i |>
        dplyr::inner_join(env_i)
      
    }, mc.cores = 1)
    env_future_hum_bind <- do.call(rbind, env_future_hum)
    
    return(env_future_hum_bind)
    
  }
  
}
