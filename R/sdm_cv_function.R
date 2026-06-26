
sdm_cv_function <- function(biomass,
                            covariates,
                            species_name,
                            cv,
                            model,
                            mc.cores,
                            min.node.size = NULL,
                            max_depth = NULL,
                            min_child_weight = NULL,
                            min_split_loss = NULL,
                            hidden = NULL,
                            lambda = NULL,
                            lr = NULL){
  
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
    dplyr::select(! ecoregion) # merge presences and selected absences
  
  # Even with the absences selection above, it can still result in to much absences in the dataset resulting in zero inflation.
  # Thus, select randomly as much absences as presences
  
  biomass_only <- sp[which(sp[, "biomass"] > 0),]
  
  n_subsample <- nrow(sp[which(sp[, "biomass"] > 0),])
  
  absence <- sp[which(sp[, "biomass"] == 0),]
  
  if(nrow(absence) > 0) {
    
    replacement <- ifelse(length(which(sp[, "biomass"] == 0)) < n_subsample, T, F)
    
    absence <- absence[sample(which(absence[, "biomass"] == 0), n_subsample, replace = replacement),]
    
  }
  
  # Combine absences and presences
  sp <- rbind(biomass_only, absence)
  
  # Create spatial k-fold cross-validation dataset, here with 5 fold, each fold being splited in 80% for training and 20% for testing. The spatial compenent can resulting in less than 80% of data in the training set
  biomass_scv <<- scv_function(sp,
                              cv)
  
  covariates[!colnames(covariates) %in% c("survey_id", "scenario", "protection_status")] <- scale(covariates[!colnames(covariates) %in% c("survey_id", "scenario", "protection_status")], center = TRUE, scale = TRUE)
  
  if(model == "xgboost"){
    
    covariates$protection_status <- as.character(covariates$protection_status)
    covariates[covariates$protection_status == "out",]$protection_status <- "0"
    covariates[covariates$protection_status == "restricted",]$protection_status <- "1"
    covariates[covariates$protection_status == "full",]$protection_status <- "2"
    covariates$protection_status <- as.numeric(covariates$protection_status)
    
  }
  
  # Fit model for each fold
  
  if(model != "xgboost"){
    
    cv_j <- lapply(1:length(biomass_scv), function(j) {
      
      # Select the jth training cv-fold
      training <- biomass_scv[[j]]$train[,c("survey_id", "biomass")]
      
      # Select the jth testing cv-fold
      testing <- biomass_scv[[j]]$test[,c("survey_id", "biomass")]
      
      # Add coordinates as a covariate
      
      training <- training |> 
        dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")]) |> 
        dplyr::rename(x = longitude, y = latitude)
      
      testing <- testing |> 
        dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")]) |> 
        dplyr::rename(x = longitude, y = latitude)
      
      train_covariates <- covariates |> 
        dplyr::filter(survey_id %in% training$survey_id) |>
        dplyr::as_tibble()
      train_covariates <- train_covariates |> 
        # As some covariates are at the country level, it means you can have very few or even only one value for these covariates
        # Check for the number of values in each covariates and add noise if < 6 values
        noise_function(avoid = c("survey_id", "scenario", "protection_status", "biomass"),
                       limit = 0.1 * nrow(train_covariates))
      
      test_covariates <- covariates |> 
        dplyr::filter(survey_id %in% testing$survey_id) |>
        dplyr::as_tibble() 
      test_covariates <- test_covariates |> 
        noise_function(avoid = c("survey_id", "scenario", "protection_status", "biomass"),
                       limit = 0.1 * nrow(test_covariates))
      
      # Merge covariates and biomass dataset
      training <- dplyr::inner_join(training, train_covariates, by = "survey_id")
      
      # Merge covariates and biomass dataset
      testing <- dplyr::inner_join(testing, test_covariates, by = "survey_id")
      
      ### FITTING MODELS 
      # fit the random forests, one per scenario
      
      if(model == "rf"){
        
        model_fit <- ranger::ranger(x = training[!colnames(training) %in% c("survey_id", "biomass", "scenario")],
                                    y = unlist(training[colnames(training) %in% "biomass"]),
                                    num.trees = 100,
                                    mtry = length(colnames(training)[!colnames(training) %in% c("survey_id", "biomass", "scenario")]),
                                    min.node.size = min.node.size)
        
        # Make predictions
        
        validation_predict  <- predict(object = model_fit, testing)$predictions
        
      }
      
      if(model == "dnn"){
        
        model_fit <- cito::dnn(X = training |> 
                                 dplyr::select(-c(survey_id, biomass, x, y)) |> 
                                 as.data.frame(),
                               Y = training |> 
                                 dplyr::select(biomass) |> 
                                 as.data.frame(),
                               epochs = 500,
                               lr = lr,
                               lr_scheduler = cito::config_lr_scheduler("reduce_on_plateau",
                                                                        patience = 5, 
                                                                        factor = 0.5),
                               hidden = hidden,
                               loss = "mse",
                               validation = 0.2,
                               batchsize = round(0.1*nrow(training)),
                               bootstrap = 5,
                               bootstrap_parallel = 1,
                               early_stopping = 10,
                               lambda = lambda,
                               alpha = 0.2,
                               verbose = TRUE)
        
        # Make predictions
        
        validation_predict <- lapply(1:5, function(i) {
          
          as.data.frame(predict(model_fit$models[[i]], newdata = testing))
          
        })
        
        validation_predict <- Reduce("+", validation_predict)
        validation_predict <- unlist(validation_predict/5)
        
      }
      
      # Create a dataframe with the three predicted biomass values and the observed ones
      
      validation_predict <- data.frame(survey_id = testing$survey_id,
                                       validation_predict = ifelse(round(validation_predict) < 0, 0, round(validation_predict)))
      # validation_predict <- data.frame(survey_id = testing$survey_id,
      #                                  validation_predict = as.numeric(validation_predict))
      
      validation_observed <- testing[,c("survey_id", "biomass")]
      
      validation_obs_prd <- validation_predict |>
        dplyr::inner_join(validation_observed, multiple = "first") |> 
        dplyr::mutate(species_name = species_name,
                      cv = j,
                      model = model,
                      min.node.size = if(is.null(min.node.size)) { NA }else{ min.node.size },
                      max_depth = if(is.null(max_depth)) { NA }else{ max_depth },
                      min_child_weight = if(is.null(min_child_weight)) { NA }else{ min_child_weight },
                      min_split_loss = if(is.null(min_split_loss)) { NA }else{ min_split_loss },
                      hidden = if(is.null(hidden)) { NA }else{ hidden },
                      lr = if(is.null(lr)) { NA }else{ lr },
                      lambda = if(is.null(lambda)) { NA }else{ lambda })
      
      colnames(validation_obs_prd)[colnames(validation_obs_prd) == "biomass"] <- "validation_observed"
      
      validation_obs_prd
      
    })
  
  }else{
    
    # Fit model for each fold
    cv_j <- list()
    
    for(j in 1:length(biomass_scv)) {
      
      # Select the jth training cv-fold
      training <- biomass_scv[[j]]$train[,c("survey_id", "biomass")]
      
      # Select the jth testing cv-fold
      testing <- biomass_scv[[j]]$test[,c("survey_id", "biomass")]
      
      # Add coordinates as a covariate
      
      training <- training |> 
        dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")]) |> 
        dplyr::rename(x = longitude, y = latitude)
      
      testing <- testing |> 
        dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")]) |> 
        dplyr::rename(x = longitude, y = latitude)
      
      train_covariates <- covariates |> 
        dplyr::filter(survey_id %in% training$survey_id) |>
        dplyr::as_tibble()
      train_covariates <- train_covariates |> 
        # As some covariates are at the country level, it means you can have very few or even only one value for these covariates
        # Check for the number of values in each covariates and add noise if < 6 values
        noise_function(avoid = c("survey_id", "scenario", "protection_status", "biomass"),
                       limit = 0.1 * nrow(train_covariates))
      
      test_covariates <- covariates |> 
        dplyr::filter(survey_id %in% testing$survey_id) |>
        dplyr::as_tibble() 
      test_covariates <- test_covariates |> 
        noise_function(avoid = c("survey_id", "scenario", "protection_status", "biomass"),
                       limit = 0.1 * nrow(test_covariates))
      
      # Merge covariates and biomass dataset
      training <- dplyr::inner_join(training, train_covariates, by = "survey_id")
      
      # Merge covariates and biomass dataset
      testing <- dplyr::inner_join(testing, test_covariates, by = "survey_id")
      
      model_fit <- xgboost::xgboost(x = as.matrix(training[!colnames(training) %in% c("survey_id", "site_code", "biomass")]),
                                    y = unlist(training[colnames(training) %in% c("biomass")]),
                                    nrounds = 1000,
                                    max_depth = max_depth,
                                    min_child_weight = min_child_weight,
                                    min_split_loss = min_split_loss,
                                    colsample_bytree = 0.8,
                                    subsample = 0.5,
                                    learning_rate = 0.01,
                                    early_stopping_rounds = 5,
                                    objective = "reg:squarederror",
                                    nthread = parallel::detectCores() - 1)
      
      # Make predictions
      
      validation_predict  <- predict(object = model_fit, as.matrix(testing[!colnames(testing) %in% c("survey_id", "site_code", "biomass")]))
      
      # Create a dataframe with the three predicted biomass values and the observed ones
      
      validation_predict <- data.frame(survey_id = testing$survey_id,
                                       validation_predict = ifelse(round(validation_predict) < 0, 0, round(validation_predict)))
      
      validation_observed <- testing[,c("survey_id", "biomass")]
      
      validation_obs_prd <- validation_predict |>
        dplyr::inner_join(validation_observed, multiple = "first") |> 
        dplyr::mutate(species_name = species_name,
                      cv = j,
                      model = model,
                      min.node.size = if(is.null(min.node.size)) { NA }else{ min.node.size },
                      max_depth = if(is.null(max_depth)) { NA }else{ max_depth },
                      min_child_weight = if(is.null(min_child_weight)) { NA }else{ min_child_weight },
                      min_split_loss = if(is.null(min_split_loss)) { NA }else{ min_split_loss },
                      hidden = if(is.null(hidden)) { NA }else{ hidden },
                      lr = if(is.null(lr)) { NA }else{ lr },
                      lambda = if(is.null(lambda)) { NA }else{ lambda })
      
      colnames(validation_obs_prd)[colnames(validation_obs_prd) == "biomass"] <- "validation_observed"
      
      cv_j[[j]] <- validation_obs_prd
      
    }
    
  }
  
  # Bind the 5 folds, it should have as much rows as the "sp" object
  
  all_pred <- do.call(rbind, cv_j)
  
  return(all_pred)
  
  # # Save the data
  # 
  # dir.create(base_dir)
  # 
  # save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name, " ", "_"), ".Rdata"))
  
}
