# path_to_samplehmsc = "results/biomass_class_test_hmsc/Abudefduf_bengalensis/model/cv1/model_fit_ssp245.rds"

hmsc_convergence <- function(path_to_samplehmsc){

  model <- Hmsc::convertToCodaObject(readRDS(path_to_samplehmsc))
  # summary(test$Beta)
  plot(test$Beta)
  coda::effectiveSize(test$Beta)
  coda::gelman.diag(test$Beta,multivariate=FALSE)$psrf
  
}


# biomass = rls_biomass_i
# covariates = rls_covariates_train
# species_name = species_name[i]
# base_dir = "results/biomass_class_test_hmsc/"
# nSamples = nSamples
# thin = thin
# nChains = nChains
# verbose = verbose
# transient = transient
# studyDesign_name = NULL
# distribution = "normal"

hmsc_function <- function(biomass,
                          covariates,
                          species_name,
                          base_dir,
                          nSamples,
                          thin,
                          nChains,
                          verbose,
                          transient,
                          studyDesign_name,
                          distribution){
  
  dir.create(base_dir)
  dir.create(paste0(base_dir, stringr::str_replace_all(species_name, " ", "_"), "/"))
  
  # First, select only zero within the living area of the species considered
  
  sp <- biomass
  colnames(sp)[colnames(sp) %in% species_name] <- "biomass" # Rename the species name for consistency
  sp$biomass <- as.numeric(sp$biomass)
  sp$latitude_arr <- round(sp$latitude, 1) # Do so by considering only 0 where presences are recorded within degree of latitude
  
  pres <- sp |> 
    dplyr::filter(biomass != 0)
  abs <- sp |> 
    dplyr::filter(biomass == 0) |> 
    dplyr::filter(latitude_arr %in% unique(pres$latitude_arr))
  
  sp <- rbind(pres, abs) |> 
    dplyr::select(! latitude_arr) # merge presences and selected absences
  
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
  biomass_scv <- scv_function(sp,
                              5)

  cv_j <- pbmcapply::pbmclapply(1:length(biomass_scv), function(j) {
    
    # Select the jth training cv-fold
    training <- biomass_scv[[j]]$train[,c("survey_id", "biomass")]
    
    # Select the jth testing cv-fold
    testing <- biomass_scv[[j]]$test[,c("survey_id", "biomass")]
    
    # Create a covariate dataset for each of the three scenario : ssp245, ssp370, ssp585
    
    train_covariates_ssp245 <- covariates |> 
      dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")]) |> 
      dplyr::rename(x = longitude, y = latitude) |> 
      dplyr::filter(scenario == "ssp245",
                    survey_id %in% training$survey_id) |>
      dplyr::as_tibble()
    train_covariates_ssp245 <- train_covariates_ssp245 |> 
      # As some covariates are at the country level, it means you can have very few or even only one value for these covariates
      # Check for the number of values in each covariates and add noise if < 6 values
      noise_function(avoid = c("survey_id", "scenario", "effectiveness", "biomass", "x", "y"),
                     limit = nrow(train_covariates_ssp245))
    
    
    test_covariates_ssp245 <- covariates |> 
      dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")]) |> 
      dplyr::rename(x = longitude, y = latitude) |> 
      dplyr::filter(scenario == "ssp245",
                    survey_id %in% testing$survey_id) |>
      dplyr::as_tibble() 
    test_covariates_ssp245 <- test_covariates_ssp245 |> 
      noise_function(avoid = c("survey_id", "scenario", "effectiveness", "biomass"),
                     limit = nrow(test_covariates_ssp245)) |> 
      as.data.frame()
    
    
    train_covariates_ssp370 <- covariates |> 
      dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")]) |> 
      dplyr::rename(x = longitude, y = latitude) |> 
      dplyr::filter(scenario == "ssp370",
                    survey_id %in% training$survey_id) |>
      dplyr::as_tibble()
    train_covariates_ssp370 <- train_covariates_ssp370 |> 
      noise_function(avoid = c("survey_id", "scenario", "effectiveness", "biomass"),
                     limit = nrow(train_covariates_ssp370))
    
    
    test_covariates_ssp370 <- covariates |> 
      dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")]) |> 
      dplyr::rename(x = longitude, y = latitude) |> 
      dplyr::filter(scenario == "ssp370",
                    survey_id %in% testing$survey_id) |>
      dplyr::as_tibble()
    test_covariates_ssp370 <- test_covariates_ssp370 |> 
      noise_function(avoid = c("survey_id", "scenario", "effectiveness", "biomass"),
                     limit = nrow(test_covariates_ssp370)) |> 
      as.data.frame()
    
    
    train_covariates_ssp585 <- covariates |> 
      dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")]) |> 
      dplyr::rename(x = longitude, y = latitude) |> 
      dplyr::filter(scenario == "ssp585",
                    survey_id %in% training$survey_id) |>
      dplyr::as_tibble()
    train_covariates_ssp585 <- train_covariates_ssp585 |> 
      noise_function(avoid = c("survey_id", "scenario", "effectiveness", "biomass"),
                     limit = nrow(train_covariates_ssp585))
    
    
    test_covariates_ssp585 <- covariates |> 
      dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "latitude", "longitude")]) |> 
      dplyr::rename(x = longitude, y = latitude) |> 
      dplyr::filter(scenario == "ssp585",
                    survey_id %in% testing$survey_id) |>
      dplyr::as_tibble()
    test_covariates_ssp585 <- test_covariates_ssp585 |> 
      noise_function(avoid = c("survey_id", "scenario", "effectiveness", "biomass"),
                     limit = nrow(test_covariates_ssp585)) |> 
      as.data.frame()
    
    # Create a formula for hmsc
    
    # rename covariates
    covnames <- names(train_covariates_ssp245)
    covnames <- paste0(covnames[-which(covnames %in% c("survey_id", "scenario", "x", "y"))])
    
    # create formula with new covariate names
    fmla <- as.formula(paste(" ~ ", paste(covnames, collapse = " + ")))
    
    ### FITTING MODELS 
    # fit the HMSC, one per scenario
    
    dir.create(paste0(base_dir, stringr::str_replace_all(species_name, " ", "_"), "/model/"), showWarnings = FALSE)
    path_cv <- paste0(base_dir, stringr::str_replace_all(species_name, " ", "_"), "/model/cv", j, "/")
    dir.create(path_cv, showWarnings = FALSE)
    
    if(is.null(studyDesign_name)){
      
      init_ssp245 <- Hmsc::Hmsc(Y = as.matrix(training[,colnames(training) %in% c("biomass")]),
                                XFormula = fmla,
                                XData = train_covariates_ssp245[!colnames(train_covariates_ssp245) %in% c("survey_id", "y", "x", "site_code", "scenario")],
                                distr = distribution)
      model_fit_ssp245 <- Hmsc::sampleMcmc(init_ssp245, 
                                           samples = nSamples, 
                                           thin = thin,
                                           transient = transient, 
                                           nChains = nChains,
                                           verbose = verbose, 
                                           engine = "R")
      saveRDS(model_fit_ssp245, file = paste0(path_cv, "model_fit_ssp245.rds"))
      
      init_ssp370 <- Hmsc::Hmsc(Y = as.matrix(training[,colnames(training) %in% c("biomass")]),
                                XFormula = fmla,
                                XData = train_covariates_ssp370[!colnames(train_covariates_ssp370) %in% c("survey_id", "y", "x", "site_code", "scenario")],
                                distr = distribution)
      model_fit_ssp370 <- Hmsc::sampleMcmc(init_ssp370, 
                                           samples = nSamples, 
                                           thin = thin,
                                           transient = transient, 
                                           nChains = nChains,
                                           verbose = verbose, 
                                           engine = "R")
      saveRDS(model_fit_ssp370, file = paste0(path_cv, "model_fit_ssp370.rds"))
      
      init_ssp585 <- Hmsc::Hmsc(Y = as.matrix(training[,colnames(training) %in% c("biomass")]),
                                XFormula = fmla,
                                XData = train_covariates_ssp585[!colnames(train_covariates_ssp585) %in% c("survey_id", "y", "x", "site_code", "scenario")],
                                distr = distribution)
      model_fit_ssp585 <- Hmsc::sampleMcmc(init_ssp585, 
                                           samples = nSamples, 
                                           thin = thin,
                                           transient = transient, 
                                           nChains = nChains,
                                           verbose = verbose, 
                                           engine = "R")
      saveRDS(model_fit_ssp585, file = paste0(path_cv, "model_fit_ssp585.rds"))

    }else{
      
      ##### Set study design #####
      
      # (1) add noise to coordinates for them to differ sltitly
      train_covariates_ssp245 <- train_covariates_ssp245 |>
        dplyr::rowwise() |>
        dplyr::mutate(y = y + runif(1, -0.0001, 0.0001),
                      x = x + runif(1, -0.0001, 0.0001)) |>
        as.data.frame()
      
      rownames(train_covariates_ssp245) <- 1:nrow(train_covariates_ssp245)
      rownames(test_covariates_ssp245) <- 1:nrow(test_covariates_ssp245)
      
      # (2) create a matrix with coordinates : xycoords is a matrix with 2 columns "x-coordinate","y-coordinate" and row names with survey_id
      xycoords <- train_covariates_ssp245 |>
        dplyr::select(x, y) |> 
        as.data.frame()
      rownames(xycoords) <- 1:nrow(xycoords)
      xycoords_test <- test_covariates_ssp245 |>
        dplyr::select(x, y) |> 
        dplyr::rowwise() |>
        dplyr::mutate(y = y + runif(1, -0.0001, 0.0001),
                      x = x + runif(1, -0.0001, 0.0001)) |>
        as.data.frame()
      rownames(xycoords_test) <- rownames(1:nrow(xycoords_test))
      
      # (3) add spatial random effect and study design (rows in Y)
      rL.nngp <- Hmsc::HmscRandomLevel(sData = xycoords) #Should be True but doesn't work after
      # rL.nngp <- Hmsc::setPriors(rL.nngp, nfMin = 1, nfMax = 1)
      
      rL.nngp.test <- Hmsc::HmscRandomLevel(sData = xycoords_test) #Should be True but doesn't work after
      # rL.nngp.test <- Hmsc::setPriors(rL.nngp.test, nfMin = 1, nfMax = 1)
      
      studyDesign_df <- data.frame(sample = as.factor(row.names(train_covariates_ssp245)))
      studyDesign_df_test <- data.frame(sample = as.factor(row.names(test_covariates_ssp245)))
      
      ##### Set random levels #####
      
      random_levels <- rL.nngp
      random_levels_test <- rL.nngp.test
      
      init_ssp245 <- Hmsc::Hmsc(Y = as.matrix(training[,colnames(training) %in% c("biomass")]),
                                XFormula = fmla,
                                XData = train_covariates_ssp245[!colnames(train_covariates_ssp245) %in% c("survey_id", "y", "x", "site_code", "scenario")],
                                distr = distribution,
                                studyDesign = studyDesign_df,
                                ranLevels = list("sample" = random_levels))
      model_fit_ssp245 <- Hmsc::sampleMcmc(init_ssp245, 
                                           samples = nSamples, 
                                           thin = thin,
                                           transient = transient, 
                                           nChains = nChains,
                                           verbose = verbose, 
                                           engine = "R")
      
      init_ssp370 <- Hmsc::Hmsc(Y = as.matrix(training[,colnames(training) %in% c("biomass")]),
                                XFormula = fmla,
                                XData = train_covariates_ssp370[!colnames(train_covariates_ssp370) %in% c("survey_id", "y", "x", "site_code", "scenario")],
                                distr = distribution,
                                studyDesign = studyDesign_df,
                                ranLevels = list("sample" = random_levels))
      model_fit_ssp370 <- Hmsc::sampleMcmc(init_ssp370, 
                                           samples = nSamples, 
                                           thin = thin,
                                           transient = transient, 
                                           nChains = nChains,
                                           verbose = verbose, 
                                           engine = "R")
      
      init_ssp585 <- Hmsc::Hmsc(Y = as.matrix(training[,colnames(training) %in% c("biomass")]),
                                XFormula = fmla,
                                XData = train_covariates_ssp585[!colnames(train_covariates_ssp585) %in% c("survey_id", "y", "x", "site_code", "scenario")],
                                distr = distribution,
                                studyDesign = studyDesign_df,
                                ranLevels = list("sample" = random_levels))
      model_fit_ssp585 <- Hmsc::sampleMcmc(init_ssp585, 
                                           samples = nSamples, 
                                           thin = thin,
                                           transient = transient, 
                                           nChains = nChains,
                                           verbose = verbose, 
                                           engine = "R")
      
    }

    # Make predictions
    
    validation_predict_ssp245 <- as.vector(apply(simplify2array(predict(model_fit_ssp245, XData = test_covariates_ssp245, expected = TRUE)), c(1, 2), FUN = mean, na.rm = TRUE))
    validation_predict_ssp370 <- as.vector(apply(simplify2array(predict(model_fit_ssp370, XData = test_covariates_ssp370, expected = TRUE)), c(1, 2), FUN = mean, na.rm = TRUE))
    validation_predict_ssp585 <- as.vector(apply(simplify2array(predict(model_fit_ssp585, XData = test_covariates_ssp585, expected = TRUE)), c(1, 2), FUN = mean, na.rm = TRUE))
    
    # Create a dataframe with the three predicted biomass values and the observed ones
    
    validation_predict <- data.frame(survey_id = test_covariates_ssp245$survey_id,
                                     ssp245 = ifelse(round(validation_predict_ssp245) < 0, 0, round(validation_predict_ssp245)),
                                     ssp370 = ifelse(round(validation_predict_ssp370) < 0, 0, round(validation_predict_ssp370)),
                                     ssp585 = ifelse(round(validation_predict_ssp585) < 0, 0, round(validation_predict_ssp585)))
    
    validation_observed <- testing[,c("survey_id", "biomass")]
    
    validation_obs_prd <- validation_predict |>
      dplyr::inner_join(validation_observed, multiple = "first") |> 
      dplyr::mutate(species_name = species_name,
                    cv = j,
                    model = "hmsc")
    
    colnames(validation_obs_prd)[colnames(validation_obs_prd) == "biomass"] <- "validation_observed"
    
    validation_obs_prd
    
  }, mc.cores = 5)
  
  # Bind the 5 folds, it should have as much rows as the "sp" object (not if some MPA effectiveness have been removed)
  
  all_pred <- do.call(rbind, cv_j)
  
  # Save the data

  save(all_pred, file = paste0(base_dir, stringr::str_replace_all(species_name, " ", "_"), "/", stringr::str_replace_all(species_name, " ", "_"), ".Rdata"))
  
}


