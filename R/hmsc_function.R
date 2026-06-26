
# path_to_samplehmsc = "results/biomass_class_hmsc_cv/hmsc_100_samples_1000_thin_2_chains_normal_distribution_59_species/cv1/post_file.rds"
# path_to_call_hmsc = "results/biomass_class_hmsc_cv/hmsc_100_samples_1000_thin_2_chains_normal_distribution_59_species/cv1/m_cv1.rds"
# path_to_save = "figures/hmsc/convergence/"
# nSamples = nSamples
# thin = thin
# nChains = nChains
# verbose = verbose
# transient = nSamples * thin
# ns = 59
# distribution = "normal"
# covariate = "sst"

hmsc_convergence <- function(path_to_samplehmsc,
                             path_to_call_hmsc,
                             path_to_save,
                             nSamples,
                             thin,
                             nChains,
                             verbose,
                             transient,
                             distribution,
                             ns,
                             covariate){
  
  require(ggplot2)

  dir.create(paste0(path_to_save))
  path_hmsc <- paste0(path_to_save, "hmsc_", nSamples, "_samples_", thin, "_thin_", nChains, "_chains_", distribution, "_distribution_", ns, "_species/")
  dir.create(path_hmsc)
  
  importFromHPC <- jsonify::from_json(readRDS(path_to_samplehmsc)[[1]])
  postList <- importFromHPC[1:nChains]
  cat(sprintf("fitting time %.1f sec\n", importFromHPC[[nChains+1]]))
  
  m <- readRDS(path_to_call_hmsc)
  
  fittf <- Hmsc::importPosteriorFromHPC(m, postList, nSamples, thin, transient)
  
  ######## Overall  convergence ########
  
  # 4 panel : 1 ess(beta) objectif = maximiser chain * sample, psrf(beta) objectif = 1
  convergence <- Hmsc::convertToCodaObject(fittf)
  
  png(filename = paste0(path_hmsc, "mcmc_diagnostic.png"))
  
  par(mfrow = c(2,2))
  ess.beta <- coda::effectiveSize(convergence$Beta)
  psrf.beta <- coda::gelman.diag(convergence$Beta, multivariate = FALSE)$psrf
  hist(ess.beta)
  hist(psrf.beta)
  sppairs <- matrix(sample(x = 1:ns^2, size = 100))
  tmp <- convergence$Omega[[1]]
  for (chain in 1:length(tmp)){
    
    tmp[[chain]] = tmp[[chain]][,sppairs]
    
  }
  ess.omega <- coda::effectiveSize(tmp)
  psrf.omega <- coda::gelman.diag(tmp, multivariate = FALSE)$psrf
  hist(ess.omega)
  hist(psrf.omega)
  
  dev.off()
  
  
  
  ######## Chains convergence  ######## 
  
  # ggmc permet de comparer les chaines, toutes les chaines doivent donner +/- les mêmes résultats
  # see help at : http://xavier-fim.net/post/using_ggmcmc/
  
  S <- ggmcmc::ggs(convergence$Beta)
  S <- S[grep(paste(covariate, collapse = "|"), S$Parameter),]
  
  # quick look on the distribution of the values and the shape of the posterior distribution.
  # ggmcmc::ggs_histogram(S)+facet_wrap(~ Parameter, ncol = 5)
  
  # ggs_density() allows comparing the target distribution by chains and whether each
  # chain has converged in a similar space.
  ggmcmc::ggs_density(S) + facet_wrap(~ Parameter, ncol = 5)
  ggplot2::ggsave(filename = paste0(path_hmsc, "density_chain_convergence.pdf"), width = 15, height = 10)
  
  # assess convergence and diagnose chain problems. the expected outcome is to produce
  # “white noise”. + a good tool to assess within-chain convergence
  ggmcmc::ggs_traceplot(S) + facet_wrap(~ Parameter, ncol = 5)
  ggplot2::ggsave(filename = paste0(path_hmsc, "noise_convergence.pdf"), width = 15, height = 10)
  
  # The expected output is a line that quickly approaches the overall mean, in addition
  # to the fact that all chains are expected to have the same mean
  ggmcmc::ggs_running(S) + facet_wrap(~ Parameter, ncol = 5)
  ggplot2::ggsave(filename = paste0(path_hmsc, "mean_convergence.pdf"), width = 15, height = 10)
         
  # Ideally, the initial and final parts of the chain have to be sampling in the same
  # target distribution
  # ggmcmc::ggs_compare_partial(S)+facet_wrap(~ Parameter, ncol = 5)
  
  # The autocorrelation plot expectes a bar at one in the first lag, but no autocorrelation
  # beyond it. While autocorrelation is not per se a signal of lack of convergence,
  # it may indicate some misbehaviour of several chains or parameters, or indicate that
  # a chain needs more time to converge.
  ggmcmc::ggs_autocorrelation(S)+facet_wrap(~ Parameter, ncol = 5)
  ggplot2::ggsave(filename = paste0(path_hmsc, "autocorrelation_convergence.pdf"), width = 15, height = 10)
  
  # diagnose potential problems of convergence due to highly correlated parameters
  # ggmcmc::ggs_crosscorrelation(S)
  # compare the between-chain variation with the within-chain variation. It is expected to be close to 1.
  # ggmcmc::ggs_Rhat(S) + xlab("R_hat")
  # ggmcmc::ggs_caterpillar(S)
  
}



nSamples = nSamples
thin = thin
nChains = nChains
verbose = verbose
transient = transient
datay = Y
datax = XData
formula = model_formula
distribution = "normal"
studyDesign_name = c("spatial", "association")
nNeighbours = 25
save_hmsc_path = "results/biomass_class_hmsc_cv/"

hmsc_function_scv <- function(nSamples,
                              thin,
                              nChains,
                              verbose,
                              transient,
                              datay,
                              datax,
                              formula,
                              distribution,
                              studyDesign_name,
                              nNeighbours,
                              save_hmsc_path){
  
  message("-------------- Initialisation of python for Hmsc --------------")
  
  dir.create(save_hmsc_path)
  
  python <- file.path("/home/marbec/Bureau/hmsc/hmsc-hpc/examples/basic_example", "hmsc-venv", "bin", "python")  # hmsc-venv for Linux and macOS
  
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL=3)  # reduce debug output from tensorflow
  system2(python, "-c \"import tensorflow as tf; print(tf.constant(1))\"")
  system2(python, "-c \"import hmsc\"")

  message("-------------- Create cross validation dataset --------------")

  which_sp <- sapply(1:length(species_name), function(j) { 
    
    test <- datay[,species_name[j]]
    colnames(test) <- "biomass"
    nrow(test |> 
           dplyr::group_by(biomass) |> 
           dplyr::summarise(n = dplyr::n()))
    
  })
  info <- datay[colnames(datay) %in% c("survey_id", "latitude", "longitude", "site_code", "realm")]
  datay <- datay[species_name][which(which_sp > 1)]
  datay <- cbind(info, datay)

  cv <- scv_function(dats = datay,
                     n.folds = 5)
  
  path_hmsc <- paste0(save_hmsc_path, "hmsc_", nSamples, "_samples_", thin, "_thin_", nChains, "_chains_", distribution, "_distribution_", length(which(which_sp > 1)), "_species/")
  dir.create(path_hmsc)
  
  saveRDS(cv, paste0(path_hmsc, "cv.rds"))
  
  for(i in 1:length(cv)) {
    
    path_cv <- paste0(path_hmsc, "cv", i, "/")
    dir.create(path_cv)
    
    datay_cvi <- cv[[i]]$train
    datax_cvi <- datax |> 
      dplyr::filter(survey_id %in% datay_cvi$survey_id) |> 
      dplyr::as_tibble() |> 
      # noise_function(avoid = c("survey_id", "effectiveness", "latitude", "longitude", "site_code", "realm"),
      #                limit = 6,
      #                size = 0.01) |> 
      as.data.frame()
    datax_cvi_test <- datax |> 
      dplyr::filter(survey_id %in% cv[[i]]$test$survey_id) |> 
      dplyr::as_tibble() |> 
      # noise_function(avoid = c("survey_id", "effectiveness", "latitude", "longitude", "site_code", "realm"),
      #                limit = 6,
      #                size = 0.01) |> 
      as.data.frame()
    
    datay_cvi <- datay_cvi[!colnames(datay_cvi) %in% c("survey_id", "latitude", "longitude", "site_code", "realm")]

    datay_cvi <- apply(datay_cvi, 2, as.numeric)

    message(paste0("-------------- Initialisation of Hmsc model for cv ", i, " on ", length(cv), " --------------"))
    
    init_file_path <- paste0(path_cv, "init_file.rds")
    post_file_path <- paste0(path_cv, "post_file.rds")
    
    python_cmd_args <- paste("-m hmsc.run_gibbs_sampler",
                             "--input", shQuote(init_file_path),
                             "--output", shQuote(post_file_path),
                             "--samples", nSamples,
                             "--transient", transient,
                             "--thin", thin,
                             "--verbose", verbose)
    
    cat(paste(shQuote(python), python_cmd_args), "\n")
    
    if(is.null(studyDesign_name)){
      
      m <- Hmsc::Hmsc(Y = datay_cvi,
                      XFormula = formula,
                      XData = datax_cvi[!colnames(datax_cvi) %in% c("survey_id", "latitude", "longitude", "site_code", "realm")],
                      distr = distribution)
      
    }else{
      
      ##### Set study design #####
      
      # Create an empty dataframe with the name of the parameters of the model
      studyDesign_list <- list()
      studyDesign_test_list <- list()
      
      for (j in 1:length(studyDesign_name)) {
        
        studyDesign_list[[studyDesign_name[j]]] <- rep(NA, nrow(datax_cvi))
        
      }
      
      for (j in 1:length(studyDesign_name)) {
        
        studyDesign_test_list[[studyDesign_name[j]]] <- rep(NA, nrow(datax_cvi_test))
        
      }
      
      studyDesign_df <- as.data.frame(studyDesign_list)
      studyDesign_df_test <- as.data.frame(studyDesign_test_list)
      
      if("spatial" %in% studyDesign_name){
        
        # (1) add noise to coordinates for them to differ sltitly
        datax_cvi <- datax_cvi |>
          dplyr::rowwise() |>
          dplyr::mutate(latitude = latitude + runif(1, -0.0001, 0.0001),
                        longitude = longitude + runif(1, -0.0001, 0.0001)) |>
          as.data.frame()

        rownames(datax_cvi) <- 1:nrow(datax_cvi)
        rownames(datax_cvi_test) <- 1:nrow(datax_cvi_test)
        
        # (2) create a matrix with coordinates : xycoords is a matrix with 2 columns "x-coordinate","y-coordinate" and row names with survey_id
        xycoords <- datax_cvi |>
          dplyr::select(longitude, latitude) |> 
          as.data.frame()
        rownames(xycoords) <- 1:nrow(xycoords)
        xycoords_test <- datax_cvi_test |>
          dplyr::select(longitude, latitude) |> 
          dplyr::rowwise() |>
          dplyr::mutate(latitude = latitude + runif(1, -0.0001, 0.0001),
                        longitude = longitude + runif(1, -0.0001, 0.0001)) |>
          as.data.frame()
         rownames(xycoords_test) <- rownames(1:nrow(xycoords_test))
        
        # (3) add spatial random effect and study design (rows in Y)
        rL.nngp <- Hmsc::HmscRandomLevel(sData = xycoords,
                                         sMethod = "NNGP",
                                         nNeighbours = nNeighbours,
                                         longlat = TRUE) #Should be True but doesn't work after
        rL.nngp <- Hmsc::setPriors(rL.nngp, nfMin = 1, nfMax = 1)
        
        rL.nngp.test <- Hmsc::HmscRandomLevel(sData = xycoords_test,
                                              sMethod = "NNGP",
                                              nNeighbours = nNeighbours,
                                              longlat = TRUE) #Should be True but doesn't work after
        rL.nngp.test <- Hmsc::setPriors(rL.nngp.test, nfMin = 1, nfMax = 1)
        
        studyDesign_df[,"spatial"] <- as.factor(row.names(datax_cvi))
        studyDesign_df_test[,"spatial"] <- as.factor(row.names(datax_cvi_test))

      }
      
      if("association" %in% studyDesign_name){
        
        association <- as.factor(rownames(datax_cvi))
        association_test <- as.factor(rownames(datax_cvi_test))
        
        studyDesign_df$association <- association
        studyDesign_df_test$association <- association_test
        
      }
      
      
      ##### Set random levels #####
      
      # Create an empty list with the name of the parameters of the model
      random_levels_list <- list()
      random_levels_list_test <- list()
      
      for (j in 1:length(studyDesign_name)) {
        
        random_levels_list[[studyDesign_name[j]]] <- NA
        
      }
      
      for (j in 1:length(studyDesign_name)) {
        
        random_levels_list_test[[studyDesign_name[j]]] <- NA
        
      }
      
      if("spatial" %in% studyDesign_name){
        
        random_levels_list$spatial <- rL.nngp
        random_levels_list_test$spatial <- rL.nngp.test
        
      }
      
      if("association" %in% studyDesign_name){
        
        rL_asso <- Hmsc::HmscRandomLevel(units = studyDesign_df$association)
        rL_asso_test <- Hmsc::HmscRandomLevel(units = studyDesign_df_test$association)
        
        random_levels_list$association <- rL_asso
        random_levels_list_test$association <- rL_asso_test
        
      }
      
      m <- Hmsc::Hmsc(Y = datay_cvi,
                      XFormula = formula,
                      XData = datax_cvi[!colnames(datax_cvi) %in% c("survey_id", "latitude", "longitude", "site_code", "realm")],
                      distr = distribution,
                      studyDesign = studyDesign_df,
                      ranLevels = random_levels_list)
      
    }
  
  saveRDS(m, file = paste0(path_cv, "m_cv", i, ".rds"))
  
  init_obj <- Hmsc::sampleMcmc(m, 
                               samples = nSamples, 
                               thin = thin,
                               transient = transient, 
                               nChains = nChains,
                               verbose = verbose, 
                               engine = "HPC")
  
  saveRDS(jsonify::to_json(init_obj), file = init_file_path)
  
  message("-------------- Run Hmsc with python --------------")
  
  system2(python, python_cmd_args)
  
  message("-------------- Make predictions --------------")
  
  importFromHPC <- jsonify::from_json(readRDS(post_file_path)[[1]])
  postList <- importFromHPC[1:nChains]
  cat(sprintf("fitting time %.1f sec\n", importFromHPC[[nChains+1]]))
  
  m <- readRDS(paste0(path_cv, "m_cv", i, ".rds"))
  
  transient2 <- nSamples * thin
  fittf <- Hmsc::importPosteriorFromHPC(m, postList, nSamples, thin, transient2)
  
  if(is.null(studyDesign_name)) {
    
    predictions <- predict(fittf, 
                           XData = datax_cvi_test, 
                           expected = TRUE)
    
  }else{
    
    predictions <- predict(fittf,
                           XData = datax_cvi_test[!colnames(datax_cvi_test) %in% c("survey_id", "latitude", "longitude", "site_code", "realm")], 
                           studyDesign = studyDesign_df_test, 
                           ranLevels = random_levels_list_test, 
                           expected = TRUE)
    
  }
  

  predictions_mean <- apply(simplify2array(predictions), c(1, 2), FUN = mean, na.rm = TRUE)
  
  info_test <- cv[[i]]$test[colnames(cv[[i]]$test) %in% c("survey_id", "latitude", "longitude", "realm")]
  
  predictions_mean <- cbind(info_test, predictions_mean) |> 
    as.data.frame()
  obs_cvi <- cbind(info_test, apply(cv[[i]]$test[!colnames(cv[[i]]$test) %in% c("survey_id", "latitude", "longitude", "realm")], 2, as.numeric)) |> 
    as.data.frame()

  prd_obs <- list(predictions_mean, obs_cvi)
  names(prd_obs) <- c("predictions", "observations")
  
  saveRDS(prd_obs, file = paste0(path_cv, "pred_obs.rds"))
  
  }
  
}
