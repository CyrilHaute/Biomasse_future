
species_biomass <- list.files("results/biomass_baseline_future_hist8", full.names = TRUE)


species_var_imp <- pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  
  baseline <- sp |> 
    dplyr::filter(year == 2024)
  
  sp_cov <- baseline |> 
    dplyr::filter(year == "2024") |>
    dplyr::select(-c(year, scenario)) |>
    tidyr::drop_na() |>
    dplyr::as_tibble()
  
  ### FITTING MODELS 
  
  # fit the random forests
  
  model_fit <- ranger::ranger(x = sp_cov[!colnames(sp_cov) %in% c("survey_id", "site_code", "biomass", "x", "y", "realm")],
                              y = unlist(sp_cov[colnames(sp_cov) %in% "biomass"]),
                              num.trees = 100,
                              mtry = length(colnames(sp_cov)[!colnames(sp_cov) %in% c("survey_id", "site_code", "biomass")]) / 3,
                              importance = "permutation")

  var_imp <- data.frame(imp = model_fit$variable.importance,
                        covariates = unique(names(c(model_fit$variable.importance))),
                        r2 = round(model_fit$r.squared, 2),
                        species = stringr::str_replace_all(gsub(".Rdata", "", gsub(pattern = "results/biomass_baseline_future_hist8/", "", species_biomass[i])), "_", " "))

}, mc.cores = parallel::detectCores() - 1)
species_var_imp_bind <- do.call(rbind, species_var_imp)

mean(species_var_imp_bind$r2)
sd(species_var_imp_bind$r2)

species_var_imp_bind <- species_var_imp_bind |> 
  dplyr::group_by(covariates) |> 
  dplyr::summarise(med_imp = median(imp),
                   q_05_imp = quantile(imp, probs = 0.05),
                   q_95_imp = quantile(imp, probs = 0.95),
                   sd_imp = sd(imp)) |> 
  dplyr::mutate(var_type = dplyr::case_when(covariates %in% c("gdp", "gravity", "hdi", "marine_ecosystem_dependency",
                                                              "neartt", "ngo", "protection_status") ~ "HUM",
                                            covariates %in% c("Lagoon", "Patch_Reefs_500m", "Plateau_500m", "Reef_Crest_500m",
                                                              "Reef_Flat", "Reef_Slope", "Terrestrial_Reef_Flat_500m", "depth",
                                                              "reef_extent") ~ "HAB",
                                            covariates %in% c("chl", "ph", "sss", "sst") ~ "ENV"))


library(ggplot2)

delta_imp_var <- species_var_imp_bind |> 
  dplyr::mutate(covariates = forcats::fct_reorder(covariates, med_imp)) |> 
  ggplot(aes(color = var_type)) +
  geom_pointrange(aes(x = covariates, y = med_imp, ymin = q_05_imp , ymax = q_95_imp)) +
  theme_bw() +
  coord_flip() +
  labs(y = "Covariates importance (MSE)", x = "", color = "")
  
ggsave("figures/delta_imp_var.png", delta_imp_var)
