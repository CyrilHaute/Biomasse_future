source("R/future_pred_scale_function.R")
source("R/06_score_function.R")
source("R/delta_plot_function.R")

load("data/raw/00_rls_surveys.Rdata")
load("results/decile_save.Rdata")
load("results/performance_bind.Rdata")

decile_save[30,] <- c(0, 0)
decile_save <- decile_save[order(decile_save$biomass_class),]

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")

color_scale <- palette.colors()
color_scale <- as.character(c(color_scale[4], color_scale[3], color_scale[5], color_scale[7]))

load("results/biomass_class_future_rf_hist_migration5_full.Rdata")

data <- biomass_class_future_rf_hist_migration4_full
unit <- "kg"
time <- as.character(c("2024":"2075"))

sc_i <- lapply(data, '[[', 1)

sc_i <- do.call(rbind, sc_i)

sc_i <- sc_i[,c("survey_id", "species_name", "biomass" , "validation_predict", colnames(sc_i)[which(grepl(scenarii[1], colnames(sc_i)))])]

colnames(sc_i)[which(grepl(scenarii[1], colnames(sc_i)))] <- gsub(pattern = paste0("_", scenarii[1]), replacement = "", colnames(sc_i)[which(grepl(scenarii[1], colnames(sc_i)))])

colnames(sc_i)[colnames(sc_i) == "validation_predict"] <- "2024"

# Étape 1 : R² entre biomasse obs et prd à t0 (il y a t-il un biais dès le départ ?)

test <- pbmcapply::pbmclapply(1:length(unique(sc_i$species_name)), function(i) {
  
  sp_i <- sc_i |> 
    dplyr::filter(species_name == unique(sc_i$species_name)[i]) |> 
    dplyr::select(biomass, `2024`) |> 
    dplyr::mutate(delta = biomass - `2024`)
  
  model_i <- summary(lm(biomass ~ `2024`, data = sp_i))
  
  sp_i <- sp_i |> 
    dplyr::mutate(species_name = unique(sc_i$species_name)[i],
                  r2 = model_i$r.squared)
  
}, mc.cores = parallel::detectCores() - 1)
test_bind <- do.call(rbind, test)
hist(unique(test_bind$r2))
hist(test_bind$delta)
mean(test_bind$r2)
# r² moyen min.node.size = 5 = 0.94
# r² moyen min.node.size = 10 = 0.78
# r² moyen min.node.size = 25 = 0.47
# r² moyen min.node.size = 50 = 0.34


# Étape 2 : Distribution des variables entre t0 et t+1

species_biomass <- list.files("results/biomass_baseline_future_hist_migration4", full.names = TRUE)

load(species_biomass[1])

baseline <- sp |> 
  dplyr::filter(year == 2024)
baseline_plus_1 <- sp |> 
  dplyr::filter(year == 2025,
                survey_id %in% baseline$survey_id)

hist(baseline$sst)
summary(baseline$sst)
hist(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$sst)
summary(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$sst)

hist(baseline$gravity)
summary(baseline$gravity)
hist(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$gravity)
summary(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$gravity)

hist(baseline$chl)
summary(baseline$chl)
hist(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$chl)
summary(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$chl)

hist(baseline$ph)
summary(baseline$ph)
hist(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$ph)
summary(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$ph)

hist(baseline$sss)
summary(baseline$sss)
hist(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$sss)
summary(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$sss)

hist(baseline$gdp)
summary(baseline$gdp)
hist(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$gdp)
summary(baseline_plus_1[baseline_plus_1$scenario == "ssp585",]$gdp)

# Étape 3 : PDP à t0

species_biomass <- list.files("results/biomass_baseline_future_hist5", full.names = TRUE)

unlist(pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
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
  
  model_fit <- ranger::ranger(x = sp_cov[!colnames(sp_cov) %in% c("survey_id", "site_code", "biomass")],
                              y = unlist(sp_cov[colnames(sp_cov) %in% "biomass"]),
                              num.trees = 100,
                              mtry = length(colnames(sp_cov)[!colnames(sp_cov) %in% c("survey_id", "site_code", "biomass")]) / 3,
                              importance = "impurity")

  var_imp <- data.frame(delta = model_fit$variable.importance,
                        covariates = unique(names(c(model_fit$variable.importance))))
  
  library(ggplot2)
  
  var_imp <- var_imp |>
    dplyr::mutate(delta_pr = (delta * 100) / sum(delta))
  
  # delta_imp_var <- ggplot(var_imp) +
  #   geom_col(aes(x = reorder(covariates, delta_pr), y = delta_pr, fill = delta_pr)) +
  #   scale_fill_viridis_c(direction = -1) +
  #   theme_minimal() +
  #   coord_flip() +
  #   theme(axis.text.x = element_text(size = 12),
  #         axis.text.y = element_text(size = 12),
  #         axis.title = element_text(size = 14),
  #         strip.text.x = element_text(size = 12),
  #         strip.text.y = element_text(size = 12),
  #         legend.position = "none") + 
  #   labs(x = "",
  #        y = "Variable importance (%)",
  #        title = paste0("R² = ", round(model_fit$r.squared, 2)))  
  
  partial_dep_function <- function(model,
                                   covariates,
                                   data,
                                   reponse_name){
    
    partial_i <- pbmcapply::pbmclapply(1:length(covariates), function(j) {
      
      partial_dep <- pdp::partial(model,
                                  train = data,
                                  pred.var = c(covariates[j]),
                                  # grid.resolution = 100,
                                  # ice = TRUE
      ) |> 
        dplyr::mutate(species = i)
      
      colnames(partial_dep)[colnames(partial_dep) == "yhat"] <- reponse_name
      
      return(partial_dep)
      
    }, mc.cores = 1)
    
    names(partial_i) <- covariates
    
    return(partial_i)
    
  }
  
  select_cov <- var_imp$covariates[order(var_imp$delta_pr, decreasing = TRUE)]
  select_cov <- select_cov[1:10]
  
  partial_var <- partial_dep_function(model = model_fit,
                                      covariates = select_cov,
                                      data = sp_cov,
                                      reponse_name = "biomass")
  
 partial_plot_function <- function(partial_data,
                                   title){
   
   require(ggplot2)
   
   plot_all <- pbmcapply::pbmclapply(1:length(partial_data), function(i) {
     
     partial_data_i <- partial_data[[i]]
     
     colnames(partial_data_i) <- c("covariates", "response")
     
     plot_i <- partial_data_i |> 
       ggplot(aes(x = covariates, y = response)) + 
       geom_point(size = 1) +
       geom_line(stat = "smooth", linewidth = 1) +
       theme_bw() +
       theme(axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 13),
             axis.title = element_text(size = 13),
             legend.position = "bottom",
             legend.direction = "horizontal",
             legend.text = element_text(size = 13)) +
       labs(y = "", x = names(partial_data)[i], color = "")
     
   }, mc.cores = 1)
   
   if(length(plot_all) == 1){
     
     plot_all
     
   }else{
     
     patchwork::wrap_plots(plot_all) + 
       patchwork::plot_layout(guides = "collect", ncol = 2) +
       patchwork::plot_annotation(title = title) &
       theme(legend.position = "bottom",
             legend.direction = "horizontal")
     
   }
   
 }
  
 partial_plot <- partial_plot_function(partial_data = partial_var,
                                       title = gsub("results/biomass_baseline_future_hist5/" , "", species_biomass[i]))
 
 # dir.create("figures/pdp_baseline")
 dir.create("figures/pdp_baseline")
 ggsave(partial_plot, file = paste0("figures/pdp_baseline/", gsub(".Rdata", "", gsub("results/biomass_baseline_future_hist5/" , "", species_biomass[i])), ".png"), width = 7, height = 10)
  
}, mc.cores = parallel::detectCores() - 1))


# évolution var

species_biomass <- list.files("results/biomass_baseline_future_hist5", full.names = TRUE)
dir.create("figures/cov_evolution_species/")

library(ggplot2)

evol_var <- pbmcapply::pbmclapply(1:length(species_biomass), function(i) {
  
  load(species_biomass[i])
  sp_i <- sp
  
  baseline_ssp126 <- sp_i |> 
    dplyr::filter(year == "2024") |> 
    dplyr::mutate(scenario = "ssp126")
  baseline_ssp245 <- sp_i |> 
    dplyr::filter(year == "2024") |> 
    dplyr::mutate(scenario = "ssp245")
  baseline_ssp370 <- sp_i |> 
    dplyr::filter(year == "2024") |> 
    dplyr::mutate(scenario = "ssp370")
  baseline_ssp585 <- sp_i |> 
    dplyr::filter(year == "2024") |> 
    dplyr::mutate(scenario = "ssp585")
  baseline <- baseline_ssp126 |> 
    dplyr::full_join(baseline_ssp245) |> 
    dplyr::full_join(baseline_ssp370) |> 
    dplyr::full_join(baseline_ssp585)
  
  future <- sp_i |> 
    dplyr::filter(year != "2024")
  
  baseline_future <- baseline |> 
    dplyr::full_join(future)
  
  baseline_future$year <- as.numeric(baseline_future$year)
  
  var <- c("chl", "ph", "sss", "sst", "gravity", "gdp")
  
  plot_var <- lapply(1:length(var), function(j) {
    
    var_j <- baseline_future |> 
      dplyr::select(var[j], scenario, year)
    
    data_pivot_scenario <- var_j |> 
      dplyr::group_by(scenario, year) |> 
      dplyr::summarise(mean_value = mean(.data[[var[j]]]),
                       se = sd(.data[[var[j]]])  / sqrt(dplyr::n()),
                       lower = mean_value - qt(0.975, df = dplyr::n() - 1) * se,
                       upper = mean_value + qt(0.975, df = dplyr::n() - 1) * se
                       )
    
    plot_data <- ggplot(data = data_pivot_scenario, aes(x = year, y = mean_value, group = scenario, color = scenario)) + 
      theme_bw() +
      theme(title = element_text(size = 10),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.text = element_text(size = 10),
            strip.text.x = element_text(size = 8),
            strip.text.y = element_text(size = 20),
            legend.position = "none") +
      geom_line(size = 1) +
      scale_color_manual(values = c("ssp126" = RColorBrewer::brewer.pal(n = 9, name = "Set1")[3],
                                    "ssp245" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[1],
                                    "ssp370" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[2],
                                    "ssp585" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[3])) +
      labs(y = var[j])
    
  })
  plot_var_bind <- patchwork::wrap_plots(plot_var, ncol = 2) &
    labs(title = gsub(".Rdata", "", gsub("results/biomass_baseline_future_hist5/", "", species_biomass[i])))
  
  ggsave(plot_var_bind, file = paste0("figures/cov_evolution_species/", gsub(".Rdata", "", gsub("results/biomass_baseline_future_hist5/", "", species_biomass[i])), ".png"))
  
}, mc.cores = parallel::detectCores() - 1)
  