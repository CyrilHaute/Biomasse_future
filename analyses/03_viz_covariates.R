######################################################################################################
####################################    Plot future covariates   #####################################
######################################################################################################

source("R/02_viz_covariates_function.R")
source("R/03_environmental_window_function.R")

list_extracted_cov_hist <- list.files("data/extracted/covariates/environmental_historical", full.names = TRUE)
list_extracted_cov <- list.files("data/extracted/covariates/environmental", full.names = TRUE)

# load("data/extracted/rls_covariates_train.RData")

gdp <- list.files("data/extracted/covariates/social", full.names = TRUE)
gdp <- gdp[which(grepl(pattern = "gdp", gdp))]
load("data/extracted/covariates/social/gravity.Rdata")

read_var_gdp <- lapply(1:length(gdp), function(i) {
  
  load(gdp[i])
  assign(paste0("var_", i), data)
  
})
read_var_gdp_bind <- do.call(rbind, read_var_gdp)


###################### hist future ######################

load("data/extracted/rls_covariates_train_hist.RData")

rls_covariates_train_hist$survey_id <- as.integer(rls_covariates_train_hist$survey_id )

### SST historical data (2005-2014)
sst_hist <- list_extracted_cov_hist[which(grepl("tos", list_extracted_cov_hist) == TRUE)]

read_var_sst_hist <- lapply(1:length(sst_hist), function(i) {
  
  load(sst_hist[i])
  assign(paste0("var_", i), data)
  
})
read_var_sst_hist_bind <- do.call(rbind, read_var_sst_hist) |> 
  dplyr::select(-c(scenario, variable))


### SST future data (2015-2100)
sst <- list_extracted_cov[which(grepl("to", list_extracted_cov) == TRUE)]

read_var_sst <- lapply(1:length(sst), function(i) {
  
  load(sst[i])
  assign(paste0("var_", i), data)
  
})
read_var_sst_bind <- do.call(rbind, read_var_sst)

sst_hist_future <- read_var_sst_hist_bind |> 
  dplyr::inner_join(read_var_sst_bind) |> 
  dplyr::relocate(model, .after = variable)

year <- 2005:2100

# Environmental window
sst_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = sst_hist_future |> 
                                  dplyr::filter(survey_id %in% rls_covariates_train_hist$survey_id),
                                info = c("survey_id", "longitude", "latitude", "scenario", "model"),
                                varname = "tos")
  
}, mc.cores = parallel::detectCores() - 1)
sst_window_info <- sst_window[[1]][colnames(sst_window[[1]]) %in% c("survey_id", "longitude", "latitude", "scenario", "model")]
sst_window_sst <- lapply(sst_window, '[', 6)
sst_window_sst <- do.call(cbind, sst_window_sst)
sst_window <- cbind(sst_window_info, sst_window_sst)

colnames(sst_window)[which(grepl("tos", colnames(sst_window)))] <- gsub("tos_", "", colnames(sst_window)[which(grepl("tos", colnames(sst_window)))])

sst <- plot_data(data = sst_window |> 
                   dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                             scenario == "ssp245" ~ "SSP2-4.5",
                                                             scenario == "ssp370" ~ "SSP3-7.0",
                                                             scenario == "ssp585" ~ "SSP5-8.5")),
                 var_name = "Sea Surface Temperature (T°C)",
                 color = "scenario",
                 year = as.character(seq(from = 2005, to = 2014, by = 1)),
                 legend = TRUE,
                 breaks = c(2005, 2025, 2050, 2070, 2100))


### pH historical data (2005-2014)
ph_hist <- list_extracted_cov_hist[which(grepl("ph", list_extracted_cov_hist) == TRUE)]

read_var_ph_hist <- lapply(1:length(ph_hist), function(i) {
  
  load(ph_hist[i])
  assign(paste0("var_", i), data)
  
})
read_var_ph_hist_bind <- do.call(rbind, read_var_ph_hist) |> 
  dplyr::select(-c(scenario, variable))


### ph future data (2015-2100)
ph <- list_extracted_cov[which(grepl("ph", list_extracted_cov) == TRUE)]

read_var_ph <- lapply(1:length(ph), function(i) {
  
  load(ph[i])
  assign(paste0("var_", i), data)
  
})
read_var_ph_bind <- do.call(rbind, read_var_ph)

ph_hist_future <- read_var_ph_hist_bind |> 
  dplyr::inner_join(read_var_ph_bind) |> 
  dplyr::relocate(model, .after = variable)

year <- 2005:2100

# Environmental window
ph_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = ph_hist_future |> 
                                  dplyr::filter(survey_id %in% rls_covariates_train_hist$survey_id),
                                info = c("survey_id", "longitude", "latitude", "scenario", "model"),
                                varname = "ph")
  
}, mc.cores = parallel::detectCores() - 1)
ph_window_info <- ph_window[[1]][colnames(ph_window[[1]]) %in% c("survey_id", "longitude", "latitude", "scenario", "model")]
ph_window_ph <- lapply(ph_window, '[', 6)
ph_window_ph <- do.call(cbind, ph_window_ph)
ph_window <- cbind(ph_window_info, ph_window_ph)

colnames(ph_window)[which(grepl("ph", colnames(ph_window)))] <- gsub("ph_", "", colnames(ph_window)[which(grepl("ph", colnames(ph_window)))])

ph <- plot_data(data = ph_window |> 
                   dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                             scenario == "ssp245" ~ "SSP2-4.5",
                                                             scenario == "ssp370" ~ "SSP3-7.0",
                                                             scenario == "ssp585" ~ "SSP5-8.5")),
                 var_name = "pH",
                 color = "scenario",
                 year = as.character(seq(from = 2005, to = 2014, by = 1)),
                 legend = TRUE,
                 breaks = c(2005, 2025, 2050, 2070, 2100))


### SSS historical data (2005-2014)
sss_hist <- list_extracted_cov_hist[which(grepl("so", list_extracted_cov_hist) == TRUE)]

read_var_sss_hist <- lapply(1:length(sss_hist), function(i) {
  
  load(sss_hist[i])
  assign(paste0("var_", i), data)
  
})
read_var_sss_hist_bind <- do.call(rbind, read_var_sss_hist) |> 
  dplyr::select(-c(scenario, variable))


### SSS future data (2015-2100)
sss <- list_extracted_cov[which(grepl("so", list_extracted_cov) == TRUE)]

read_var_sss <- lapply(1:length(sss), function(i) {
  
  load(sss[i])
  assign(paste0("var_", i), data)
  
})
read_var_sss_bind <- do.call(rbind, read_var_sss)

sss_hist_future <- read_var_sss_hist_bind |> 
  dplyr::inner_join(read_var_sss_bind) |> 
  dplyr::relocate(model, .after = variable)

year <- 2005:2100

# Environmental window
sss_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = sss_hist_future |> 
                                  dplyr::filter(survey_id %in% rls_covariates_train_hist$survey_id),
                                info = c("survey_id", "longitude", "latitude", "scenario", "model"),
                                varname = "sss")
  
}, mc.cores = parallel::detectCores() - 1)
sss_window_info <- sss_window[[1]][colnames(sss_window[[1]]) %in% c("survey_id", "longitude", "latitude", "scenario", "model")]
sss_window_sss <- lapply(sss_window, '[', 6)
sss_window_sss <- do.call(cbind, sss_window_sss)
sss_window <- cbind(sss_window_info, sss_window_sss)

colnames(sss_window)[which(grepl("sss", colnames(sss_window)))] <- gsub("sss_", "", colnames(sss_window)[which(grepl("sss", colnames(sss_window)))])

sss <- plot_data(data = sss_window |> 
                  dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                            scenario == "ssp245" ~ "SSP2-4.5",
                                                            scenario == "ssp370" ~ "SSP3-7.0",
                                                            scenario == "ssp585" ~ "SSP5-8.5")),
                var_name = "Sea Surface Salinity (PSU)",
                color = "scenario",
                year = as.character(seq(from = 2005, to = 2014, by = 1)),
                legend = TRUE,
                breaks = c(2005, 2025, 2050, 2070, 2100))


### CHL historical data (2005-2014)
chl_hist <- list_extracted_cov_hist[which(grepl("chl", list_extracted_cov_hist) == TRUE)]

read_var_chl_hist <- lapply(1:length(chl_hist), function(i) {
  
  load(chl_hist[i])
  assign(paste0("var_", i), data)
  
})
read_var_chl_hist_bind <- do.call(rbind, read_var_chl_hist) |> 
  dplyr::select(-c(scenario, variable))


### CHL future data (2015-2100)
chl <- list_extracted_cov[which(grepl("chl", list_extracted_cov) == TRUE)]

read_var_chl <- lapply(1:length(chl), function(i) {
  
  load(chl[i])
  assign(paste0("var_", i), data)
  
})
read_var_chl_bind <- do.call(rbind, read_var_chl)

chl_hist_future <- read_var_chl_hist_bind |> 
  dplyr::inner_join(read_var_chl_bind) |> 
  dplyr::relocate(model, .after = variable)

year <- 2005:2100

# Environmental window
chl_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = chl_hist_future |> 
                                  dplyr::filter(survey_id %in% rls_covariates_train_hist$survey_id),
                                info = c("survey_id", "longitude", "latitude", "scenario", "model"),
                                varname = "chl")
  
}, mc.cores = parallel::detectCores() - 1)
chl_window_info <- chl_window[[1]][colnames(chl_window[[1]]) %in% c("survey_id", "longitude", "latitude", "scenario", "model")]
chl_window_chl <- lapply(chl_window, '[', 6)
chl_window_chl <- do.call(cbind, chl_window_chl)
chl_window <- cbind(chl_window_info, chl_window_chl)

colnames(chl_window)[which(grepl("chl", colnames(chl_window)))] <- gsub("chl_", "", colnames(chl_window)[which(grepl("chl", colnames(chl_window)))])

chl <- plot_data(data = chl_window |> 
                 dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                             scenario == "ssp245" ~ "SSP2-4.5",
                                                             scenario == "ssp370" ~ "SSP3-7.0",
                                                             scenario == "ssp585" ~ "SSP5-8.5")),
                 var_name = "Sea Water Chlorophyll (kg.m-3)",
                 color = "scenario",
                 year = as.character(seq(from = 2005, to = 2014, by = 1)),
                 legend = TRUE,
                 breaks = c(2005, 2025, 2050, 2070, 2100))


gdp <- plot_data(data = read_var_gdp_bind |> 
                   dplyr::filter(scenario %in% c("ssp1", "ssp2", "ssp3", "ssp5"),
                                 survey_id %in% rls_covariates_train_hist$survey_id) |> 
                   dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp1" ~ "SSP1-2.6",
                                                             scenario == "ssp2" ~ "SSP2-4.5",
                                                             scenario == "ssp3" ~ "SSP3-7.0",
                                                             scenario == "ssp5" ~ "SSP5-8.5")) |> 
                   dplyr::select(-c(x, y, variable)),
                 var_name = "Gross Domestic Product ($)",
                 color = "scenario",
                 year = as.character(seq(from = 2010, to = 2100, by = 10)),
                 legend = TRUE,
                 breaks = c(2010, 2050, 2100))

library(ggplot2)

gravity_plot <- gravity_scenario_baseline_bind |> 
  dplyr::group_by(scenario, year) |>
  dplyr::summarise(mean_value = mean(gravity),
                   se = sd(gravity)  / sqrt(dplyr::n()),
                   lower = mean_value - qt(0.975, df = dplyr::n() - 1) * se,
                   upper = mean_value + qt(0.975, df = dplyr::n() - 1) * se) |>
  ggplot(aes(x = year, y = mean_value, group = scenario, color = scenario)) + 
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
  scale_color_manual(values = c("SSP1" = RColorBrewer::brewer.pal(n = 9, name = "Set1")[3],
                                "SSP2" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[1],
                                "SSP3" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[2],
                                "SSP5" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[3])) + 
  labs(x = "", y = "", title = "Human gravity (population count.min-2)") +
  scale_x_discrete(breaks = c(2020, 2050, 2100)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, size = 0.1)

cov_evol_plot <- patchwork::wrap_plots(sst, ph, chl, sss, gdp, gravity_plot) +
  patchwork::plot_layout(guides = "collect", ncol = 2)

ggsave(cov_evol_plot, file = "figures/cov_evol_plot.png", height = 9, width = 9)


# load("data/raw/covariates/social/gravity.RData")
# load("data/raw/00_rls_surveys.Rdata")
# 
# rls_surveys$survey_id <- as.numeric(rls_surveys$survey_id)
# 
# gravity_eva <- gravity |> 
#   dplyr::select(survey_id, gravity) |> 
#   dplyr::inner_join(rls_surveys |> 
#                       dplyr::select(survey_id, latitude, longitude)) |> 
#   dplyr::select(-survey_id) |> 
#   unique() |> 
#   dplyr::rename(gravity_eva = gravity)
# 
# baseline <- gravity_scenario_baseline_bind |> 
#   dplyr::filter(year == "2020") |> 
#   dplyr::select(-scenario) |> 
#   unique() |> 
#   dplyr::rename(gravity_cyril = gravity)
# 
# eva_vs_baseline <- gravity_eva |> 
#   dplyr::inner_join(baseline, by = c("latitude", "longitude"))
# 
# plot_gravity_eva_vs_cyril <- eva_vs_baseline |> 
#   ggplot(aes(x = log10(gravity_eva + 1), y = log10(gravity_cyril + 1))) + 
#   geom_point() + 
#   geom_smooth(method = "lm") + 
#   labs(title = paste0("Gravity baseline Eva vs Cyril : r² = ", round(summary(lm(log10(gravity_eva + 1) ~ log10(gravity_cyril + 1), data = eva_vs_baseline))$r.squared, 2)))
# ggsave(plot_eva_vs_cyril, file = "figures/plot_gravity_eva_vs_cyril.png")
