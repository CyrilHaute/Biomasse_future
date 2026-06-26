source("R/hist_future_var_function.R")
source("R/03_environmental_window_function.R")
source("R/02_viz_covariates_function.R")

list_extracted_cov_hist <- list.files("data/extracted/covariates/environmental_historical", full.names = TRUE)
list_extracted_cov <- list.files("data/extracted/covariates/environmental", full.names = TRUE)

load("data/extracted/rls_covariates_train.RData")

rls_covariates_train$survey_id <- as.integer(rls_covariates_train$survey_id )

year <- 2005:2100

chl_hist_future <- hist_future_var_function(hist_var = list_extracted_cov_hist[which(grepl("chl", list_extracted_cov_hist) == TRUE)],
                                            future_var = list_extracted_cov[which(grepl("chl", list_extracted_cov) == TRUE)]) |> 
  dplyr::filter(survey_id %in% rls_covariates_train$survey_id)

# Environmental window
chl_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = chl_hist_future,
                                info = c("survey_id", "longitude", "latitude", "scenario", "model"),
                                varname = "chl")
  
}, mc.cores = parallel::detectCores() - 1)
chl_window_info <- chl_window[[1]][colnames(chl_window[[1]]) %in% c("survey_id", "longitude", "latitude", "scenario", "model")]
chl_window_chl <- lapply(chl_window, '[', 6)
chl_window_chl <- do.call(cbind, chl_window_chl)
chl_window <- cbind(chl_window_info, chl_window_chl)

# colnames(chl_window)[!colnames(chl_window) %in% c("survey_id", "longitude", "latitude", "scenario", "model")] <- gsub("chl_", "", colnames(chl_window)[!colnames(chl_window) %in% c("survey_id", "longitude", "latitude", "scenario", "model")])

chl <- plot_data(data = chl_window |> 
                   dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                             scenario == "ssp245" ~ "SSP2-4.5",
                                                             scenario == "ssp370" ~ "SSP3-7.0",
                                                             scenario == "ssp585" ~ "SSP5-8.5")),
                 var_name = "CHL",
                 color = "scenario",
                 year = as.character(seq(from = 2005, to = 2100, by = 1)),
                 legend = TRUE,
                 breaks = c(2005, 2014, 2025, 2050, 2070, 2100))

ph_hist_future <- hist_future_var_function(hist_var = list_extracted_cov_hist[which(grepl("ph", list_extracted_cov_hist) == TRUE)],
                                           future_var = list_extracted_cov[which(grepl("ph", list_extracted_cov) == TRUE)]) |> 
  dplyr::filter(survey_id %in% rls_covariates_train$survey_id)

ph_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = ph_hist_future,
                                info = c("survey_id", "longitude", "latitude", "scenario", "model"),
                                varname = "ph")
  
}, mc.cores = parallel::detectCores() - 1)
ph_window_info <- ph_window[[1]][colnames(ph_window[[1]]) %in% c("survey_id", "longitude", "latitude", "scenario", "model")]
ph_window_ph <- lapply(ph_window, '[', 6)
ph_window_ph <- do.call(cbind, ph_window_ph)
ph_window <- cbind(ph_window_info, ph_window_ph)

# colnames(ph_window)[!colnames(ph_window) %in% c("survey_id", "longitude", "latitude", "scenario", "model")] <- gsub("ph_", "", colnames(ph_window)[!colnames(ph_window) %in% c("survey_id", "longitude", "latitude", "scenario", "model")])

ph <- plot_data(data = ph_window |> 
                  dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                            scenario == "ssp245" ~ "SSP2-4.5",
                                                            scenario == "ssp370" ~ "SSP3-7.0",
                                                            scenario == "ssp585" ~ "SSP5-8.5")),
                var_name = "pH",
                color = "scenario",
                year = as.character(seq(from = 2005, to = 2100, by = 1)),
                legend = TRUE,
                breaks = c(2005, 2014, 2025, 2050, 2070, 2100))


sss_hist_future <- hist_future_var_function(hist_var = list_extracted_cov_hist[which(grepl("so", list_extracted_cov_hist) == TRUE)],
                                            future_var = list_extracted_cov[which(grepl("so", list_extracted_cov) == TRUE)]) |> 
  dplyr::filter(survey_id %in% rls_covariates_train$survey_id)

sss_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = sss_hist_future,
                                info = c("survey_id", "longitude", "latitude", "scenario", "model"),
                                varname = "so")
  
}, mc.cores = parallel::detectCores() - 1)
sss_window_info <- sss_window[[1]][colnames(sss_window[[1]]) %in% c("survey_id", "longitude", "latitude", "scenario", "model")]
sss_window_sss <- lapply(sss_window, '[', 6)
sss_window_sss <- do.call(cbind, sss_window_sss)
sss_window <- cbind(sss_window_info, sss_window_sss)

# colnames(sss_window)[!colnames(sss_window) %in% c("survey_id", "longitude", "latitude", "scenario", "model")] <- gsub("so_", "", colnames(sss_window)[!colnames(sss_window) %in% c("survey_id", "longitude", "latitude", "scenario", "model")])

sss <- plot_data(data = sss_window |> 
                  dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                            scenario == "ssp245" ~ "SSP2-4.5",
                                                            scenario == "ssp370" ~ "SSP3-7.0",
                                                            scenario == "ssp585" ~ "SSP5-8.5")),
                var_name = "SSS",
                color = "scenario",
                year = as.character(seq(from = 2005, to = 2100, by = 1)),
                legend = TRUE,
                breaks = c(2005, 2014, 2025, 2050, 2070, 2100))


sst_hist_future <- hist_future_var_function(hist_var = list_extracted_cov_hist[which(grepl("tos", list_extracted_cov_hist) == TRUE)],
                                            future_var = list_extracted_cov[which(grepl("to", list_extracted_cov) == TRUE)]) |> 
  dplyr::filter(survey_id %in% rls_covariates_train$survey_id)

sst_window <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = sst_hist_future,
                                info = c("survey_id", "longitude", "latitude", "scenario", "model"),
                                varname = "tos")
  
}, mc.cores = parallel::detectCores() - 1)
sst_window_info <- sst_window[[1]][colnames(sst_window[[1]]) %in% c("survey_id", "longitude", "latitude", "scenario", "model")]
sst_window_sst <- lapply(sst_window, '[', 6)
sst_window_sst <- do.call(cbind, sst_window_sst)
sst_window <- cbind(sst_window_info, sst_window_sst)

# colnames(sst_window)[!colnames(sst_window) %in% c("survey_id", "longitude", "latitude", "scenario", "model")] <- gsub("tos_", "", colnames(sst_window)[!colnames(sst_window) %in% c("survey_id", "longitude", "latitude", "scenario", "model")])

sst <- plot_data(data = sst_window |> 
                   dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                             scenario == "ssp245" ~ "SSP2-4.5",
                                                             scenario == "ssp370" ~ "SSP3-7.0",
                                                             scenario == "ssp585" ~ "SSP5-8.5")),
                 var_name = "SST",
                 color = "scenario",
                 year = as.character(seq(from = 2005, to = 2100, by = 1)),
                 legend = TRUE,
                 breaks = c(2005, 2014, 2025, 2050, 2070, 2100))


read_var_chl_hist <- chl_window |> 
  dplyr::select(survey_id, longitude, latitude, "chl_2005":"chl_2024", model) |>
  dplyr::group_by(survey_id, longitude, latitude) |> 
  dplyr::summarise(dplyr::across("chl_2005":"chl_2024", mean)) |> 
  dplyr::group_by(survey_id, longitude, latitude) |> 
  dplyr::summarise(chl_2024 = rowMeans(dplyr::across("chl_2005":"chl_2024")))

read_var_chl_future <- chl_window |> 
  dplyr::select(survey_id, longitude, latitude, "chl_2025":"chl_2100", model, scenario) |>
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(dplyr::across("chl_2025":"chl_2100", mean))

chl_hist_future <- read_var_chl_hist |> 
  dplyr::inner_join(read_var_chl_future) |> 
  dplyr::relocate(scenario, .after = latitude) |> 
  dplyr::mutate(model = NA)
# |> 
#   dplyr::group_by(survey_id, longitude, latitude) |> 
#   dplyr::summarise(dplyr::across("2024":"2100"))

# diff_chl <- chl_hist_future$chl_2025 - chl_hist_future$chl_2024
# 
# chl_hist_future[,!colnames(chl_hist_future) %in% c("survey_id", "longitude", "latitude", "scenario", "chl_2024")] <- chl_hist_future[,!colnames(chl_hist_future) %in% c("survey_id", "longitude", "latitude", "scenario", "chl_2024")] - diff_chl

plot_data(data = chl_hist_future |> 
            dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                      scenario == "ssp245" ~ "SSP2-4.5",
                                                      scenario == "ssp370" ~ "SSP3-7.0",
                                                      scenario == "ssp585" ~ "SSP5-8.5")),
          var_name = "chl",
          color = "scenario",
          year = as.character(seq(from = 2005, to = 2100, by = 1)),
          legend = TRUE,
          breaks = c(2005, 2014, 2025, 2050, 2070, 2100))

read_var_ph_bind <- ph_window |>  
  dplyr::select(survey_id, longitude, latitude, "ph_2005":"ph_2024", model) |>
  dplyr::group_by(survey_id, longitude, latitude) |> 
  dplyr::summarise(dplyr::across("ph_2005":"ph_2024", mean)) |> 
  dplyr::group_by(survey_id, longitude, latitude) |> 
  dplyr::summarise(ph_2024 = rowMeans(dplyr::across("ph_2005":"ph_2024")))

read_var_ph_future <- ph_window |>
  dplyr::select(survey_id, longitude, latitude, "ph_2025":"ph_2100", model, scenario) |>
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(across("ph_2025":"ph_2100", mean))

ph_hist_future <- read_var_ph_bind |> 
  dplyr::inner_join(read_var_ph_future) |> 
  dplyr::relocate(scenario, .after = latitude) |> 
  dplyr::mutate(model = NA)
# |> 
#   dplyr::group_by(survey_id, longitude, latitude) |> 
#   dplyr::summarise(dplyr::across("2024":"2100"))

# diff_ph <- ph_hist_future$ph_2025 - ph_hist_future$ph_2024
# 
# ph_hist_future[,!colnames(ph_hist_future) %in% c("survey_id", "longitude", "latitude", "scenario", "ph_2024")] <- ph_hist_future[,!colnames(ph_hist_future) %in% c("survey_id", "longitude", "latitude", "scenario", "ph_2024")] - diff_ph


plot_data(data = ph_hist_future |> 
            dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                      scenario == "ssp245" ~ "SSP2-4.5",
                                                      scenario == "ssp370" ~ "SSP3-7.0",
                                                      scenario == "ssp585" ~ "SSP5-8.5")),
          var_name = "ph",
          color = "scenario",
          year = as.character(seq(from = 2005, to = 2100, by = 1)),
          legend = TRUE,
          breaks = c(2005, 2014, 2025, 2050, 2070, 2100))


read_var_sss_bind <- sss_window |>  
  dplyr::select(survey_id, longitude, latitude, "so_2005":"so_2024", model) |>
  dplyr::group_by(survey_id, longitude, latitude) |> 
  dplyr::summarise(dplyr::across("so_2005":"so_2024", mean)) |> 
  dplyr::group_by(survey_id, longitude, latitude) |> 
  dplyr::summarise(so_2024 = rowMeans(dplyr::across("so_2005":"so_2024")))

read_var_sss_future <- sss_window |>
  dplyr::select(survey_id, longitude, latitude, "so_2025":"so_2100", model, scenario) |>
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(across("so_2025":"so_2100", mean))

sss_hist_future <- read_var_sss_bind |> 
  dplyr::inner_join(read_var_sss_future) |> 
  dplyr::relocate(scenario, .after = latitude) |> 
  dplyr::mutate(model = NA)
# |> 
#   dplyr::group_by(survey_id, longitude, latitude) |> 
#   dplyr::summarise(dplyr::across("2024":"2100"))

# diff_sss <- sss_hist_future$so_2025 - sss_hist_future$so_2024
# 
# sss_hist_future[,!colnames(sss_hist_future) %in% c("survey_id", "longitude", "latitude", "scenario", "so_2024")] <- sss_hist_future[,!colnames(sss_hist_future) %in% c("survey_id", "longitude", "latitude", "scenario", "so_2024")] - diff_sss


plot_data(data = sss_hist_future |> 
            dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                      scenario == "ssp245" ~ "SSP2-4.5",
                                                      scenario == "ssp370" ~ "SSP3-7.0",
                                                      scenario == "ssp585" ~ "SSP5-8.5")),
          var_name = "sss",
          color = "scenario",
          year = as.character(seq(from = 2005, to = 2100, by = 1)),
          legend = TRUE,
          breaks = c(2005, 2014, 2025, 2050, 2070, 2100))


read_var_sst_bind <- sst_window |>  
  dplyr::select(survey_id, longitude, latitude, "tos_2005":"tos_2024", model) |>
  dplyr::group_by(survey_id, longitude, latitude) |> 
  dplyr::summarise(dplyr::across("tos_2005":"tos_2024", mean)) |> 
  dplyr::group_by(survey_id, longitude, latitude) |> 
  dplyr::summarise(tos_2024 = rowMeans(dplyr::across("tos_2005":"tos_2024")))

read_var_sst_future <- sst_window |>
  dplyr::select(survey_id, longitude, latitude, "tos_2025":"tos_2100", model, scenario) |>
  dplyr::group_by(survey_id, longitude, latitude, scenario) |> 
  dplyr::summarise(across("tos_2025":"tos_2100", mean))

sst_hist_future <- read_var_sst_bind |> 
  dplyr::inner_join(read_var_sst_future) |> 
  dplyr::relocate(scenario, .after = latitude) |> 
  dplyr::mutate(model = NA)
# |> 
#   dplyr::group_by(survey_id, longitude, latitude) |> 
#   dplyr::summarise(dplyr::across("2024":"2100"))

# diff_sst <- sst_hist_future$tos_2025 - sst_hist_future$tos_2024
# 
# sst_hist_future[,!colnames(sst_hist_future) %in% c("survey_id", "longitude", "latitude", "scenario", "tos_2024")] <- sst_hist_future[,!colnames(sst_hist_future) %in% c("survey_id", "longitude", "latitude", "scenario", "tos_2024")]  - diff_sst

plot_data(data = sst_hist_future |> 
            dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                                      scenario == "ssp245" ~ "SSP2-4.5",
                                                      scenario == "ssp370" ~ "SSP3-7.0",
                                                      scenario == "ssp585" ~ "SSP5-8.5")),
          var_name = "sst",
          color = "scenario",
          year = as.character(seq(from = 2005, to = 2100, by = 1)),
          legend = TRUE,
          breaks = c(2005, 2014, 2025, 2050, 2070, 2100))

list_future <- list(chl_hist_future |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6)),
                    ph_hist_future |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6)),
                    sss_hist_future |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6)),
                    sst_hist_future |> 
                      dplyr::ungroup() |> 
                      dplyr::mutate(latitude = round(latitude, 6),
                                    longitude = round(longitude, 6))
)

year <- 2025:2100

# Create future env dataframe per year
env_future_hist <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  year_i <- year[i]
  
  col_name_future <- lapply(1:length(list_future), function(j) { 
    
    colnames(list_future[[j]])
    
  })
  
  which_var_year_i <- lapply(1:length(col_name_future), function(j) {
    
    col_name_future[[j]][which(grepl(year_i, col_name_future[[j]]) == TRUE)]
    
  })
  
  var_year_i <- lapply(1:length(list_future), function(j) {
    
    list_future[[j]][,c("survey_id", "longitude", "latitude", "scenario", which_var_year_i[[j]])]
    
  })
  
  var_year_i_bind <- purrr::reduce(var_year_i, dplyr::inner_join)
  
}, mc.cores = parallel::detectCores() - 1)
names(env_future_hist) <- year

save(env_future_hist, file = "data/extracted/env_future_hist.Rdata")

load("data/extracted/rls_covariates_train.RData")

rls_covariates_train_hist <- rls_covariates_train |> 
  dplyr::select(-c(chl:gdp))

rls_covariates_train_hist$survey_id <- as.numeric(rls_covariates_train_hist$survey_id)

env_hist <- list_future[[1]][,c("survey_id", "longitude", "latitude", "scenario", "chl_2024")] |> 
  dplyr::inner_join(list_future[[2]][,c("survey_id", "longitude", "latitude", "scenario", "ph_2024")]) |> 
  dplyr::inner_join(list_future[[3]][,c("survey_id", "longitude", "latitude", "scenario", "so_2024")]) |> 
  dplyr::inner_join(list_future[[4]][,c("survey_id", "longitude", "latitude", "scenario", "tos_2024")])

rls_covariates_train_hist <- env_hist |> 
  dplyr::inner_join(rls_covariates_train_hist)

save(rls_covariates_train_hist, file = "data/extracted/rls_covariates_train_hist.Rdata")
