###########################################################################
############# Load environmental covariates for extrapolation #############
###########################################################################

source("R/03_environmental_window_function.R")

list_extracted_cov_ext_fut <- list.files("data/extracted/covariates/environmental_extrapolation", full.names = TRUE)

chl_ext <- list_extracted_cov_ext_fut[which(grepl("chl", list_extracted_cov_ext_fut) == TRUE)]
ph_ext <- list_extracted_cov_ext_fut[which(grepl("ph", list_extracted_cov_ext_fut) == TRUE)]
sss_ext <- list_extracted_cov_ext_fut[which(grepl("so", list_extracted_cov_ext_fut) == TRUE)]
sst_ext <- list_extracted_cov_ext_fut[which(grepl("to", list_extracted_cov_ext_fut) == TRUE)]

list_extracted_cov_ext_hist <- list.files("data/extracted/covariates/environmental_extrapolation_historical", full.names = TRUE)

chl_ext_hist <- list_extracted_cov_ext_hist[which(grepl("chl", list_extracted_cov_ext_hist) == TRUE)]
ph_ext_hist <- list_extracted_cov_ext_hist[which(grepl("ph", list_extracted_cov_ext_hist) == TRUE)]
sss_ext_hist <- list_extracted_cov_ext_hist[which(grepl("so", list_extracted_cov_ext_hist) == TRUE)]
sst_ext_hist <- list_extracted_cov_ext_hist[which(grepl("tos", list_extracted_cov_ext_hist) == TRUE)]

### CHL

read_var_chl_ext_fut <- lapply(1:length(chl_ext), function(i) {
  
  load(chl_ext[i])
  assign(paste0("var_", i), data)
  
})
read_var_chl_ext_hist <- lapply(1:length(chl_ext_hist), function(i) {
  
  load(chl_ext_hist[i])
  assign(paste0("var_", i), data)
  
})

read_var_chl_bind_ext_hist <- do.call(rbind, read_var_chl_ext_hist) |> 
  dplyr::filter(!model == "IPSL-CM6A-LR") |> 
  dplyr::select(x, y, "2005":"2014", model)

read_var_chl_ext_bind_fut <- do.call(rbind, read_var_chl_ext_fut) |> 
  dplyr::filter(!model == "IPSL-CM6A-LR") |> 
  dplyr::select(x, y, "2015":"2024", model, scenario)

read_var_chl_bind_ext_hist <- read_var_chl_bind_ext_hist |> 
  dplyr::inner_join(read_var_chl_ext_bind_fut) |> 
  dplyr::relocate(model, .after = "2024") |> 
  dplyr::group_by(x, y) |> 
  dplyr::summarise(dplyr::across("2005":"2024", mean)) |> 
  dplyr::group_by(x, y) |> 
  dplyr::summarise(chl = rowMeans(dplyr::across("2005":"2024")))

read_var_chl_bind_ext_fut <- do.call(rbind, read_var_chl_ext_fut) |> 
  dplyr::filter(!model == "IPSL-CM6A-LR") |> 
  dplyr::select(x, y, "2025":"2100", model, scenario) |>
  dplyr::ungroup() |> 
  dplyr::group_by(x, y, scenario) |> 
  dplyr::summarise(dplyr::across("2025":"2100", mean))

year <- 2025:2100

# Environmental window
chl_future_window_ext <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = read_var_chl_bind_ext_fut,
                                info = c("x", "y", "scenario"),
                                varname = "chl")
  
}, mc.cores = parallel::detectCores() - 1)
chl_future_window_ext_reduce <- purrr::reduce(chl_future_window_ext, dplyr::full_join)

chl_fut_hist_ext <- read_var_chl_bind_ext_hist |> 
  dplyr::inner_join(chl_future_window_ext_reduce) |> 
  dplyr::relocate(scenario, .after = y)

diff_chl <- chl_fut_hist_ext$chl_2025 - chl_fut_hist_ext$chl

chl_fut_hist_ext[,!colnames(chl_fut_hist_ext) %in% c("x", "y", "scenario", "chl")] <- chl_fut_hist_ext[,!colnames(chl_fut_hist_ext) %in% c("x", "y", "scenario", "chl")] - diff_chl

read_var_chl_bind_ext_hist <- chl_fut_hist_ext[,colnames(chl_fut_hist_ext) %in% c("x", "y", "scenario", "chl")]

chl_future_window_ext_reduce <- chl_fut_hist_ext[,!colnames(chl_fut_hist_ext) %in% c("chl")]


### SSS

read_var_sss_ext_fut <- lapply(1:length(sss_ext), function(i) {
  
  load(sss_ext[i])
  assign(paste0("var_", i), data)
  
})
read_var_sss_ext_hist <- lapply(1:length(sss_ext_hist), function(i) {
  
  load(sss_ext_hist[i])
  assign(paste0("var_", i), data)
  
})

read_var_sss_bind_ext_hist <- do.call(rbind, read_var_sss_ext_hist) |> 
  dplyr::select(x, y, "2005":"2014", model)

read_var_sss_ext_bind_fut <- do.call(rbind, read_var_sss_ext_fut) |> 
  dplyr::select(x, y, "2015":"2024", model, scenario)

read_var_sss_bind_ext_hist <- read_var_sss_bind_ext_hist |> 
  dplyr::inner_join(read_var_sss_ext_bind_fut) |> 
  dplyr::relocate(model, .after = "2024") |> 
  dplyr::group_by(x, y) |> 
  dplyr::summarise(dplyr::across("2005":"2024", mean)) |> 
  dplyr::group_by(x, y) |> 
  dplyr::summarise(sss = rowMeans(dplyr::across("2005":"2024")))

read_var_sss_bind_ext_fut <- do.call(rbind, read_var_sss_ext_fut) |> 
  dplyr::select(x, y, "2025":"2100", model, scenario) |>
  dplyr::ungroup() |> 
  dplyr::group_by(x, y, scenario) |> 
  dplyr::summarise(dplyr::across("2025":"2100", mean))

year <- 2025:2100

# Environmental window
sss_future_window_ext <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = read_var_sss_bind_ext_fut,
                                info = c("x", "y", "scenario"),
                                varname = "sss")
  
}, mc.cores = parallel::detectCores() - 1)
sss_future_window_ext_reduce <- purrr::reduce(sss_future_window_ext, dplyr::full_join)

sss_fut_hist_ext <- read_var_sss_bind_ext_hist |> 
  dplyr::inner_join(sss_future_window_ext_reduce) |> 
  dplyr::relocate(scenario, .after = y)

diff_sss <- sss_fut_hist_ext$sss_2025 - sss_fut_hist_ext$sss

sss_fut_hist_ext[,!colnames(sss_fut_hist_ext) %in% c("x", "y", "scenario", "sss")] <- sss_fut_hist_ext[,!colnames(sss_fut_hist_ext) %in% c("x", "y", "scenario", "sss")] - diff_sss

read_var_sss_bind_ext_hist <- sss_fut_hist_ext[,colnames(sss_fut_hist_ext) %in% c("x", "y", "scenario", "sss")]

sss_future_window_ext_reduce <- sss_fut_hist_ext[,!colnames(sss_fut_hist_ext) %in% c("sss")]


### pH

read_var_ph_ext_fut <- lapply(1:length(ph_ext), function(i) {
  
  load(ph_ext[i])
  assign(paste0("var_", i), data)
  
})
read_var_ph_ext_hist <- lapply(1:length(ph_ext_hist), function(i) {
  
  load(ph_ext_hist[i])
  assign(paste0("var_", i), data)
  
})

read_var_ph_bind_ext_hist <- do.call(rbind, read_var_ph_ext_hist) |> 
  dplyr::select(x, y, "2005":"2014", model)

read_var_ph_ext_bind_fut <- do.call(rbind, read_var_ph_ext_fut) |> 
  dplyr::select(x, y, "2015":"2024", model, scenario)

read_var_ph_bind_ext_hist <- read_var_ph_bind_ext_hist |> 
  dplyr::inner_join(read_var_ph_ext_bind_fut) |> 
  dplyr::relocate(model, .after = "2024") |> 
  dplyr::group_by(x, y) |> 
  dplyr::summarise(dplyr::across("2005":"2024", mean)) |> 
  dplyr::group_by(x, y) |> 
  dplyr::summarise(ph = rowMeans(dplyr::across("2005":"2024")))

read_var_ph_bind_ext_fut <- do.call(rbind, read_var_ph_ext_fut) |> 
  dplyr::select(x, y, "2025":"2100", model, scenario) |>
  dplyr::ungroup() |> 
  dplyr::group_by(x, y, scenario) |> 
  dplyr::summarise(dplyr::across("2025":"2100", mean))

year <- 2025:2100

# Environmental window
ph_future_window_ext <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = read_var_ph_bind_ext_fut,
                                info = c("x", "y", "scenario"),
                                varname = "ph")
  
}, mc.cores = parallel::detectCores() - 1)
ph_future_window_ext_reduce <- purrr::reduce(ph_future_window_ext, dplyr::full_join)

ph_fut_hist_ext <- read_var_ph_bind_ext_hist |> 
  dplyr::inner_join(ph_future_window_ext_reduce) |> 
  dplyr::relocate(scenario, .after = y)

diff_ph <- ph_fut_hist_ext$ph_2025 - ph_fut_hist_ext$ph

ph_fut_hist_ext[,!colnames(ph_fut_hist_ext) %in% c("x", "y", "scenario", "ph")] <- ph_fut_hist_ext[,!colnames(ph_fut_hist_ext) %in% c("x", "y", "scenario", "ph")] - diff_ph

read_var_ph_bind_ext_hist <- ph_fut_hist_ext[,colnames(ph_fut_hist_ext) %in% c("x", "y", "scenario", "ph")]

ph_future_window_ext_reduce <- ph_fut_hist_ext[,!colnames(ph_fut_hist_ext) %in% c("ph")]


### SST

read_var_sst_ext_fut <- lapply(1:length(sst_ext), function(i) {
  
  load(sst_ext[i])
  assign(paste0("var_", i), data)
  
})
read_var_sst_ext_hist <- lapply(1:length(sst_ext_hist), function(i) {
  
  load(sst_ext_hist[i])
  assign(paste0("var_", i), data)
  
})

read_var_sst_bind_ext_hist <- do.call(rbind, read_var_sst_ext_hist) |> 
  dplyr::select(x, y, "2005":"2014", model)

read_var_sst_ext_bind_fut <- do.call(rbind, read_var_sst_ext_fut) |> 
  dplyr::select(x, y, "2015":"2024", model, scenario)

read_var_sst_bind_ext_hist <- read_var_sst_bind_ext_hist |> 
  dplyr::inner_join(read_var_sst_ext_bind_fut) |> 
  dplyr::relocate(model, .after = "2024") |> 
  dplyr::group_by(x, y) |> 
  dplyr::summarise(dplyr::across("2005":"2024", mean)) |> 
  dplyr::group_by(x, y) |> 
  dplyr::summarise(sst = rowMeans(dplyr::across("2005":"2024")))

read_var_sst_bind_ext_fut <- do.call(rbind, read_var_sst_ext_fut) |> 
  dplyr::select(x, y, "2025":"2100", model, scenario) |>
  dplyr::ungroup() |> 
  dplyr::group_by(x, y, scenario) |> 
  dplyr::summarise(dplyr::across("2025":"2100", mean))

year <- 2025:2100

# Environmental window
sst_future_window_ext <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  environmental_window_function(years = year,
                                year_i = year[i],
                                data = read_var_sst_bind_ext_fut,
                                info = c("x", "y", "scenario"),
                                varname = "sst")
  
}, mc.cores = parallel::detectCores() - 1)
sst_future_window_ext_reduce <- purrr::reduce(sst_future_window_ext, dplyr::full_join)

sst_fut_hist_ext <- read_var_sst_bind_ext_hist |> 
  dplyr::inner_join(sst_future_window_ext_reduce) |> 
  dplyr::relocate(scenario, .after = y)

diff_sst <- sst_fut_hist_ext$sst_2025 - sst_fut_hist_ext$sst

sst_fut_hist_ext[,!colnames(sst_fut_hist_ext) %in% c("x", "y", "scenario", "sst")] <- sst_fut_hist_ext[,!colnames(sst_fut_hist_ext) %in% c("x", "y", "scenario", "sst")] - diff_sst

read_var_sst_bind_ext_hist <- sst_fut_hist_ext[,colnames(sst_fut_hist_ext) %in% c("x", "y", "scenario", "sst")]

sst_future_window_ext_reduce <- sst_fut_hist_ext[,!colnames(sst_fut_hist_ext) %in% c("sst")]


gdp_ext <- list.files("data/extracted/covariates/social_extrapolation", full.names = TRUE)

read_var_gdp_ext <- lapply(1:length(gdp_ext), function(i) {
  
  load(gdp_ext[i])
  assign(paste0("var_", i), data)
  
})
read_var_gdp_ext_bind <- do.call(rbind, read_var_gdp_ext) |>
  dplyr::filter(!scenario %in% c("ssp4")) |>
  dplyr::select(x, y, "2010":"2020", model, scenario) |>
  dplyr::mutate(x = round(x, 2),
                y = round(y, 2)) |> 
  dplyr::group_by(x, y, scenario) |>
  dplyr::summarise(dplyr::across("2010":"2020", mean)) |>
  dplyr::group_by(x, y, scenario) |>
  dplyr::summarise(gdp = rowMeans(dplyr::across("2010":"2020"))) |>
  dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp1" ~ "ssp126",
                                            scenario == "ssp2" ~ "ssp245",
                                            scenario == "ssp3" ~ "ssp370",
                                            scenario == "ssp5" ~ "ssp585"))


read_var_gdp_ext_future <- do.call(rbind, read_var_gdp_ext) |>
  dplyr::filter(!scenario %in% c("ssp4")) |>
  dplyr::select(x, y, "2030":"2100", model, scenario) |>
  dplyr::mutate(x = round(x, 2),
                y = round(y, 2)) |> 
  dplyr::group_by(x, y, scenario) |>
  dplyr::summarise(dplyr::across("2030":"2100", mean)) |>
  dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp1" ~ "ssp126",
                                            scenario == "ssp2" ~ "ssp245",
                                            scenario == "ssp3" ~ "ssp370",
                                            scenario == "ssp5" ~ "ssp585"))
read_var_gdp_ext_future[,colnames(read_var_gdp_ext_future) %in% "2030":"2100"] <- log10(read_var_gdp_ext_future[,colnames(read_var_gdp_ext_future) %in% "2030":"2100"] + 1)
year <- colnames(read_var_gdp_ext_future)[!colnames(read_var_gdp_ext_future) %in% c("x", "y", "scenario")]

read_var_gdp_ext_future_full <- read_var_gdp_ext_future |>
  dplyr::select(x:scenario) |>
  dplyr::mutate(!!!setNames(rep(list(NA), length(2025:2100)), as.character(2025:2100)))
year_full <- colnames(read_var_gdp_ext_future_full)[!colnames(read_var_gdp_ext_future_full) %in% c("x", "y", "scenario")]

read_var_gdp_ext_future_full <- pbmcapply::pbmclapply(1:length(year_full), function(i) {
  
  year_i <- read_var_gdp_ext_future_full |>
    dplyr::select(x, y, scenario, year_full[i])
  
  year_i[,colnames(year_i) %in% year_full[i]] <- read_var_gdp_ext_future[,colnames(read_var_gdp_ext_future) %in% year[which.min(abs(as.numeric(year) - as.numeric(year_full[i])))]]
  
  return(year_i)
  
}, mc.cores = parallel::detectCores() - 1)
read_var_gdp_ext_future_full <- purrr::reduce(read_var_gdp_ext_future_full, dplyr::inner_join)
colnames(read_var_gdp_ext_future_full)[colnames(read_var_gdp_ext_future_full) %in% year_full] <- paste0("gdp_", colnames(read_var_gdp_ext_future_full)[colnames(read_var_gdp_ext_future_full) %in% year_full])

env_baseline_ext_hist <- read_var_chl_bind_ext_hist |> 
  dplyr::inner_join(read_var_sss_bind_ext_hist) |> 
  dplyr::inner_join(read_var_ph_bind_ext_hist) |> 
  dplyr::inner_join(read_var_sst_bind_ext_hist) |> 
  dplyr::inner_join(read_var_gdp_ext_bind)

list_future_ext_hist <- list(chl_future_window_ext_reduce,
                             ph_future_window_ext_reduce,
                             sss_future_window_ext_reduce,
                             sst_future_window_ext_reduce,
                             read_var_gdp_ext_future_full)

year <- 2025:2100

# Create future env dataframe per year
env_future_ext_hist <- pbmcapply::pbmclapply(1:length(year), function(i) {
  
  year_i <- year[i]
  
  col_name_future <- lapply(1:length(list_future_ext_hist), function(j) { 
    
    colnames(list_future_ext_hist[[j]])
    
  })
  
  which_var_year_i <- lapply(1:length(col_name_future), function(j) {
    
    col_name_future[[j]][which(grepl(year_i, col_name_future[[j]]) == TRUE)]
    
  })
  
  var_year_i <- lapply(1:length(list_future_ext_hist), function(j) {
    
    list_future_ext_hist[[j]][,c("x", "y", "scenario", which_var_year_i[[j]])]
    
  })
  
  var_year_i_bind <- purrr::reduce(var_year_i, dplyr::inner_join)
  
}, mc.cores = parallel::detectCores() - 1)
names(env_future_ext_hist) <- year

save(env_baseline_ext_hist, file = "data/extracted/env_baseline_ext_hist.Rdata")
save(env_future_ext_hist, file = "data/extracted/env_future_ext_hist.Rdata")
