load("data/extracted/rls_covariates_train_hist.RData")
load("data/raw/00_rls_surveys.Rdata")
load("data/extracted/env_future_raw_hist.Rdata")

year <- 2025:2100
names(env_future_raw_hist) <- year

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

covariates <- rls_covariates_train_hist |>
  dplyr::group_by(survey_id) |>
  dplyr::summarise(dplyr::across(c(chl:marine_ecosystem_dependency, Plateau_500m:Lagoon), mean))
effectiveness <- rls_covariates_train_hist |>
  dplyr::select(survey_id, protection_status) |>
  unique()
covariates <- covariates |>
  dplyr::inner_join(effectiveness) |>
  dplyr::inner_join(rls_surveys[colnames(rls_surveys) %in% c("survey_id", "realm")]) |> 
  tidyr::drop_na()

env_future <- pbmcapply::pbmclapply(1:length(env_future_raw_hist), function(i) {
  
  year_i <- env_future_raw_hist[[i]]
  year_i$survey_id <- as.character(year_i$survey_id)
  
  colnames(year_i)[!colnames(year_i) %in% c("survey_id", "longitude", "latitude", "scenario")] <- gsub(paste0("_", names(env_future_raw_hist[i])), "", colnames(year_i)[!colnames(year_i) %in% c("survey_id", "longitude", "latitude", "scenario")])
  
  year_i$year <- names(env_future_raw_hist)[[i]]
  
  year_i
  
}, mc.cores = parallel::detectCores() - 1)
env_future_bind <- do.call(rbind, env_future)





baseline_126 <- covariates |> 
  dplyr::select(survey_id, chl) |> 
  dplyr::mutate(year = "2024",
                scenario = "ssp126")
baseline_245 <- covariates |> 
  dplyr::select(survey_id, chl) |> 
  dplyr::mutate(year = "2024",
                scenario = "ssp245")
baseline_370 <- covariates |> 
  dplyr::select(survey_id, chl) |> 
  dplyr::mutate(year = "2024",
                scenario = "ssp370")
baseline_585 <- covariates |> 
  dplyr::select(survey_id, chl) |> 
  dplyr::mutate(year = "2024",
                scenario = "ssp585")
baseline <- baseline_126 |> 
  dplyr::full_join(baseline_245) |> 
  dplyr::full_join(baseline_370) |> 
  dplyr::full_join(baseline_585)
future <- env_future_bind |> 
  dplyr::select(survey_id, chl, scenario, year)
baseline_future <- baseline |> 
  dplyr::full_join(future)

data_pivot_scenario <- baseline_future |> 
  dplyr::group_by(scenario, year) |> 
  dplyr::summarise(mean_value = mean(chl),
                   se = sd(chl)  / sqrt(dplyr::n()),
                   lower = mean_value - qt(0.975, df = dplyr::n() - 1) * se,
                   upper = mean_value + qt(0.975, df = dplyr::n() - 1) * se) |> 
  dplyr::mutate(scenario = dplyr::case_when(scenario == "ssp126" ~ "SSP1-2.6",
                                            scenario == "ssp245" ~ "SSP2-4.5",
                                            scenario == "ssp370" ~ "SSP3-7.0",
                                            scenario == "ssp585" ~ "SSP5-8.5"))

library(ggplot2)

plot_data <- ggplot(data = data_pivot_scenario, aes(x = year, y = mean_value, group = scenario, color = scenario)) + 
  theme_bw() +
  theme(title = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 20)) +
  scale_x_discrete(breaks = c(2005, 2025, 2050, 2070, 2100))

plot_data <- plot_data +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, size = 0.1) +
  geom_line(size = 1) +
  scale_color_manual(values = c("SSP1-2.6" = RColorBrewer::brewer.pal(n = 9, name = "Set1")[3],
                                "SSP2-4.5" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[1],
                                "SSP3-7.0" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[2],
                                "SSP5-8.5" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[3])) +
  labs(x = "", y = "", color = "")






a <- baseline_future |> 
  dplyr::filter(scenario == "ssp126")

sample_id <- sample(unique(a$survey_id), 100)


library(ggplot2)

plot_data <- a |> 
  dplyr::filter(survey_id %in% sample_id) |> 
  ggplot(aes(x = year, y = chl, group = survey_id)) + 
  theme_bw() +
  theme(title = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 20)) +
  scale_x_discrete(breaks = c(2005, 2025, 2050, 2070, 2100))

plot_data <- plot_data +
  geom_line(size = 1)
