############################################################################################################
####################################  Assess non-analogue conditions   #####################################
############################################################################################################

load("data/extracted/env_baseline_ext_hist.Rdata")
load("data/extracted/env_future_ext_hist.Rdata")

covariates <- env_baseline_ext_hist |> 
  dplyr::group_by(x, y) |>
  dplyr::summarise(dplyr::across(c(chl:gdp), mean))

cov <- covariates |> 
  dplyr::ungroup() |> 
  dplyr::mutate(year = 2024)
year <- "2025":"2100"

cov_future <- pbmcapply::pbmclapply(1:length(year), function(k) {
  
  year_k <- env_future_ext_hist[[k]] # Select the kth future covariates dataset (k = 1 = 2023 ; k = 2 = 2024 ; k = 3 = 2025 ; etc)
  
  scenario <- lapply(1:length(unique(year_k$scenario)), function(l) { # Select the lth scenario dataset (l = 1 = ssp126 ; l = 2 = ssp245 ; etc5)
    
    scenario_l <- year_k[year_k$scenario == unique(year_k$scenario)[l],]
    
    # Rename covariates
    
    colnames(scenario_l)[!colnames(scenario_l) %in% c("x", "y", "scenario")] <- gsub(paste0("_", names(env_future_ext_hist[k])), "", colnames(scenario_l)[!colnames(scenario_l) %in% c("x", "y", "scenario")])
    
    # Add human and habitat covariates
    
    scenario_l <- scenario_l |>
      dplyr::inner_join(cov[!colnames(cov) %in% c("chl", "ph", "sss", "sst", "gdp")])
    
  })
  
  scenario_bind <- purrr::reduce(scenario, dplyr::full_join)
  scenario_bind$year <- year[k]
  scenario_bind
  
}, mc.cores = parallel::detectCores() - 1)
cov_future <- do.call(rbind, cov_future)
cov_future <- cov_future |> 
  dplyr::ungroup()

baseline_future <- dplyr::full_join(cov, cov_future)
baseline_future[!colnames(baseline_future) %in% c("x", "y", "scenario", "year")] <- scale(baseline_future[!colnames(baseline_future) %in% c("x", "y", "scenario", "year")], center = TRUE, scale = TRUE)

sp_cov <- baseline_future |> 
  dplyr::filter(year == "2024") |> 
  dplyr::select(-c(year, scenario)) |> 
  as.data.frame()

future <- baseline_future |> 
  dplyr::filter(year != "2024") |> 
  dplyr::group_split(year)

scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")


non_analog_climat <- pbmcapply::pbmclapply(1:length(future), function(i) {
  
  year_i <- future[[i]]
  
  scenarii_i <- lapply(1:length(scenarii), function(j) {
    
    test_mess <- modEvA::MESS(V = as.data.frame(sp_cov[!colnames(sp_cov) %in% c("scenario", "effectiveness", "x", "y")]),
                              P = as.data.frame(year_i[year_i$scenario == scenarii[j],][!colnames(year_i[year_i$scenario == scenarii[j],]) %in% c("scenario", "effectiveness", "year", "x", "y")]))
    test_mess_mean <- test_mess |>
      dplyr::rowwise() |>
      dplyr::mutate(mean_diff = mean(c(chl, ph, sss, sst))) |>
      dplyr::select(-c(TOTAL, MoD))
    info <- year_i[year_i$scenario == scenarii[j],] |>
      dplyr::inner_join(sp_cov[colnames(sp_cov) %in% c("x", "y")]) |>
      dplyr::select(x, y, year, scenario)
    test_mess_mean <- cbind(info, test_mess_mean)
    
    test_mess_mean
    
  })
  
}, mc.cores = parallel::detectCores() - 1)


non_analog_climat_ssp126 <- lapply(non_analog_climat, '[[', 1)
non_analog_climat_ssp126_reduce <- do.call(rbind, non_analog_climat_ssp126)
non_analog_climat_ssp126_reduce <- non_analog_climat_ssp126_reduce |> 
  dplyr::mutate(diff = ifelse(mean_diff < 0, 1, 0))
non_analog_climat_ssp126_reduce$diff <- as.factor(non_analog_climat_ssp126_reduce$diff)

non_analog_climat_ssp245 <- lapply(non_analog_climat, '[[', 2)
non_analog_climat_ssp245_reduce <- do.call(rbind, non_analog_climat_ssp245)
non_analog_climat_ssp245_reduce <- non_analog_climat_ssp245_reduce |> 
  dplyr::mutate(diff = ifelse(mean_diff < 0, 1, 0))
non_analog_climat_ssp245_reduce$diff <- as.factor(non_analog_climat_ssp245_reduce$diff)

non_analog_climat_ssp370 <- lapply(non_analog_climat, '[[', 3)
non_analog_climat_ssp370_reduce <- do.call(rbind, non_analog_climat_ssp370)
non_analog_climat_ssp370_reduce <- non_analog_climat_ssp370_reduce |> 
  dplyr::mutate(diff = ifelse(mean_diff < 0, 1, 0))
non_analog_climat_ssp370_reduce$diff <- as.factor(non_analog_climat_ssp370_reduce$diff)

non_analog_climat_ssp585 <- lapply(non_analog_climat, '[[', 4)
non_analog_climat_ssp585_reduce <- do.call(rbind, non_analog_climat_ssp585)
non_analog_climat_ssp585_reduce <- non_analog_climat_ssp585_reduce |> 
  dplyr::mutate(diff = ifelse(mean_diff < 0, 1, 0))
non_analog_climat_ssp585_reduce$diff <- as.factor(non_analog_climat_ssp585_reduce$diff)

world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_map <- world_map |> 
  dplyr::filter(name_long %in% c("Australia", "Indonesia", "Papua New Guinea", "East Timor", "Singapore", "Malaysia",
                                 "Brunei Darussalam", "Philippines", "Solomon Islands", "Vietnam", "Cambodia", "Thailand",
                                 "New Caledonia", "Vanuatu", "Lao PDR", "China", "Myanmar"))

library(ggplot2)

non_analog_climat <- non_analog_climat_ssp126_reduce |> 
  dplyr::full_join(non_analog_climat_ssp245_reduce) |> 
  dplyr::full_join(non_analog_climat_ssp370_reduce) |> 
  dplyr::full_join(non_analog_climat_ssp585_reduce) |> 
  dplyr::mutate(scenario = dplyr::recode(scenario,
                                         "ssp126" = "SSP1-2.6",
                                         "ssp245" = "SSP2-4.5",
                                         "ssp370" = "SSP3-7.0",
                                         "ssp585" = "SSP5-8.5"))

# how many pixels ?

nrow(non_analog_climat |> 
       dplyr::filter(year == "2025",
                     scenario == "SSP1-2.6"))

cut_biomass <- non_analog_climat |>
  dplyr::group_by(year, scenario, diff) |>
  dplyr::summarise(n_non_analog = round(((dplyr::n() * 100) / nrow(non_analog_climat |> 
                                                                     dplyr::filter(year == "2025",
                                                                                   scenario == "SSP1-2.6"))), 2)) |>
  dplyr::filter(diff == 1)

cut_biomass$scenario <- as.factor(cut_biomass$scenario)

cut_biomass <- split(cut_biomass, cut_biomass$scenario)

non_analog_climat_map <- non_analog_climat |> 
  dplyr::filter(x > 95.20664 & x < 169.8963,
                y > -54.74922 & y < 23.34521)

non_analog_climat_map <- non_analog_climat_map |> 
  dplyr::filter(year %in% c(2050, 2060, 2075, 2100))

map_non_analog_climate <- ggplot(data = world_map) +
  geom_sf() +
  geom_raster(data = non_analog_climat_map, aes(x = x, y = y, fill = diff)) +
  theme_bw() +
  scale_fill_manual(values = c("0" = "grey", "1" = viridis::viridis(1)),
                    labels = c("Analogue conditions", "Non-analogue conditions")) +
  scale_x_continuous(limits = c(95.20664, 170), breaks = c(100, 120, 140, 160)) +
  scale_y_continuous(limits = c(-35, 23.34521), breaks = c(-30, -10, 10, 23)) +
  coord_sf(expand = FALSE) +
  facet_grid(rows = vars(scenario),
             cols = vars(year)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15), 
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        panel.background = element_rect(fill = "white", colour = "grey50",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(title = "A.", x = "", y = "", fill = "")
ggsave("figures/map_non_analog_climate_hist.png", map_non_analog_climate, width = 11, height = 11)

cut_biomass_bind <- do.call(rbind, cut_biomass)

add_0 <- data.frame(year = as.numeric(2041:2100),
                    scenario = as.factor("SSP1-2.6"),
                    diff = as.factor(1),
                    n_non_analog = 0)

cut_biomass_bind <- rbind(cut_biomass_bind, add_0)


non_analog_plot <- cut_biomass_bind |> 
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")) |> 
  ggplot() + 
  geom_line(aes(x = year, y = n_non_analog, color = scenario), size = 1) +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(n = 9, name = "Set1")[3], PNWColors::pnw_palette("Bay", 3, type = "discrete"))) +
  geom_hline(aes(yintercept = 30), color = "black", linetype = "dashed", size = 1) + 
  theme_bw() + 
  labs(title = "B.", y = "Pixels under non-analoguous conditions (%)", color = "", x = "Year")
ggsave("figures/non_analog_plot_hist.png", non_analog_plot)



color_scale <- ggthemes::colorblind_pal()(8)[-1]

non_analog_plot <- cut_biomass_bind |> 
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")) |> 
  ggplot() + 
  geom_line(aes(x = year, y = n_non_analog, color = scenario), size = 1) +
  scale_color_manual(values = c("SSP1-2.6" = color_scale[3],
                                "SSP2-4.5" = color_scale[2],
                                "SSP3-7.0" = color_scale[4],
                                "SSP5-8.5" = color_scale[1])) +
  geom_hline(aes(yintercept = 30), color = "black", linetype = "dashed", size = 1) + 
  theme_bw() + 
  labs(title = "B.", y = "Pixels under non-analoguous conditions (%)", color = "", x = "Year")
ggsave("non_analog_plot_hist.png", non_analog_plot)
