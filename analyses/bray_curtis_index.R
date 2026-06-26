############################################## Evolution of biomass at global, realm and ecoregion scale for all species ############################################## 
source("R/06_score_function.R")
source("R/bray_curtis_function.R")

load("data/raw/00_rls_surveys.Rdata")
load("results/decile_save.Rdata")
load("results/performance_bind.Rdata")

decile_save[30,] <- c(0, 0)
decile_save <- decile_save[order(decile_save$biomass_class),]

rescale_01 <- function(x){(x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))}

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")

rf <- list.files("results/biomass_class_future_rf_full", full.names = TRUE)

rf <- parallel::mclapply(1:length(rf), function(i) {
  
  load(rf[i])
  assign(as.character(i), all_pred)
  
}, mc.cores = 1)

dnn <- list.files("results/biomass_class_future_dnn", full.names = TRUE)

dnn <- parallel::mclapply(1:length(dnn), function(i) {
  
  load(dnn[i])
  assign(as.character(i), all_pred)
  
}, mc.cores = 1)

xgboost <- list.files("results/biomass_class_future_xgboost", full.names = TRUE)

xgboost <- parallel::mclapply(1:length(xgboost), function(i) {
  
  load(xgboost[i])
  assign(as.character(i), all_pred)
  
}, mc.cores = 1)

score_data <- score_function(pred_biomass_data = list(rf, xgboost, dnn),
                             score_data = performance_bind)

bray_curstis_data <- bray_curtis_function(data = score_data)

bray_curstis_bind <- purrr::reduce(bray_curstis_data, dplyr::full_join)

colnames(bray_curstis_bind)[which(grepl("bray_curtis_", colnames(bray_curstis_bind)))] <- gsub(pattern = "bray_curtis_", replacement = "", colnames(bray_curstis_bind)[which(grepl("bray_curtis_", colnames(bray_curstis_bind)))])


bray_curstis_bind_all <- bray_curstis_bind |> 
  dplyr::group_by(scenario) |> 
  dplyr::summarise(dplyr::across("2022":"2100", mean)) |> 
  tidyr::pivot_longer(cols = -c("scenario"),
                      names_to = "year",
                      values_to = "value") |> 
  dplyr::mutate(color = dplyr::case_when(scenario == "ssp126" ~ RColorBrewer::brewer.pal(n = 9, name = "Set1")[3],
                                         scenario == "ssp245" ~ PNWColors::pnw_palette("Bay", 3, type = "discrete")[1],
                                         scenario == "ssp370" ~ PNWColors::pnw_palette("Bay", 3, type = "discrete")[2],
                                         scenario == "ssp585" ~ PNWColors::pnw_palette("Bay", 3, type = "discrete")[3]))

bray_curstis_bind_all$year <- as.numeric(bray_curstis_bind_all$year)
  

bray_curtis_all_plot <- ggplot(bray_curstis_bind_all, aes(x = year, y = value, color = scenario)) + 
  geom_smooth(se = FALSE, size = 0.7) +
  # geom_line(size = 1) + 
  scale_colour_manual(aesthetics = "color", values = c(RColorBrewer::brewer.pal(n = 9, name = "Set1")[3], PNWColors::pnw_palette("Bay", 3, type = "discrete"))) +
  theme_bw() +
  labs(y = "Bray-Curtis") +
  scale_x_continuous(breaks = c(2020, 2050, 2100)) + 
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6),
        strip.text.x = element_text(size = 6),
        strip.text.y = element_text(size = 3))

ggsave("figures/Bray-Curtis_all.png", bray_curtis_all_plot, width = 7, height = 5)


load("data/raw/1c_species_traits_final.Rdata")
species_traits_final <- species_traits_final |> 
  dplyr::ungroup() |> 
  dplyr::rename(species_name = rls_species_name) |> 
  dplyr::select(species_name, trophic_guild)
species_traits_final <- species_traits_final[complete.cases(species_traits_final),]
species_traits_final[species_traits_final$trophic_guild == "Herbivores Microvores Detritivores",]$trophic_guild <- "herbivores"

bray_curstis_bind_traits <- bray_curstis_bind |> 
  dplyr::inner_join(species_traits_final)

bray_curstis_bind_traits_pivot <- bray_curstis_bind_traits |> 
  tidyr::pivot_longer(cols = -c("species_name", "scenario", "trophic_guild"),
                      names_to = "year",
                      values_to = "value") |> 
  dplyr::group_by(scenario, trophic_guild, year) |> 
  dplyr::summarise(value = mean(value))

bray_curstis_bind_traits_pivot$year <- as.numeric(bray_curstis_bind_traits_pivot$year)

bray_curtis_trait_plot <- ggplot(bray_curstis_bind_traits_pivot, aes(x = year, y = value, color = trophic_guild)) + 
  geom_smooth(se = FALSE, size = 0.7) +
  # geom_line(size = 1) + 
  facet_wrap(~scenario) +
  scale_colour_manual(aesthetics = "color", values = c(RColorBrewer::brewer.pal(n = 9, name = "Set1"))) +
  theme_bw() +
  labs(y = "Bray-Curtis") +
  scale_x_continuous(breaks = c(2020, 2050, 2100)) + 
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6),
        strip.text.x = element_text(size = 6),
        strip.text.y = element_text(size = 3))

ggsave("figures/Bray-Curtis_trait.png", bray_curtis_trait_plot, width = 7, height = 5)
