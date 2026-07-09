load("data/extracted/rls_biomass_class.Rdata")
load("data/raw/00_rls_surveys.Rdata")

rescale_01 <- function(x){
  
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm=T))
  
}

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

rls_surveys <- rls_surveys |> 
  dplyr::filter(survey_id %in% rls_biomass_class$survey_id)

test <- rls_surveys |> 
  dplyr::select(realm) |> 
  unique()

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sv")

west_poly <- terra::vect(rbind(
  c(-180, -90),
  c(-30, -90),
  c(-30, 90),
  c(-180, 90),
  c(-180, -90)
), type="polygons", crs="EPSG:4326")

east_poly <- terra::vect(rbind(
  c(-30, -90),
  c(180, -90),
  c(180, 90),
  c(-30, 90),
  c(-30, -90)
), type="polygons", crs="EPSG:4326")

# Couper les polygones
west_part_world <- terra::intersect(world, west_poly)
east_part_world <- terra::intersect(world, east_poly)

# Décaler la partie Ouest
g <- terra::geom(west_part_world)
g[, "x"] <- g[, "x"] + 360
west_shifted <- terra::vect(g, type="polygons", crs="EPSG:4326")
terra::values(west_shifted) <- terra::values(west_part_world)

# Recoller
world_shifted <- rbind(west_shifted, east_part_world)

world_sf <- world_shifted |> 
  sf::st_as_sf()

realm <- terra::vect("data/raw/shp_eco_realm2/cyrilo_final.gpkg")
realm <- terra::aggregate(realm, "REALM", dissolve = TRUE)

realm_sf <- realm |> 
  sf::st_as_sf() |>
  dplyr::filter(REALM %in% test$realm)

library(ggplot2)
library(ggthemes)

rls_map <- ggplot() +
  geom_sf(data = realm_sf, aes(color = REALM), fill = alpha("white", 0), linewidth = 1) +
  geom_sf(data = world_sf, color = NA) +
  scale_color_manual(values = RColorBrewer::brewer.pal(n = 9, name = "Set1")[-6]) +
  coord_sf(ylim = c(-35, 35), expand = FALSE) +
  labs(title = "", color = "") +
  theme_tufte() +
  theme(
    legend.position = "none",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  guides(colour = guide_legend(nrow = 1))

ggsave("figures/rls_map.png", rls_map)
ggsave("figures/rls_map_legend.png", rls_map, width = 14, height = 5)



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

migration_ensemble <- readRDS("results/migration_ensemble8.rds")

species_to_remove <- c("Acanthurus achilles", "Acanthurus grammoptilus", "Acanthurus olivaceus", "Acanthurus pyroferus", 
                       "Amblyglyphidodon curacao", "Canthigaster rostrata", "Centropyge bispinosa", "Centropyge vrolikii", 
                       "Chaetodon kleinii", "Chaetodon ornatissimus", "Cheilinus chlorourus", "Cheilinus fasciatus",
                       "Chromis cyanea", "Chrysiptera rollandi", "Epinephelus daemelii", "Fistularia commersonii", 
                       "Gnathodentex aureolineatus", "Goniistius vestitus", "Hologymnosus annulatus", 
                       "Hypoplectrus nigricans", "Lutjanus carponotatus", "Lutjanus kasmira", "Lutjanus lemniscatus", 
                       "Neoglyphidodon nigroris", "Neoglyphidodon polyacanthus", "Novaculichthys taeniourus", 
                       "Parupeneus spilurus", "Plectorhinchus flavomaculatus", "Plectorhinchus multivittatus", 
                       "Plectropomus maculatus", "Pomacentrus milleri", "Pomacentrus reidi", "Pseudanthias huchtii", 
                       "Pterocaesio pisang", "Pterocaesio tile", "Scarus oviceps", "Stegastes nigricans", 
                       "Thalassoma amblycephalum")
species_to_remove <- stringr::str_replace_all(species_to_remove, " ", "_")
list_species <- gsub(".Rdata", "", list.files("results/biomass_class_future_rf_hist_migration8_full", full.names = FALSE))

which_species_to_remove <- which(list_species %in% species_to_remove)

migration_ensemble <- migration_ensemble[-which_species_to_remove]


future_pred_ecoregion_by_realm <- lapply(1:length(unique(test$realm)), function(i) {
  
  realm_i <- unique(test$realm)[i]
  
  ensemble_model_realm <- pbmcapply::pbmclapply(1:length(migration_ensemble), function(j) {
    
    species_j <- migration_ensemble[[j]]
    
    species_j_realm <- lapply(1:length(species_j), function(k) {
      
      sp_k <- species_j[[k]] |> 
        dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "realm")]) |> 
        dplyr::filter(realm == realm_i) |> 
        dplyr::select(-realm)
      
    })
    
  }, mc.cores = parallel::detectCores() - 1)

  data <- future_pred_scale_function(data = ensemble_model_realm,
                                     unit = "kg",
                                     time = as.character(c("2024":"2075")),
                                     scale = "realm",
                                     score = TRUE,
                                     to_remove = 0,
                                     biais = TRUE,
                                     mc.cores = parallel::detectCores() - 1)
  
  data |> 
    dplyr::filter(year %in% c(2024, 2030, 2050, 2075)) |> 
    dplyr::group_by(scenario, scale) |> 
    dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                  upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                  biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |>
    dplyr::ungroup() |>
    dplyr::filter(year %in% c(2030, 2050, 2075)) |> 
    ggplot(aes(x = as.factor(year), y = biomass, color = scenario)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "red") +
    geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.6), size = 1) +
    scale_color_manual(values = color_scale) +
    theme_bw() +
    labs(x = "", y = "", title = realm_i) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          title = element_text(size = 14))
  
  
})
names(future_pred_ecoregion_by_realm) <- unique(test$realm)

dir.create("figures/future_realm_plot_migration_point8")

ggsave("figures/future_realm_plot_migration_point8/future_pred_tropical_atlantic.png", future_pred_ecoregion_by_realm[[1]], width = 4, height = 4)
ggsave("figures/future_realm_plot_migration_point8/future_pred_temperate_australasia.png", future_pred_ecoregion_by_realm[[2]], width = 4, height = 4)
ggsave("figures/future_realm_plot_migration_point8/future_pred_central_indo-pacific.png", future_pred_ecoregion_by_realm[[3]], width = 4, height = 4)
ggsave("figures/future_realm_plot_migration_point8/future_pred_western_indo-pacific.png", future_pred_ecoregion_by_realm[[4]], width = 4, height = 4)
ggsave("figures/future_realm_plot_migration_point8/future_pred_temperate_northern_atlantic.png", future_pred_ecoregion_by_realm[[5]], width = 4, height = 4)
ggsave("figures/future_realm_plot_migration_point8/future_pred_tropical_eastern_pacific.png", future_pred_ecoregion_by_realm[[6]], width = 4, height = 4)
ggsave("figures/future_realm_plot_migration_point8/future_pred_eastern_indo-pacific.png", future_pred_ecoregion_by_realm[[7]], width = 4, height = 4)


withoutmigration_ensemble <- readRDS("results/withoutmigration_ensemble8.rds")
withoutmigration_ensemble <- withoutmigration_ensemble[-which_species_to_remove]

future_pred_ecoregion_by_realm_without <- lapply(1:length(unique(test$realm)), function(i) {
  
  realm_i <- unique(test$realm)[i]
  
  ensemble_model_realm <- pbmcapply::pbmclapply(1:length(withoutmigration_ensemble), function(j) {
    
    species_j <- withoutmigration_ensemble[[j]]
    
    species_j_realm <- lapply(1:length(species_j), function(k) {
      
      sp_k <- species_j[[k]] |> 
        dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "realm")]) |> 
        dplyr::filter(realm == realm_i) |> 
        dplyr::select(-realm)
      
    })
    
  }, mc.cores = parallel::detectCores() - 1)

  data <- future_pred_scale_function(data = ensemble_model_realm,
                                     unit = "kg",
                                     time = as.character(c("2024":"2075")),
                                     scale = "realm",
                                     score = TRUE,
                                     to_remove = 0,
                                     biais = FALSE)
  
  data |> 
    dplyr::filter(year %in% c(2024, 2030, 2050, 2075)) |> 
    dplyr::group_by(scenario, scale) |> 
    dplyr::mutate(lower = (1 - (lower / dplyr::first(biomass))) * - 100,
                  upper = (1 - (upper / dplyr::first(biomass))) * - 100,
                  biomass = (1 - (biomass / dplyr::first(biomass))) * - 100) |>
    dplyr::ungroup() |>
    dplyr::filter(year %in% c(2030, 2050, 2075)) |> 
    ggplot(aes(x = as.factor(year), y = biomass, color = scenario)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "red") +
    geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.6), size = 1) +
    scale_color_manual(values = color_scale) +
    theme_bw() +
    labs(x = "", y = "", title = realm_i) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          title = element_text(size = 14))
  
  
})
names(future_pred_ecoregion_by_realm_without) <- unique(test$realm)

dir.create("figures/future_realm_plot_without_migration_point")

ggsave("figures/future_realm_plot_without_migration_point/future_pred_tropical_atlantic.png", future_pred_ecoregion_by_realm_without[[1]], width = 4, height = 4)
ggsave("figures/future_realm_plot_without_migration_point/future_pred_temperate_australasia.png", future_pred_ecoregion_by_realm_without[[2]], width = 4, height = 4)
ggsave("figures/future_realm_plot_without_migration_point/future_pred_central_indo-pacific.png", future_pred_ecoregion_by_realm_without[[3]], width = 4, height = 4)
ggsave("figures/future_realm_plot_without_migration_point/future_pred_western_indo-pacific.png", future_pred_ecoregion_by_realm_without[[4]], width = 4, height = 4)
ggsave("figures/future_realm_plot_without_migration_point/future_pred_temperate_northern_atlantic.png", future_pred_ecoregion_by_realm_without[[5]], width = 4, height = 4)
ggsave("figures/future_realm_plot_without_migration_point/future_pred_tropical_eastern_pacific.png", future_pred_ecoregion_by_realm_without[[6]], width = 4, height = 4)
ggsave("figures/future_realm_plot_without_migration_point/future_pred_eastern_indo-pacific.png", future_pred_ecoregion_by_realm_without[[7]], width = 4, height = 4)


delta_realm_migration <- future_pred_scale_function(data = migration_ensemble[-which_species_to_remove],
                                                    unit = "kg",
                                                    time = as.character(c("2024":"2075")),
                                                    scale = "realm",
                                                    to_remove = 0,
                                                    score = TRUE,
                                                    biais = TRUE,
                                                    mc.cores = parallel::detectCores() - 1)
delta_realm_s_migration <- delta_realm_migration |> 
  dplyr::ungroup() |> 
  dplyr::select(scale, delta, scenario) |> 
  unique() |> 
  dplyr::mutate(migration = "with")

delta_realm_without_migration <- future_pred_scale_function(data = withoutmigration_ensemble[-which_species_to_remove],
                                                            unit = "kg",
                                                            time = as.character(c("2024":"2075")),
                                                            scale = "realm",
                                                            to_remove = 0,
                                                            score = TRUE,
                                                            biais = FALSE,
                                                            mc.cores = parallel::detectCores() - 1)
delta_realm_s_without_migration <- delta_realm_without_migration |> 
  dplyr::ungroup() |> 
  dplyr::select(scale, delta, scenario) |> 
  unique() |> 
  dplyr::mutate(migration = "without")

delta_realm_s_migration <- delta_realm_s_migration |> 
  dplyr::mutate(delta_with_without = abs(delta_realm_s_without_migration$delta - delta_realm_s_migration$delta))

