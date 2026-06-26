list_habitat <- c(list.files("data/raw/covariates/habitat/Allen", pattern = "reefextent.gpkg", recursive = TRUE, full.names = TRUE))

shapes <- lapply(list_habitat, terra::vect)

mask <- do.call(rbind, shapes)

terra::writeVector(mask, "data/raw/covariates/habitat/mask.shp")

mask <- terra::vect("data/raw/covariates/habitat/mask.shp")

ph_test <- terra::rast("data/raw/covariates/environmental/CanESM5_ssp245_phos_05.nc")
terra::crs(ph_test) <- terra::crs(mask)

mask_ph <- terra::mask(ph_test[[1]], mask = mask)
crds_mask <- terra::crds(mask_ph)

terra::plot(mask_ph)

test <- terra::extract(ph_test[[1]], crds_mask)
