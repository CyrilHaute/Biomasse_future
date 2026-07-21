############################################################################################################
####################################    Plot models predictive power   #####################################
################################    and sensitivity to hyperparameters    ##################################
############################################################################################################

#### Load data
rf_ns1 <- list.files("results/biomass_class_test_rf_hist_ns1", full.names = TRUE)
rf_ns1 <- lapply(1:length(rf_ns1), function(i) {
  
  load(rf_ns1[i])
  assign(as.character(i), all_pred)
  
})
rf_ns5 <- list.files("results/biomass_class_test_rf_hist_ns5", full.names = TRUE)
rf_ns5 <- lapply(1:length(rf_ns5), function(i) {
  
  load(rf_ns5[i])
  assign(as.character(i), all_pred)
  
})
rf_ns25 <- list.files("results/biomass_class_test_rf_hist_ns25", full.names = TRUE)
rf_ns25 <- lapply(1:length(rf_ns25), function(i) {
  
  load(rf_ns25[i])
  assign(as.character(i), all_pred)
  
})
rf_ns50 <- list.files("results/biomass_class_test_rf_hist_ns50", full.names = TRUE)
rf_ns50 <- lapply(1:length(rf_ns50), function(i) {
  
  load(rf_ns50[i])
  assign(as.character(i), all_pred)
  
})

xgboost_md6_mcw1_msl0 <- list.files("results/biomass_class_test_xgboost_hist_md6_mcw1_msl0", full.names = TRUE)
xgboost_md6_mcw1_msl0 <- lapply(1:length(xgboost_md6_mcw1_msl0), function(i) {
  
  load(xgboost_md6_mcw1_msl0[i])
  assign(as.character(i), all_pred)
  
})
xgboost_md5_mcw5_msl2 <- list.files("results/biomass_class_test_xgboost_hist_md5_mcw5_msl2", full.names = TRUE)
xgboost_md5_mcw5_msl2 <- lapply(1:length(xgboost_md5_mcw5_msl2), function(i) {
  
  load(xgboost_md5_mcw5_msl2[i])
  assign(as.character(i), all_pred)
  
})
xgboost_md4_mcw25_msl3 <- list.files("results/biomass_class_test_xgboost_hist_md4_mcw25_msl3", full.names = TRUE)
xgboost_md4_mcw25_msl3 <- lapply(1:length(xgboost_md4_mcw25_msl3), function(i) {
  
  load(xgboost_md4_mcw25_msl3[i])
  assign(as.character(i), all_pred)
  
})
xgboost_md3_mcw50_msl5 <- list.files("results/biomass_class_test_xgboost_hist_md3_mcw50_msl5", full.names = TRUE)
xgboost_md3_mcw50_msl5 <- lapply(1:length(xgboost_md3_mcw50_msl5), function(i) {
  
  load(xgboost_md3_mcw50_msl5[i])
  assign(as.character(i), all_pred)
  
})

dnn_h11_lr005_ld001 <- list.files("results/biomass_class_test_dnn_hist_h11_lr005_ld001", full.names = TRUE)
dnn_h11_lr005_ld001 <- lapply(1:length(dnn_h11_lr005_ld001), function(i) {
  
  load(dnn_h11_lr005_ld001[i])
  assign(as.character(i), all_pred)
  
})
dnn_h10_lr003_ld005 <- list.files("results/biomass_class_test_dnn_hist_h10_lr003_ld005", full.names = TRUE)
dnn_h10_lr003_ld005 <- lapply(1:length(dnn_h10_lr003_ld005), function(i) {
  
  load(dnn_h10_lr003_ld005[i])
  assign(as.character(i), all_pred)
  
})
dnn_h9_lr002_ld01 <- list.files("results/biomass_class_test_dnn_hist_h9_lr002_ld01", full.names = TRUE)
dnn_h9_lr002_ld01 <- lapply(1:length(dnn_h9_lr002_ld01), function(i) {
  
  load(dnn_h9_lr002_ld01[i])
  assign(as.character(i), all_pred)
  
})
dnn_h8_lr001_ld1 <- list.files("results/biomass_class_test_dnn_hist_h8_lr001_ld1", full.names = TRUE)
dnn_h8_lr001_ld1 <- lapply(1:length(dnn_h8_lr001_ld1), function(i) {
  
  load(dnn_h8_lr001_ld1[i])
  assign(as.character(i), all_pred)
  
})

read_sp_eco <- c(rf_ns1, rf_ns5, rf_ns25, rf_ns50, xgboost_md6_mcw1_msl0, xgboost_md5_mcw5_msl2, xgboost_md4_mcw25_msl3, xgboost_md3_mcw50_msl5, dnn_h11_lr005_ld001, dnn_h10_lr003_ld005, dnn_h9_lr002_ld01, dnn_h8_lr001_ld1)

# You can check for predicitive power for each species
# library(ggplot2)
# sp <- read_sp_eco[[1]] |> # select species number
#   dplyr::filter(validation_observed != 0) |>
#   dplyr::mutate(validation_observed = (validation_observed-min(validation_observed, na.rm = T))/(max(validation_observed, na.rm = T)-min(validation_observed, na.rm=T)),
#                 validation_predict = (validation_predict-min(validation_predict, na.rm = T))/(max(validation_predict, na.rm = T)-min(validation_predict, na.rm=T)))
# ggplot(sp,
#        aes(x = validation_predict, y = validation_observed)) +
#   theme_minimal() +
#   stat_density2d_filled() +
#   geom_abline(slope = 1, intercept = 0) +
#   annotate(geom = "text", x = 0.1, y = 0.9, color = "white",
#            label = paste0("R² = ", round(summary(lm(validation_observed ~ validation_predict, data = sp))$r.squared, 2))) +
#   theme(legend.position = "none",
#         title = element_text(face = "italic"),
#         axis.title = element_text(face = "plain")) +
#   labs(title = unique(sp$species_name))


performance <- pbmcapply::pbmclapply(1:length(read_sp_eco), function(i) {
  
  sp <- read_sp_eco[[i]]
  sp$survey_id <- as.numeric(sp$survey_id)
  sp$validation_predict <- as.numeric(sp$validation_predict)

  # Linear model between values
  lm_test <- lapply(1:length(unique(sp$cv)), function(k) {
    
    tryCatch(lm(validation_predict ~ validation_observed, data = sp[sp$cv %in% unique(sp$cv)[k],]), error = function(e) NA)
    
  })
  
  
  Pr2 <- lapply(1:length(lm_test), function(k) {
    
    sum_lm <- summary(lm_test[[k]])
    Pr2 <- sum_lm$r.squared
    
  })
  
  cor.test_pearson  <- lapply(1:length(unique(sp$cv)), function(l) {
    
    cor.test_pearson  <- tryCatch(cor.test(as.numeric(sp[sp$cv %in% unique(sp$cv)[l],]$validation_predict), 
                                           as.numeric(sp[sp$cv %in% unique(sp$cv)[l],]$validation_observed), method = 'pearson'), error = function(e) NA)
    
    Pearson <- cor.test_pearson$estimate
    
  })
  
  if(length(is.na(cor.test_pearson)[which(is.na(cor.test_pearson))]) == length(unique(sp$cv))) {
    
    cor.test_pearson  <- as.list(rep(0, length(unique(sp$cv))))
    
  }
  
  cor.test_spearman  <- lapply(1:length(unique(sp$cv)), function(l) {
    
    cor.test_spearman <- tryCatch(cor.test(as.numeric(sp[sp$cv %in% unique(sp$cv)[l],]$validation_predict), 
                                           as.numeric(sp[sp$cv %in% unique(sp$cv)[l],]$validation_observed), method = 'spearman'), error = function(e) NA)
    
    Spearman <- cor.test_spearman$estimate
    
  })
  
  if(length(is.na(cor.test_spearman)[which(is.na(cor.test_spearman))]) == length(unique(sp$cv))) {
    
    cor.test_spearman  <- as.list(rep(0, length(unique(sp$cv))))
    
  }
  
  metric_summary <- lapply(1:length(unique(sp$cv)), function(j) {
    
    data.frame(species_name = unique(sp$species_name),
               R2 = unlist(Pr2[[j]]),
               pearson = unlist(cor.test_pearson[[j]]),
               spearman = unlist(cor.test_spearman[[j]]),
               cv = unique(sp$cv)[j])
    
  })
  metric_summary <- do.call(rbind, metric_summary)
  
  metric_summary <- metric_summary |> 
    dplyr::group_by(species_name) |> 
    dplyr::summarise(r2 = mean(R2, na.rm = TRUE),
                     r2_sd = round(sd(R2, na.rm = TRUE), digits = 2),
                     sd_pearson = round(sd(pearson, na.rm = TRUE), digits = 2),
                     pearson = mean(pearson, na.rm = TRUE),
                     sd_spearman = round(sd(spearman, na.rm = TRUE), digits = 2),
                     spearman = mean(spearman, na.rm = TRUE)) |> 
    dplyr::mutate(model = unique(sp$model),
                  min.node.size = paste0("min.node.size = ", unique(sp$min.node.size)),
                  md_mcw_msl = paste0("md = ", unique(sp$max_depth), ", mcw = ", unique(sp$min_child_weight), ", msl = ", unique(sp$min_split_loss)),
                  hid_lr_lb = paste0("hid = ", unique(sp$hidden), ", lr = ", unique(sp$lr), ", lb = ", unique(sp$lambda)))

    return(metric_summary)
  
}, mc.cores = parallel::detectCores() - 1)
performance_bind <- do.call(rbind, performance)

perf_model <- performance_bind |> 
  dplyr::group_by(model, min.node.size, md_mcw_msl, hid_lr_lb) |> 
  dplyr::summarise(medianr2 = median(r2, na.rm = TRUE),
                   q05r2 = quantile(r2, probs = 0.05, na.rm = TRUE),
                   q95r2 = quantile(r2, probs = 0.95, na.rm = TRUE),
                   medianpearson = median(pearson, na.rm = TRUE),
                   q05pearson  = quantile(pearson , probs = 0.05, na.rm = TRUE),
                   q95pearson  = quantile(pearson , probs = 0.95, na.rm = TRUE),
                   medianspearman = median(spearman, na.rm = TRUE),
                   q05spearman  = quantile(spearman , probs = 0.05, na.rm = TRUE),
                   q95spearman  = quantile(spearman , probs = 0.95, na.rm = TRUE))

performance_bind <- performance_bind |> 
  dplyr::inner_join(perf_model, by = c("model", "min.node.size", "md_mcw_msl", "hid_lr_lb"))

performance_bind_rf <- performance_bind |> 
  dplyr::filter(min.node.size == "min.node.size = 50")

performance_bind_xgboost <- performance_bind |> 
  dplyr::filter(md_mcw_msl == "md = 4, mcw = 25, msl = 3")

performance_bind_dnn <- performance_bind |> 
  dplyr::filter(hid_lr_lb == "hid = 8, lr = 0.001, lb = 0.1")

performance_bind_save <- performance_bind_rf |> 
  dplyr::full_join(performance_bind_xgboost) |> 
  dplyr::full_join(performance_bind_dnn)

performance_bind_save |> 
  dplyr::select(model, medianpearson, q05pearson, q95pearson) |> 
  unique()

save(performance_bind_save, file = "results/performance_bind.Rdata")


performance_bind[performance_bind$model == "rf",]$model <- "RF"
performance_bind[performance_bind$model == "xgboost",]$model <- "Xgboost"
performance_bind[performance_bind$model == "dnn",]$model <- "DNN"

performance_bind <- performance_bind |> 
  dplyr::mutate(medianr2_label = paste0("Median = ", round(medianr2, 2)),
                medianpearson_label = paste0("Median = ", round(medianpearson, 2)),
                medianspearman_label = paste0("Median = ", round(medianspearman, 2)))

library(ggplot2)

pearson_dist_model_rf <- performance_bind |> 
  dplyr::filter(model == "RF") |> 
  dplyr::mutate(min.node.size = as.character(min.node.size),
                min.node.size = forcats::fct_relevel(min.node.size, "min.node.size = 1", "min.node.size = 5", "min.node.size = 25", "min.node.size = 50")) |>  
  ggplot(aes(x = pearson)) +
  facet_grid(model ~ min.node.size) +
  geom_histogram(aes(fill = model), position = "identity", color = "#e9ecef", binwidth = 0.05) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9, name = "Set1")[1]) +
  geom_vline(aes(xintercept = medianpearson), color = "black", linetype = "dashed", size = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 10)) +
  labs(x = "", y = "", title = "")


pearson_dist_model_xgboost <- performance_bind |> 
  dplyr::filter(model == "Xgboost") |> 
  dplyr::mutate(md_mcw_msl = forcats::fct_relevel(md_mcw_msl, "md = 6, mcw = 1, msl = 0", "md = 5, mcw = 5, msl = 2", "md = 4, mcw = 25, msl = 3", "md = 3, mcw = 50, msl = 5")) |> 
  ggplot(aes(x = pearson)) +
  facet_grid(model ~ md_mcw_msl) +
  geom_histogram(aes(fill = model), position = "identity", color = "#e9ecef", binwidth = 0.05) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9, name = "Set1")[2]) +
  geom_vline(aes(xintercept = medianpearson), color = "black", linetype = "dashed", size = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 10)) +
  labs(x = "", y = "Count", title = "")

pearson_dist_model_dnn <- performance_bind |> 
  dplyr::filter(model == "DNN") |> 
  dplyr::mutate(hid_lr_lb = forcats::fct_relevel(hid_lr_lb, "hid = 11, lr = 0.005, lb = 0.001", "hid = 10, lr = 0.003, lb = 0.005", "hid = 9, lr = 0.002, lb = 0.01", "hid = 8, lr = 0.001, lb = 0.1")) |>
  ggplot(aes(x = pearson)) +
  facet_grid(model ~ hid_lr_lb) +
  geom_histogram(aes(fill = model), position = "identity", color = "#e9ecef", binwidth = 0.05) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9, name = "Set1")[3]) +
  geom_vline(aes(xintercept = medianpearson), color = "black", linetype = "dashed", size = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 10)) +
  labs(x = "Pearson", y = "", title = "")

pearson_dist_model <- patchwork::wrap_plots(pearson_dist_model_rf / pearson_dist_model_xgboost / pearson_dist_model_dnn)
ggsave("figures/pearson_dist_model2.png", pearson_dist_model, width = 10, height = 9)
