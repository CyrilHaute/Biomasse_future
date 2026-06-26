delta_plot_function <- function(delta_species_bind){
  
  delta_species_bind$delta_round <- round(delta_species_bind$delta)
  
  delta_species_bind <- delta_species_bind |> 
    dplyr::mutate(stability = ifelse(delta_round %in% c(-5:5), "Stable",
                                     ifelse(delta_round < -5, "Decrease",
                                            ifelse(delta_round > 5, "Increase", NA)))) |> 
    dplyr::mutate(scenario = dplyr::recode(scenario,
                                           "SSP1-2.6" = "1",
                                           "SSP2-4.5" = "2",
                                           "SSP3-7.0" = "3",
                                           "SSP5-8.5" = "4"))
  
  n_delta <- delta_species_bind |> 
    dplyr::group_by(scenario, stability) |> 
    dplyr::summarise(n = dplyr::n()) |> 
    dplyr::mutate(scenario = dplyr::recode(scenario,
                                           "SSP1-2.6" = "1",
                                           "SSP2-4.5" = "2",
                                           "SSP3-7.0" = "3",
                                           "SSP5-8.5" = "4"),
                  y = dplyr::case_when(stability == "Decrease" ~ 150,
                                       stability == "Increase" ~ 200,
                                       stability == "Stable" ~ 175))
  
  n_delta$scenario <- as.numeric(n_delta$scenario)
  
  
  library(ggplot2)
  
  delta_species_plot <- delta_species_bind |> 
    dplyr::mutate(stability = forcats::fct_relevel(stability, "Increase", "Stable", "Decrease")) |> 
    ggplot() + 
    theme_bw() + 
    geom_point(aes(x = as.numeric(scenario), y = delta, color = stability), position = "jitter", size = 1, alpha = 0.7) +
    scale_color_manual(values = c("Increase" = RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")[c(10)],
                                  "Stable" = RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")[c(4)],
                                  "Decrease" = RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")[c(1)])) +
    labs(x = "", y = "Biomass change (%)", color = "") +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 11), 
          legend.title = element_text(size = 10),
          strip.text.x = element_text(size = 6.5),
          strip.text.y = element_text(size = 10)) +
    ggnewscale::new_scale_colour() +
    geom_violin(aes(x = as.numeric(scenario), y = delta, fill = scenario), alpha = 0.5, linewidth = 0.5, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_discrete_manual(aesthetics = "fill", values = color_scale)
  
  # Test ANOVA
  anova_result <- aov(delta ~ scenario, data = delta_species_bind)
  
  # Test post-hoc Tukey
  tukey_result <- TukeyHSD(anova_result)
  tukey_table <- as.data.frame(tukey_result$scenario)
  
  diff <- tukey_table$`p adj` < 0.05
  names(diff) <- rownames(tukey_table)
  
  # Conversion en lettres avec multcompView
  significance_letters <- multcompView::multcompLetters(diff,
                                                        Letters = letters)$Letters
  
  groupes <- names(significance_letters)
  new_order <- sort(as.numeric(groupes))
  
  # Création d'un nouveau vecteur de lettres ordonnées
  new_letters <- setNames(letters[seq_along(new_order)], new_order)

  y <- delta_species_bind |>
    dplyr::group_by(scenario) |>
    dplyr::summarise(y = 100) |>
    dplyr::ungroup()
  
  label_data <- data.frame(
    scenario = names(new_letters),
    Letter = new_letters
  ) |>
    dplyr::inner_join(y)
  
  label_data <- label_data |> 
    dplyr::arrange(scenario) |> 
    dplyr::mutate(x = scenario) |> 
    dplyr::mutate(x = dplyr::recode(x,
                                    "SSP1-2.6" = "1",
                                    "SSP2-4.5" = "2",
                                    "SSP3-7.0" = "3",
                                    "SSP5-8.5" = "4"))
  
  delta_species_plot_text <- delta_species_plot + 
    # geom_text(data = label_data,
    #           aes(x = as.numeric(x) - 0.2, y = y, label = Letter),
    #           inherit.aes = FALSE,
    #           size = 6) + 
    geom_text(data = n_delta,
              aes(x = scenario + 0.2, y = y, label = n, color = stability),
              size = 4.5, 
              show.legend = FALSE) +
    scale_color_manual(values = RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")[c(1,10,4)]) +
    scale_x_continuous(breaks = 1:4, labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"))
  
}


delta_plot_function_model <- function(delta_species_all){
  
  # delta_species_all$delta_round <- round(delta_species_all$delta)
  delta_species_all$delta_round <- delta_species_all$delta
  
  delta_species_all <- delta_species_all |> 
    dplyr::mutate(stability = ifelse(delta_round %in% c(-5:5), "Stable",
                                     ifelse(delta_round < -5, "Decrease",
                                            ifelse(delta_round > 5, "Increase", NA)))) |>
    dplyr::mutate(scenario = dplyr::recode(scenario,
                                           "SSP1-2.6" = "1",
                                           "SSP2-4.5" = "2",
                                           "SSP3-7.0" = "3",
                                           "SSP5-8.5" = "4"))

  
  library(ggplot2)
  
  delta_species_plot <- delta_species_all |> 
    dplyr::mutate(model = forcats::fct_reorder(model, delta, .desc = TRUE)) |> 
    ggplot() + 
    theme_bw() + 
    geom_boxplot(aes(x = scenario, y = delta, fill = model), outlier.size = 0.7) +
    scale_fill_manual(values = c("hum" = PNWColors::pnw_palette("Bay", 2, type = "discrete")[2],
                                 "env" = RColorBrewer::brewer.pal(n = 9, name = "Set1")[3],
                                 "full" = PNWColors::pnw_palette("Bay", 2, type = "discrete")[1])) +
    labs(x = "", y = "Biomass change (%)", fill = "Model type") +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 11), 
          legend.title = element_text(size = 10),
          strip.text.x = element_text(size = 6.5),
          strip.text.y = element_text(size = 10))
  
  # Test ANOVA
  anova_result <- aov(delta ~ interaction(model, scenario), data = delta_species_all)
  
  # Test post-hoc Tukey
  tukey_result <- TukeyHSD(anova_result)
  tukey_table <- as.data.frame(tukey_result$`interaction(model, scenario)`)
  
  diff <- tukey_table$`p adj` < 0.05
  names(diff) <- rownames(tukey_table)
  
  # Conversion en lettres avec multcompView
  significance_letters <- multcompView::multcompLetters(diff,
                                                        Letters = letters)$Letters
  
  y <- delta_species_all |>
    dplyr::group_by(scenario, model) |>
    dplyr::summarise(y = 100) |>
    dplyr::ungroup() |> 
    dplyr::mutate(interaction = paste0(model, ".", scenario))
  
  label_data <- data.frame(
    interaction = names(significance_letters),
    Letter = significance_letters
  ) |>
    dplyr::inner_join(y)
  
  label_data <- label_data |> 
    dplyr::arrange(scenario) |> 
    dplyr::mutate(x = scenario) |> 
    dplyr::mutate(x = dplyr::case_when(model == "hum" ~ as.numeric(x) - 0.3,
                                       model == "full" ~ as.numeric(x) + 0.3,
                                       model == "env" ~ as.numeric(x)))
  
  delta_species_plot_text <- delta_species_plot + 
    geom_text(data = label_data,
              aes(x = as.numeric(x), y = y, label = Letter),
              inherit.aes = FALSE,
              size = 6) +
    scale_x_discrete(breaks = "1":"4", labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"))
  
}

iucn_plot_function <- function(delta_species_bind){
  
  delta_species_bind$delta_round <- round(delta_species_bind$delta)
  
  delta_species_bind <- delta_species_bind |> 
    dplyr::mutate(
      IUCN = dplyr::case_when(
        IUCN_category %in% c("DD", "NE") ~ "DDNE",
        delta > 0 ~ "LC",
        delta > -30 ~ "NT",
        delta <= -80 ~ "CR",
        delta <= -50 ~ "EN",
        delta <= -30 ~ "VU"),
      # IUCN = dplyr::case_when(
      #   IUCN_inferred_Loiseau23 == "No Status" ~ "DDNE",
      #   delta > -30 ~ "Non-Threatened",
      #   delta <= -30 ~ "Threatened"),
                  scenario = dplyr::recode(scenario,
                                           "SSP1-2.6" = "1",
                                           "SSP2-4.5" = "2",
                                           "SSP3-7.0" = "3",
                                           "SSP5-8.5" = "4"))
  
  n_delta <- delta_species_bind |> 
    dplyr::group_by(scenario, IUCN) |> 
    dplyr::summarise(n = dplyr::n()) |> 
    dplyr::mutate(scenario = dplyr::recode(scenario,
                                           "SSP1-2.6" = "1",
                                           "SSP2-4.5" = "2",
                                           "SSP3-7.0" = "3",
                                           "SSP5-8.5" = "4"),
                  y = dplyr::case_when(IUCN == "VU" ~ 70,
                                       IUCN == "CR" ~ 100,
                                       IUCN == "EN" ~ 85,
                                       IUCN == "NT" ~ 55,
                                       IUCN == "LC" ~ 40,
                                       IUCN == "DDNE" ~ 25),
                  # y = dplyr::case_when(IUCN == "Non-Threatened" ~ 80,
                  #                      IUCN == "Threatened" ~ 100,
                  #                      IUCN == "DDNE" ~ 60)
                  )
  
  n_delta$scenario <- as.numeric(n_delta$scenario)

  
  library(ggplot2)
  
  delta_species_plot <- delta_species_bind |> 
    dplyr::mutate(IUCN = forcats::fct_relevel(IUCN, "CR", "EN", "VU", "NT", "LC", "DDNE")) |>
    # dplyr::mutate(IUCN = forcats::fct_relevel(IUCN, "Threatened", "Non-Threatened", "DDNE")) |>
    ggplot() + 
    theme_bw() + 
    geom_point(aes(x = scenario, y = delta, color = IUCN), position = "jitter", size = 1) +
    scale_y_continuous(n.breaks = 8) + 
    scale_color_manual(values = c("#D81E05", "#FC7F3F", "#F9E814", "#CCE226", "#60C659", "grey")) +
    # scale_color_manual(values = c("#D81E05", "#FC7F3F", "#60C659")) +
    labs(x = "", y = "Delta Biomass (%)", color = "") +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 11), 
          legend.title = element_text(size = 10),
          strip.text.x = element_text(size = 6.5),
          strip.text.y = element_text(size = 10)) +
  #   ggnewscale::new_scale_colour() +
    # geom_violin(aes(x = as.numeric(scenario), y = delta, color = scenario), alpha = 0, linewidth = 2, show.legend = FALSE) +
    geom_violin(aes(x = as.numeric(scenario), y = delta, group = scenario), alpha = 0, linewidth = 1, show.legend = FALSE)
  #   scale_color_manual(aesthetics = "color", values = color_scale)
  
  # Test ANOVA
  anova_result <- aov(delta ~ scenario, data = delta_species_bind)
  
  # Test post-hoc Tukey
  tukey_result <- TukeyHSD(anova_result)
  tukey_table <- as.data.frame(tukey_result$scenario)
  
  diff <- tukey_table$`p adj` < 0.05
  names(diff) <- rownames(tukey_table)
  
  # Conversion en lettres avec multcompView
  significance_letters <- multcompView::multcompLetters(diff,
                                                        Letters = letters)$Letters
  
  groupes <- names(significance_letters)
  new_order <- sort(as.numeric(groupes))
  
  # Création d'un nouveau vecteur de lettres ordonnées
  new_letters <- setNames(letters[seq_along(new_order)], new_order)
  
  y <- delta_species_bind |>
    dplyr::group_by(scenario) |>
    dplyr::summarise(y = 100) |>
    dplyr::ungroup()
  
  label_data <- data.frame(
    scenario = names(new_letters),
    Letter = new_letters
  ) |>
    dplyr::inner_join(y)
  
  label_data <- label_data |> 
    dplyr::arrange(scenario) |> 
    dplyr::mutate(x = scenario) |> 
    dplyr::mutate(x = dplyr::recode(x,
                                    "SSP1-2.6" = "1",
                                    "SSP2-4.5" = "2",
                                    "SSP3-7.0" = "3",
                                    "SSP5-8.5" = "4"))
  
  delta_species_plot_text <- delta_species_plot + 
    ggnewscale::new_scale_colour() +
    geom_text(data = label_data,
              aes(x = as.numeric(x) - 0.2, y = y, label = Letter),
              inherit.aes = FALSE,
              size = 6) + 
    geom_text(data = n_delta,
              aes(x = scenario + 0.2, y = y, label = n, color = IUCN),
              size = 4.5, 
              show.legend = FALSE) + 
    scale_color_manual(values = c("CR" = "#D81E05",
                                  "EN" = "#FC7F3F",
                                  "VU" = "#F9E814",
                                  "NT" = "#CCE226",
                                  "LC" = "#60C659",
                                  "DDNE" = "grey")) +
    # scale_color_manual(values = c("Threatened" = "#D81E05", 
    #                               "Non-Threatened" = "#FC7F3F", 
    #                               "DDNE" = "#60C659")) +
    scale_x_discrete(breaks = 1:4, labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"))
  
}
