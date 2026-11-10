delta_plot_function <- function(delta_species_bind,
                                stab_y){
  
  delta_species_bind$delta_round <- round(delta_species_bind$delta_pr)
  
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
                  y = dplyr::case_when(stability == "Decrease" ~ stab_y[1],
                                       stability == "Increase" ~ stab_y[2],
                                       stability == "Stable" ~ stab_y[3]))
  
  n_delta$scenario <- as.numeric(n_delta$scenario)
  
  
  library(ggplot2)
  
  delta_species_plot <- delta_species_bind |> 
    dplyr::mutate(stability = forcats::fct_relevel(stability, "Increase", "Stable", "Decrease")) |> 
    ggplot() + 
    theme_bw() + 
    geom_point(aes(x = as.numeric(scenario), y = delta_biomass, color = stability), position = "jitter", size = 1, alpha = 0.7) +
    scale_color_manual(values = c("Increase" = "#009E73",
                                  "Stable" = "#E69F00",
                                  "Decrease" = "#0072B2")) +
    labs(x = "", y = "log10(Biomass change + 1)", color = "") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 11), 
          legend.title = element_text(size = 10),
          strip.text.x = element_text(size = 6.5),
          strip.text.y = element_text(size = 12)) +
    ggnewscale::new_scale_colour() +
    geom_violin(aes(x = as.numeric(scenario), y = delta_biomass, group = scenario), alpha = 0.3, linewidth = 0.7, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_discrete_manual(aesthetics = "color", values = color_scale)
  
  # Test ANOVA
  anova_result <- aov(delta_biomass ~ scenario, data = delta_species_bind)
  
  # Test post-hoc Tukey
  tukey_result <- TukeyHSD(anova_result)
  tukey_table <- as.data.frame(tukey_result$scenario)
  
  diff <- tukey_table$`p adj` < 0.05
  names(diff) <- rownames(tukey_table)
  
  # Convert to letters with multcompView
  significance_letters <- multcompView::multcompLetters(diff,
                                                        Letters = letters)$Letters
  
  groupes <- names(significance_letters)
  new_order <- sort(as.numeric(groupes))
  
  # Creat new vector of sorted letters
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
    geom_text(data = n_delta,
              aes(x = scenario + 0.2, y = y, label = n, color = stability),
              size = 5,
              show.legend = FALSE) +
    scale_color_manual(values = c("#0072B2","#009E73","#E69F00")) +
    scale_x_continuous(breaks = 1:4, labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"))
  
}


delta_plot_function_model <- function(delta_species_all){
  
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
  
  # Convert to letters with multcompView
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
