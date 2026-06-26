
plot_data <- function(data,
                      var_name,
                      color,
                      year,
                      legend,
                      breaks) {
  
  require(ggplot2)
  
  data_mean_scenario_model <- data |> 
    tidyr::pivot_longer(cols = -c("survey_id", "longitude", "latitude", "scenario", "model"),
                        names_to = "year",
                        values_to = "value")
  
  # data_pivot_scenario <- data_mean_scenario_model |> 
  #   dplyr::group_by(scenario, year) |> 
  #   dplyr::summarise(mean_value = mean(value),
  #                    se = sd(value),
  #                    lower = mean_value - se,
  #                    upper = mean_value + se)
  
  data_pivot_scenario <- data_mean_scenario_model |> 
    dplyr::group_by(scenario, year) |> 
    dplyr::summarise(mean_value = mean(value),
                     se = sd(value)  / sqrt(dplyr::n()),
                     lower = mean_value - qt(0.975, df = dplyr::n() - 1) * se,
                     upper = mean_value + qt(0.975, df = dplyr::n() - 1) * se)

  
  plot_data <- ggplot(data = data_pivot_scenario, aes(x = year, y = mean_value, group = scenario, color = scenario)) + 
    theme_bw() +
    theme(title = element_text(size = 10),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          strip.text.x = element_text(size = 8),
          strip.text.y = element_text(size = 20),
          legend.position = if(legend == FALSE){"none"}) +
    scale_x_discrete(breaks = breaks)
  
  if(color == "scenario")  {
    
    plot_data <- plot_data +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, size = 0.1) +
      geom_line(size = 1) +
      scale_color_manual(values = c("SSP1-2.6" = RColorBrewer::brewer.pal(n = 9, name = "Set1")[3],
                                    "SSP2-4.5" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[1],
                                    "SSP3-7.0" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[2],
                                    "SSP5-8.5" = PNWColors::pnw_palette("Bay", 3, type = "discrete")[3])) +
      labs(title = var_name, x = "", y = "", color = "")
    
  }else{
    
    plot_data <- plot_data +
      geom_line(data = data_pivot_model, aes(group = model, color = model), size = 1)
    
  }
  
  return(plot_data)
  
}
