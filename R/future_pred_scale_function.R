
future_pred_scale_function <- function(data,
                                       scale = "",
                                       all_species = FALSE,
                                       group_name = NULL,
                                       group_by = NULL,
                                       group_by_data,
                                       unit,
                                       time,
                                       score = FALSE,
                                       to_remove,
                                       biais){
  
  # scale = ""
  # all_species = FALSE
  # group_name = NULL
  # group_by = NULL
  # score = FALSE

  future_pred <- pbmcapply::pbmclapply(1:length(scenarii), function(i) {
    
    sc_i <- lapply(data, '[[', i)
    
    sc_i <- do.call(rbind, sc_i)
    
    if(!is.null(group_name) & is.null(group_by)){
      
      if(unique(sc_i$species_name) != group_name){
        
        stop("argument `species_name` different to species name in input data")
        
      }
      
    }

    if(score == FALSE){
      
      sc_i <- sc_i[,c("survey_id", "species_name", "biomass" , "validation_predict", colnames(sc_i)[which(grepl(scenarii[i], colnames(sc_i)))])]
      
      colnames(sc_i)[which(grepl(scenarii[i], colnames(sc_i)))] <- gsub(pattern = paste0("_", scenarii[i]), replacement = "", colnames(sc_i)[which(grepl(scenarii[i], colnames(sc_i)))])
      
      colnames(sc_i)[colnames(sc_i) == "validation_predict"] <- "2024"

      
      if(biais == TRUE) {

        biais_correction <- sc_i$`2025` - sc_i$`2024`
        # biais_correction[which(is.na(biais_correction))] <- 0

        sc_i[,colnames(sc_i) %in% c("2025":"2100")] <- sc_i[,colnames(sc_i) %in% c("2025":"2100")] - biais_correction

      }

      # sc_i <- sc_i |>
      #   dplyr::mutate(dplyr::across(`2024`:`2100`, ~ dplyr::case_when(. > max(decile_save$biomass_class) ~ 29,
      #                                                                 . < min(decile_save$biomass_class) ~ 0,
      #                                                                 TRUE ~ .)))
      
      sc_i <- sc_i |>
        dplyr::filter(
          dplyr::if_all(`2024`:`2100`, ~ .x >= 0 & .x <= 29)
        )
      
      sc_i <- sc_i |>
        dplyr::select(-biomass)
        

      if(unit == "g"){

        sc_i <- sc_i |>
          dplyr::mutate(dplyr::across(where(is.numeric), ~ decile_save$biomass[match(., decile_save$biomass_class)]))

      }else{

        sc_i <- sc_i |>
          dplyr::mutate(dplyr::across(where(is.numeric), ~ decile_save$biomass[match(., decile_save$biomass_class)] * 10^-3))

      }
      
    }else{
      
      if(biais == TRUE) {
        
        biais_correction <- sc_i$`2025` - sc_i$`2024`
        # biais_correction[which(is.na(biais_correction))] <- 0
        
        sc_i[,colnames(sc_i) %in% c("2025":"2100")] <- sc_i[,colnames(sc_i) %in% c("2025":"2100")] - biais_correction
        
        sum_biomass <- lapply(1:length("2024":"2100"), function(j) { 
          
          # data.frame(year = c("2024":"2100")[j],
          #            sum_biomass = sum(sc_i[colnames(sc_i) %in% c("2024":"2100")[j]]))
          
          data_year_j <- sc_i[colnames(sc_i) %in% c("species_name", c("2024":"2100")[j])] |> 
            dplyr::mutate(year = c("2024":"2100")[j])
          colnames(data_year_j)[2] <- "biomass"
          data_year_j <- data_year_j |> 
            dplyr::group_by(year, species_name) |> 
            dplyr::mutate(sum_biomass = sum(biomass)) |> 
            dplyr::select(-biomass) |> 
            unique()
          
        })
        sum_biomass_bind <- do.call(rbind, sum_biomass)
        
        # year_to_correct <- sum_biomass_bind |> 
        #   dplyr::filter(sum_biomass <= 0) |> 
        #   dplyr::select(year) |> 
        #   unlist()
        
        year_sp_to_correct <- sum_biomass_bind |> 
          dplyr::filter(sum_biomass <= 0) |> 
          dplyr::select(year, species_name)
        
        if(nrow(year_sp_to_correct) != 0) {
          
          test <- lapply(1:length(unique(year_sp_to_correct$species_name)), function(j) {
            
            sp_j <- sc_i |> 
              dplyr::filter(species_name == unique(year_sp_to_correct$species_name)[j])
            
            sp_j[,colnames(sp_j) %in% year_sp_to_correct[year_sp_to_correct$species_name == unique(year_sp_to_correct$species_name)[j],]$year] <- as.data.frame(apply(sp_j[,colnames(sp_j) %in% year_sp_to_correct[year_sp_to_correct$species_name == unique(year_sp_to_correct$species_name)[j],]$year], 2, function(v) v - sum(v) / length(v)))
            
          })
          
          for(j in 1:length(test)) {
            
            sc_i[sc_i$species_name == unique(year_sp_to_correct$species_name)[j],][,colnames(sc_i) %in% year_sp_to_correct[year_sp_to_correct$species_name == unique(year_sp_to_correct$species_name)[j],]$year] <- test[[j]]
            
          }
          
        }
        
        
        # if(length(year_to_correct) != 0) {
        #   
        #   sc_i[,colnames(sc_i) %in% year_to_correct] <- as.data.frame(apply(sc_i[,colnames(sc_i) %in% year_to_correct], 2, function(v) v - sum(v) / length(v)))
        #   
        # }

      }
      
      sc_i <- sc_i[,!colnames(sc_i) %in% c("scenario", "model")]
      
    }

    if(all_species == TRUE){
      
      if(scale == ""){
        
        if(is.null(group_by)){
          
          sc_i <- sc_i |>
            dplyr::group_by(species_name) |>
            dplyr::summarise(dplyr::across(dplyr::all_of(time), sum)) |>
            dplyr::mutate(delta = (get(time[length(time)]) * 100 / get(time[1])) - 100) |>
            tidyr::pivot_longer(time, names_to = "year", values_to = "biomass") |>
            dplyr::mutate(scenario = scenarii[i])
          
        }else{
          
          sc_i <- sc_i |> 
            dplyr::left_join(group_by_data)
          
          colnames(sc_i)[colnames(sc_i) == group_by] <- "group_by"
          
          if(is.null(group_name)){
            
            sc_i <- sc_i |> 
              dplyr::group_by(group_by) |> 
              dplyr::summarise(dplyr::across(dplyr::all_of(time), sum)) |>
              tidyr::pivot_longer(time, names_to = "year", values_to = "biomass") |>
              dplyr::mutate(scenario = scenarii[i])
            
          }else{
            
            sc_i <- sc_i |> 
              dplyr::group_by(species_name, group_by) |> 
              dplyr::summarise(dplyr::across(dplyr::all_of(time), sum)) |>
              dplyr::mutate(delta = (get(time[length(time)]) * 100 / get(time[1])) - 100) |>
              tidyr::pivot_longer(time, names_to = "year", values_to = "biomass") |>
              dplyr::mutate(scenario = scenarii[i])
            
          }
          
        }
        
      }else{
        
        sc_i <- sc_i |> 
          dplyr::inner_join(rls_surveys[,which(colnames(rls_surveys) %in% c("survey_id", scale))])
        colnames(sc_i)[length(colnames(sc_i))] <- "scale"
        
        sc_i <- sc_i |> 
          dplyr::group_by(species_name, scale) |> 
          dplyr::summarise(dplyr::across(dplyr::all_of(time), sum))
        
        sc_i <- sc_i |> 
          tidyr::pivot_longer(-c("species_name", scale), names_to = "year", values_to = "biomass") |> 
          dplyr::mutate(scenario = scenarii[i])
        
      }
      
    }else{
      
      if(scale == ""){
        
        if(is.null(group_by)){

          # sc_i <- sc_i |>
          #   dplyr::group_by(survey_id) |>
          #   dplyr::summarise(dplyr::across(dplyr::all_of(time), sum)) |>
          #   dplyr::summarise(dplyr::across(dplyr::all_of(time), sum)) |>
          #   dplyr::mutate(delta = (get(time[length(time)]) * 100 / get(time[1])) - 100) |>
          #   tidyr::pivot_longer(time, names_to = "year", values_to = "biomass") |>
          #   dplyr::mutate(scenario = scenarii[i],
          #                 model = unique(data[[1]][[1]]$model))
          
          
          sc_i <- sc_i |> 
            tidyr::pivot_longer(cols = -c("survey_id", "species_name"),
                                names_to = "year",
                                values_to = "value") |> 
            dplyr::filter(year %in% time)
          
          # sc_i <- sc_i |> 
          #   dplyr::group_by(year) |> 
          #   dplyr::summarise(biomass = mean(value),
          #                    se = sd(value) / sqrt(dplyr::n()),
          #                    lower = biomass - qt(0.975, df = dplyr::n() - 1) * se,
          #                    upper = biomass + qt(0.975, df = dplyr::n() - 1) * se)
          sc_i <- sc_i |> 
            dplyr::group_by(year) |> 
            dplyr::summarise(biomass = sum(value),
                             var_sum = sum((value - mean(value))^2) / (dplyr::n() - 1),
                             se_sum = sqrt(dplyr::n() * var_sum),
                             t_value = qt(0.975, df = dplyr::n() - 1),
                             lower = biomass - t_value * se_sum,
                             upper = biomass + t_value * se_sum)
          
          delta <- (sc_i[sc_i$year == time[length(time)],]$biomass * 100) / sc_i[sc_i$year == time[1],]$biomass - 100

          sc_i <- sc_i |> 
            dplyr::mutate(delta = delta,
                          scenario = scenarii[i],
                          model = unique(data[[1]][[1]]$model))
          

        }else{
          
          sc_i <- sc_i |> 
            dplyr::left_join(group_by_data)
          
          sc_i <- sc_i[complete.cases(sc_i),]
          
          colnames(sc_i)[colnames(sc_i) == group_by] <- "group_by"
          
          if(is.null(group_name)){
            # 
            # sc_i <- sc_i |>
            #   dplyr::group_by(group_by) |>
            #   dplyr::summarise(dplyr::across(dplyr::all_of(time), sum)) |>
            #   dplyr::mutate(delta = (get(time[length(time)]) * 100 / get(time[1])) - 100) |>
            #   tidyr::pivot_longer(time, names_to = "year", values_to = "biomass") |>
            #   dplyr::mutate(scenario = scenarii[i])
            
            sc_i <- sc_i |> 
            tidyr::pivot_longer(cols = -c("survey_id", "species_name", "group_by"),
                                names_to = "year",
                                values_to = "value") |> 
            dplyr::filter(year %in% time)
          
          # sc_i <- sc_i |> 
          #   dplyr::group_by(year) |> 
          #   dplyr::summarise(biomass = mean(value),
          #                    se = sd(value) / sqrt(dplyr::n()),
          #                    lower = biomass - qt(0.975, df = dplyr::n() - 1) * se,
          #                    upper = biomass + qt(0.975, df = dplyr::n() - 1) * se)
          sc_i <- sc_i |> 
            dplyr::group_by(year, group_by) |> 
            dplyr::summarise(biomass = sum(value),
                             var_sum = sum((value - mean(value))^2) / (dplyr::n() - 1),
                             se_sum = sqrt(dplyr::n() * var_sum),
                             t_value = qt(0.975, df = dplyr::n() - 1),
                             lower = biomass - t_value * se_sum,
                             upper = biomass + t_value * se_sum)
          
          delta <- (sc_i[sc_i$year == time[length(time)],]$biomass * 100) / sc_i[sc_i$year == time[1],]$biomass - 100
          
          sc_i <- sc_i |> 
            dplyr::mutate(delta = delta,
                          scenario = scenarii[i],
                          model = unique(data[[1]][[1]]$model))

          }else{
            
            sc_i <- sc_i |> 
              dplyr::group_by(group_by) |> 
              dplyr::summarise(dplyr::across(dplyr::all_of(time), sum)) |>
              dplyr::mutate(delta = (get(time[length(time)]) * 100 / get(time[1])) - 100) |>
              tidyr::pivot_longer(time, names_to = "year", values_to = "biomass") |>
              dplyr::mutate(scenario = scenarii[i])
            
          }
          
        }
        
      }else{
        
        if(scale == "survey_id") {
          
          sc_i <- sc_i
          colnames(sc_i)[colnames(sc_i) == "survey_id"] <- "scale"
          
          sc_i <- sc_i |> 
            tidyr::pivot_longer(cols = -c("species_name", "scale"),
                                names_to = "year",
                                values_to = "value") |> 
            dplyr::filter(year %in% time)
          
        }else{
          
          sc_i <- sc_i |> 
            dplyr::inner_join(rls_surveys[,which(colnames(rls_surveys) %in% c("survey_id", scale))])
          colnames(sc_i)[length(colnames(sc_i))] <- "scale"
          
          sc_i <- sc_i |> 
            tidyr::pivot_longer(cols = -c("survey_id", "species_name", "scale"),
                                names_to = "year",
                                values_to = "value") |> 
            dplyr::filter(year %in% time)
          
        }

        # to_remove <- sc_i |> 
        #   dplyr::select(survey_id, scale) |> 
        #   unique() |> 
        #   dplyr::group_by(scale) |> 
        #   dplyr::summarise(n = dplyr::n()) |> 
        #   dplyr::filter(n > to_remove)
        # 
        # sc_i <- sc_i |> 
        #   dplyr::filter(scale %in% to_remove$scale)
        
        # scale <- sc_i
        
        sc_i <- sc_i |> 
          dplyr::group_by(year, scale) |> 
          dplyr::summarise(biomass = sum(value),
                           var_sum = sum((value - mean(value))^2) / (dplyr::n() - 1),
                           se_sum = sqrt(dplyr::n() * var_sum),
                           t_value = qt(0.975, df = dplyr::n() - 1),
                           lower = biomass - t_value * se_sum,
                           upper = biomass + t_value * se_sum)
        
        delta <- (sc_i[sc_i$year == time[length(time)],]$biomass * 100) / sc_i[sc_i$year == time[1],]$biomass - 100
        
        sc_i <- sc_i |> 
          dplyr::mutate(delta = delta,
                        scenario = scenarii[i])

        # sc_i <- sc_i |> 
        #   dplyr::group_by(scale) |> 
        #   dplyr::summarise(dplyr::across(dplyr::all_of(time), sum)) |> 
        #   dplyr::mutate(delta = (get(time[length(time)]) * 100 / get(time[1])) - 100)
        # 
        # sc_i <- sc_i |> 
        #   tidyr::pivot_longer(!c(scale, "delta"), names_to = "year", values_to = "biomass") |> 
        #   dplyr::mutate(scenario = scenarii[i])

      }
      
    }
    
    sc_i$year <- as.integer(sc_i$year)
    sc_i$scenario <- as.factor(sc_i$scenario)
    
    return(sc_i)
    
  }, mc.cores = 1)
  
  if(score == FALSE) {
    
    future_pred_bind <- purrr::reduce(future_pred, dplyr::full_join) |> 
      dplyr::mutate(scenario = dplyr::recode(scenario,
                                             "ssp126" = "SSP1-2.6",
                                             "ssp245" = "SSP2-4.5",
                                             "ssp370" = "SSP3-7.0",
                                             "ssp585" = "SSP5-8.5"),
                    model = dplyr::case_when(model == "rf" ~ "RF",
                                             model == "dnn" ~ "DNN",
                                             model == "xgboost" ~ "Xgboost"))
    
  }else{
    
    future_pred_bind <- purrr::reduce(future_pred, dplyr::full_join) |> 
      dplyr::mutate(scenario = dplyr::recode(scenario,
                                             "ssp126" = "SSP1-2.6",
                                             "ssp245" = "SSP2-4.5",
                                             "ssp370" = "SSP3-7.0",
                                             "ssp585" = "SSP5-8.5"))
    
  }

}








future_pred_scale_survey_function <- function(data,
                                       scale = "",
                                       all_species = FALSE,
                                       group_name = NULL,
                                       group_by = NULL,
                                       group_by_data,
                                       unit,
                                       time,
                                       score = FALSE,
                                       to_remove){
  
  # scale = ""
  # all_species = FALSE
  # group_name = NULL
  # group_by = NULL
  # score = FALSE
  
  future_pred <- pbmcapply::pbmclapply(1:length(scenarii), function(i) {
    
    sc_i <- lapply(data, '[[', i)
    
    sc_i <- do.call(rbind, sc_i)
    
    if(!is.null(group_name) & is.null(group_by)){
      
      if(unique(sc_i$species_name) != group_name){
        
        stop("argument `species_name` different to species name in input data")
        
      }
      
    }
    
    if(score == FALSE){
      
      sc_i <- sc_i[,c("survey_id", "species_name", "validation_predict", colnames(sc_i)[which(grepl(scenarii[i], colnames(sc_i)))])]
      
      colnames(sc_i)[which(grepl(scenarii[i], colnames(sc_i)))] <- gsub(pattern = paste0("_", scenarii[i]), replacement = "", colnames(sc_i)[which(grepl(scenarii[i], colnames(sc_i)))])
      
      colnames(sc_i)[colnames(sc_i) == "validation_predict"] <- "2022"
      # colnames(sc_i)[colnames(sc_i) == "validation_predict"] <- "2024"
      
      sc_i <- sc_i |>
        dplyr::mutate(dplyr::across(`2022`:`2100`, ~ dplyr::case_when(. > max(decile_save$biomass_class) ~ 29,
                                                                      . < min(decile_save$biomass_class) ~ 0,
                                                                      TRUE ~ .)))
      # sc_i <- sc_i |>
      #   dplyr::mutate(dplyr::across(`2024`:`2100`, ~ dplyr::case_when(. > max(decile_save$biomass_class) ~ 29,
      #                                                                 . < min(decile_save$biomass_class) ~ 0,
      #                                                                 TRUE ~ .)))
      
      sc_i[,colnames(sc_i) %in% c("2023":"2100")] <- floor(sc_i[,colnames(sc_i) %in% c("2023":"2100")])
      # sc_i[,colnames(sc_i) %in% c("2025":"2100")] <- floor(sc_i[,colnames(sc_i) %in% c("2025":"2100")])
      
      if(unit == "g"){
        
        sc_i <- sc_i |> 
          dplyr::mutate(dplyr::across(where(is.numeric), ~ decile_save$biomass[match(., decile_save$biomass_class)]))
        
      }else{
        
        sc_i <- sc_i |> 
          dplyr::mutate(dplyr::across(where(is.numeric), ~ decile_save$biomass[match(., decile_save$biomass_class)] * 10^-3))
        
      }
      
    }else{
      
      sc_i <- sc_i[,!colnames(sc_i) %in% c("scenario", "model")]
      
    }
    
    sc_i <- sc_i |> 
      tidyr::pivot_longer(cols = -c("survey_id", "species_name"),
                          names_to = "year",
                          values_to = "value") |> 
      dplyr::filter(year %in% time)
    
    species_survey <- sc_i |> 
      dplyr::select(species_name, survey_id) |> 
      unique()
    
    delta <- (sc_i[sc_i$year == time[length(time)],]$value * 100) / sc_i[sc_i$year == time[1],]$value - 100
    
    species_survey <- species_survey |> 
      dplyr::mutate(delta = delta,
                    scenario = scenarii[i],
                    model = unique(data[[1]][[1]]$model))
    species_survey[which(is.na(species_survey$delta)),]$delta <- 0

    return(species_survey)
    
  }, mc.cores = 1)

    future_pred_bind <- purrr::reduce(future_pred, dplyr::full_join) |> 
      dplyr::mutate(scenario = dplyr::recode(scenario,
                                             "ssp126" = "SSP1-2.6",
                                             "ssp245" = "SSP2-4.5",
                                             "ssp370" = "SSP3-7.0",
                                             "ssp585" = "SSP5-8.5"))

  
}



plot_future_pred_scale_function <- function(data,
                                            scale = "",
                                            ncol,
                                            all_species = FALSE,
                                            group_name = NULL,
                                            group_by = NULL,
                                            title,
                                            x_name,
                                            y_name,
                                            model_facet = TRUE,
                                            color_lab,
                                            breaks,
                                            y_scale = NULL,
                                            color_scale,
                                            model_scenario = FALSE,
                                            face = "plain",
                                            axis.text.x = 10,
                                            axis.text.y = 10,
                                            legend.position,
                                            delta = TRUE){
  
  require(ggplot2)
  
  # scale = ""
  # all_species = FALSE
  # group_name = NULL
  # group_by = NULL
  # score = FALSE
  # face = "plain"
  # model_scenario = FALSE
  # model_facet = TRUE
  
  if(delta == TRUE){
    
    if(scale == "" & model_scenario == FALSE){
      
      data$scenario <- paste0(data$scenario, " (", round(data$delta, 2), " %)")
      
    }
    
  }

  future_pred_figure_base <- data |> 
    ggplot(aes(x = year, y = biomass, group = scenario, color = scenario)) + 
    labs(y = y_name, x = x_name, title = title, color = color_lab) +
    theme_bw() + 
    theme(
      title = element_text(size = 14, face = face),
      axis.title = element_text(size = 15, face = "plain"),
      legend.text = element_text(size = 10), 
      legend.title = element_text(size = 10, face = "plain"),
      axis.text.x = element_text(size = axis.text.x),
      axis.text.y = element_text(size = axis.text.y)
    ) +
    scale_x_continuous(breaks = breaks) +
    scale_y_continuous(limits = y_scale)
  
  if(model_facet == TRUE){
    
    future_pred_figure_base <- future_pred_figure_base +
      facet_wrap(~model) +
      theme(strip.text = element_text(size = 12))
    
  }
  
  if(all_species == TRUE){
    
    if(is.null(group_by)){
      
      future_pred_figure <- data |> 
        ggplot(aes(x = year, y = biomass)) +
        geom_line(alpha = 0.4, size = 0.6, aes(group = species_name)) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(y = ifelse(unlog == TRUE, ifelse(unit == "kg", "Biomass (kg)", "Biomass (g)"), ifelse(unit == "kg", "Biomass (kg)", "Biomass (g)"))) +
        theme_bw() + 
        theme(panel.grid = element_blank(), 
              strip.background = element_rect(fill = 'grey90', colour = 'grey90'), 
              aspect.ratio = 1) +
        theme(axis.text = element_text(size = 10),
              axis.title = element_text(size = 15),
              legend.text = element_text(size = 10), 
              legend.title = element_text(size = 10),
              strip.text.x = element_text(size = 6.5),
              strip.text.y = element_text(size = 10),
              strip.background = element_blank(),
              panel.background = element_rect(fill = "white", colour = "grey50",
                                              size = 1, linetype = "solid"),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) +
        scale_x_continuous(breaks = c(2020, 2050, 2060, 2100)) +
        facet_wrap(~ scenario, scales = "free")
      
      if(scale != ""){
        
        future_pred_figure <- future_pred_figure +
          facet_wrap(scenario ~ scale, scales = "free", ncol = ncol)
        
      }
      
    }else{
      
      if(!is.null(group_name)) {
        
        future_pred_bind <- data |> 
          dplyr::filter(group_by == group_name)
        
        future_pred_figure <- future_pred_bind |> 
          ggplot(aes(x = year, y = biomass)) +
          geom_line(alpha = 0.4, size = 0.6, aes(group = species_name)) +
          labs(y = ifelse(unlog == TRUE, ifelse(unit == "kg", "Biomass (kg)", "Biomass (g)"), ifelse(unit == "kg", "Biomass (kg)", "Biomass (g)")), title = group_name) +
          theme_bw() + 
          theme(panel.grid = element_blank(), 
                strip.background = element_rect(fill = 'grey90', colour = 'grey90'), 
                aspect.ratio = 1) +
          theme(axis.text = element_text(size = 10),
                axis.title = element_text(size = 15),
                legend.text = element_text(size = 10), 
                legend.title = element_text(size = 10),
                strip.text.x = element_text(size = 10),
                strip.text.y = element_text(size = 10),
                strip.background = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50",
                                                size = 1, linetype = "solid"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) +
          scale_x_continuous(breaks = c(2020, 2050, 2060, 2100)) +
          facet_wrap(~ scenario, scales = "free")
        
      }else{
        
        future_pred_figure <- data |> 
          ggplot(aes(x = year, y = biomass)) +
          geom_line(size = 0.6, aes(color = group_by)) +
          labs(y = ifelse(unlog == TRUE, ifelse(unit == "kg", "Biomass (kg)", "Biomass (g)"), ifelse(unit == "kg", "Biomass (kg)", "Biomass (g)"))) +
          theme_bw() + 
          theme(panel.grid = element_blank(), 
                strip.background = element_rect(fill = 'grey90', colour = 'grey90'), 
                aspect.ratio = 1) +
          theme(axis.text = element_text(size = 10),
                axis.title = element_text(size = 15),
                legend.text = element_text(size = 10), 
                legend.title = element_text(size = 10),
                strip.text.x = element_text(size = 6.5),
                strip.text.y = element_text(size = 10),
                strip.background = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50",
                                                size = 1, linetype = "solid"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) +
          scale_x_continuous(breaks = c(2020, 2050, 2060, 2100)) +
          facet_wrap(~ scenario, scales = "free")
      }
      
    }
    
  }else{
    
    if(is.null(group_by)){
      
      if(model_scenario == TRUE){

        # future_pred_figure <- data |> 
        #   ggplot(aes(x = year, y = biomass)) +
        #   geom_line(size = 0.7, aes(color = scenario, linetype = model_scenario)) +
        #   labs(y = y_name, x = x_name, title = title, color = color_lab, linetype = "") +
        #   scale_discrete_manual(aesthetics = "color", values = color_scale) +
        #   scale_linetype_manual(values = c("full" = "solid", 
        #                                    "env" = "dashed",
        #                                    "hum" = "dotted")) +
        #   theme_bw() + 
        #   theme(
        #     title = element_text(size = 12, face = face),
        #     axis.text = element_text(size = 10),
        #     axis.title = element_text(size = 15, face = "plain"),
        #     legend.text = element_text(size = 10), 
        #     legend.title = element_text(size = 10, face = "plain"),
        #     strip.text.x = element_text(size = 10),
        #     strip.text.y = element_text(size = 10)
        #   ) +
        #   scale_x_continuous(breaks = breaks)
        
        future_pred_figure <- data |> 
          ggplot(aes(x = year, y = biomass)) +
          geom_line(size = 0.7, aes(color = model_scenario)) +
          facet_wrap(~scenario) +
          theme_bw() + 
          theme(
            title = element_text(size = 12, face = face),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 15, face = "plain"),
            legend.text = element_text(size = 10), 
            legend.title = element_text(size = 10, face = "plain"),
            strip.text.x = element_text(size = 10),
            strip.text.y = element_text(size = 10)
          ) +
          scale_x_continuous(breaks = breaks) +
          scale_discrete_manual(aesthetics = "color", values = color_scale) +
          labs(y = y_name, x = x_name, title = title, color = color_lab)

      }else{

        future_pred_figure <- future_pred_figure_base +
          geom_line(size = 1) +
          geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, size = 0.1) +
          scale_discrete_manual(aesthetics = "color", values = color_scale)

        if(scale != ""){

          future_pred_figure <- future_pred_figure +
            # facet_wrap(~ scale, scales = "free", ncol = ncol) +
            theme(legend.position = legend.position)
          
        }
        
        if(!is.null(group_name)) {
          
          future_pred_figure <- future_pred_figure +
            labs(title = group_name, x = "", color = "")
          
        }
        
      }
      
    }else{
      
      if(!is.null(group_name)) {
        
        future_pred_bind <- data |> 
          dplyr::filter(group_by == group_name)
        
        if(model_scenario == TRUE){
          
          future_pred_figure <- future_pred_bind |> 
            ggplot(aes(x = year, y = biomass)) +
            geom_line(size = 0.7, aes(color = scenario, linetype = model)) +
            labs(y = ifelse(unlog == TRUE, ifelse(unit == "kg", "Biomass (kg)", "Biomass (g)"), ifelse(unit == "kg", "Biomass (kg)", "Biomass (g)")),
                 title = group_name) +
            scale_discrete_manual(aesthetics = "color", values = color_scale) +
            scale_linetype_manual(values = c("full" = "solid", 
                                             "env" = "dashed",
                                             "hum" = "dotted")) +
            theme_bw() + 
            # theme(panel.grid = element_blank(), 
            #       strip.background = element_rect(fill = 'grey90', colour = 'grey90'), 
            #       aspect.ratio = 1) +
            theme(axis.text = element_text(size = 10),
                  axis.title = element_text(size = 15),
                  legend.text = element_text(size = 10), 
                  legend.title = element_text(size = 10),
                  strip.text.x = element_text(size = 6.5),
                  strip.text.y = element_text(size = 10),
                  # strip.background = element_blank(),
                  # panel.background = element_rect(fill = "white", colour = "grey50",
                  #                                 size = 1, linetype = "solid"),
                  # panel.grid.major = element_blank(), 
                  # panel.grid.minor = element_blank()
                  ) +
            scale_x_continuous(breaks = c(2020, 2050, 2060, 2100))
          
        }else{
          
          future_pred_bind$scenario <- paste0(future_pred_bind$scenario, " ", round(future_pred_bind$delta, 2), " %")
          
          future_pred_figure <- future_pred_bind |> 
            ggplot(aes(x = year, y = biomass)) +
            geom_line(size = 0.6, aes(color = scenario)) +
            # labs(y = ifelse(unlog == TRUE, ifelse(unit == "kg", "Biomass (kg)", "Biomass (g)"), ifelse(unit == "kg", "Biomass (kg)", "Biomass (g)")), title = group_name) +
            labs(y = y_name, x = x_name, title = title) +
            scale_discrete_manual(aesthetics = "color", values = color_scale) +
            theme_bw() + 
            # theme(panel.grid = element_blank(), 
            #       strip.background = element_rect(fill = 'grey90', colour = 'grey90'), 
            #       aspect.ratio = 1) +
            theme(axis.text = element_text(size = 10),
                  axis.title = element_text(size = 15),
                  legend.text = element_text(size = 10), 
                  legend.title = element_text(size = 10),
                  strip.text.x = element_text(size = 6.5),
                  strip.text.y = element_text(size = 10),
                  # strip.background = element_blank(),
                  # panel.background = element_rect(fill = "white", colour = "grey50",
                  #                                 size = 1, linetype = "solid"),
                  # panel.grid.major = element_blank(), 
                  # panel.grid.minor = element_blank()
                  ) +
            scale_x_continuous(breaks = c(2020, 2050, 2060, 2100))
          
        }

      }else{
        
        future_pred_figure <- data |> 
          ggplot(aes(x = year, y = biomass, group = group_by, color = group_by)) + 
          labs(y = y_name, x = x_name, title = title, color = color_lab) +
          theme_bw() + 
          theme(
            title = element_text(size = 14, face = face),
            axis.title = element_text(size = 15, face = "plain"),
            legend.text = element_text(size = 10), 
            legend.title = element_text(size = 10, face = "plain"),
            axis.text.x = element_text(size = axis.text.x),
            axis.text.y = element_text(size = axis.text.y)
          ) +
          scale_x_continuous(breaks = breaks) +
          scale_y_continuous(limits = y_scale) +
          facet_wrap(~scenario) +
          geom_line(linewidth = 1)

      }
      
    }
    
  }
  
  return(future_pred_figure)
  
}







# data = rf
# unlog = TRUE
# world = "world"
# year = "2023":"2100"
# year_selec = "2023"
# x_limits = c(-180, 180)
# y_limits = c(-45, 45)
# scenario = "ssp585"

future_pred_scale_extrapolation_function <- function(data,
                                                     unlog,
                                                     world,
                                                     year,
                                                     year_selec,
                                                     x_limits,
                                                     y_limits,
                                                     scenario){
  
  require(ggplot2)

  future_pred <- pbmcapply::pbmclapply(1:length(scenarii), function(i) {
    
    sc_i <- lapply(data, '[[', i)
    
    sc_i <- do.call(rbind, sc_i)
    
    colnames(sc_i)[which(grepl(scenarii[i], colnames(sc_i)))] <- gsub(pattern = paste0("_", scenarii[i]), replacement = "", colnames(sc_i)[which(grepl(scenarii[i], colnames(sc_i)))])
    
    colnames(sc_i)[colnames(sc_i) == "baseline"] <- "2022"
    
    # decile_save[31,] <- c(NA, NA) 

    for(j in 1:nrow(decile_save)) {
      
      if(!is.na(decile_save$biomass_class[j])){
        
        sc_i[sc_i == decile_save$biomass_class[j]] <- decile_save$biomass[j] * 0.001
        
      }
      
    }

    sc_i_grouped <- sc_i |>
      dplyr::group_by(x, y) |>
      dplyr::summarise(dplyr::across(c("2022", as.character(year)), sum))
    # sc_i_grouped <- dplyr::inner_join(info, sc_i_grouped)
    sc_i_grouped$scenario <- scenarii[i]
    sc_i_grouped <- sc_i_grouped |> 
      dplyr::ungroup()

    return(sc_i_grouped)
    
  }, mc.cores = parallel::detectCores() - 1)
  
  all_data <- purrr::reduce(future_pred, dplyr::full_join)
  
  # all_data_sc <- all_data[which(all_data$scenario == scenario_i),]
  
  test <- all_data |>
    dplyr::select(c(x, y, year_selec, "2023", scenario))
  
  if(length(year_selec) > 1) {
    
    test2 <- lapply(1:length(year_selec), function(j) {
      
      test <- all_data |>
        dplyr::select(c(x, y, year_selec[j], "2023", scenario))
      
      colnames(test)[colnames(test) == year_selec[j]] <- "year_selec"
      
      test <- test |> 
        dplyr::mutate(diff = year_selec[j] - `2023`)
      
      colnames(test)[colnames(test) %in% "diff"] <- paste0("diff_", year_selec[j])
      
      test
      
    })
    test2 <- purrr::reduce(test2, dplyr::full_join)
    
  }
  
  colnames(test)[colnames(test) == year_selec] <- "year_selec"

  test <- test |> 
    dplyr::mutate(diff = year_selec - `2023`)

  world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  if(any(world != "world")){
    
    world_map <- world_map |>
      dplyr::filter(name %in% world)
    
  }else{
    
    world_map <- world_map
    
  }

  if(any(world != "world")){
    
    test <- test |> 
      dplyr::filter(scenario == scenario)
    
    # max_lim <- quantile(test$diff, 0.9)
    # min_lim <- quantile(test$diff, 0.1)
    
    max_lim <- 2.2
    min_lim <- -18.2
    
    map_biomass <- ggplot(data = world_map) +
      geom_sf() +
      geom_raster(data = test, aes(x = x, y = y, fill = values)) +
      scale_x_continuous(limits = x_limits) +
      scale_y_continuous(limits = y_limits) +
      theme_bw() +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(10, "RdBu"),
                           limits = c(min_lim, max_lim),                  # Limites symétriques
                           values = scales::rescale(c(min_lim, 0, max_lim)), # Centre sur 0
                           oob = scales::squish) +
      coord_sf(expand = FALSE) +
      theme(axis.text = element_text(size = 5),
            axis.title = element_text(size = 15),
            panel.background = element_rect(fill = "white", colour = "grey50",
                                            size = 1, linetype = "solid"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +  
      facet_grid(scenario ~ year)
    
  }else{

    test <- test |> 
      dplyr::filter(scenario == scenario)

    # max_lim <- quantile(test$diff, 0.9)
    # min_lim <- quantile(test$diff, 0.1)
    
    max_lim <- 2.2
    min_lim <- -18.2

    map_biomass <- ggplot(data = world_map) +
      geom_sf() +
      geom_raster(data = test, aes(x = x, y = y, fill = diff)) +
      scale_x_continuous(limits = x_limits) +
      scale_y_continuous(limits = y_limits) +
      theme_bw() +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(10, "RdBu"),
                           limits = c(min_lim, max_lim),                  # Limites symétriques
                           values = scales::rescale(c(min_lim, 0, max_lim)), # Centre sur 0
                           oob = scales::squish) +
      coord_sf(expand = FALSE) +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 15),
            panel.background = element_rect(fill = "white", colour = "grey50",
                                            size = 1, linetype = "solid"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      labs(title = paste0(scenario, " ", year_selec))
    
  }

  return(map_biomass)
  
}
