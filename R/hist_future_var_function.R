
# hist_var = list_extracted_cov_hist[which(grepl("tos", list_extracted_cov_hist) == TRUE)]
# future_var = list_extracted_cov[which(grepl("to", list_extracted_cov) == TRUE)]

hist_future_var_function <- function(hist_var,
                                     future_var){
  
  read_var_hist <- lapply(1:length(hist_var), function(i) {
    
    load(hist_var[i])
    assign(paste0("var_", i), data)
    
  })
  read_var_hist_bind <- do.call(rbind, read_var_hist) |> 
    dplyr::select(-c(scenario, variable))
  
  read_var_fut <- lapply(1:length(future_var), function(i) {
    
    load(future_var[i])
    assign(paste0("var_", i), data)
    
  })
  read_var_fut_bind <- do.call(rbind, read_var_fut)
  
  hist_future <- read_var_hist_bind |> 
    dplyr::inner_join(read_var_fut_bind) |> 
    dplyr::relocate(model, .after = variable)
  
}
