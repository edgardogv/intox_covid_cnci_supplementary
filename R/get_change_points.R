get_change_points <- function(time_serie, intercept_only = FALSE) {
  
  if(intercept_only) {
    library(changepoint)
    full_mean_cp = cpt.mean(time_serie)
  } else {
    library(changepoint.np)
    full_mean_cp = cpt.np(time_serie)
  }
  
  ts_df <- time_serie %>% timetk::tk_tbl() %>% mutate(index = lubridate::as_date(index))
  ts_cp <- ts_df[full_mean_cp@cpts, ] %>% 
    rename(position = value) %>% 
    mutate(
      means = full_mean_cp@param.est$mean,
      label = case_when(
        !is.na(position) ~ paste0(year(index), "-", month(index))
      )
    )
  
  if(intercept_only) {
    ready_df <- ts_df %>% 
      left_join(ts_cp, by = "index") %>% 
      fill(means, .direction = "updown")
    
    cp_plot <- ggplot(ready_df, aes(x = index, y = value)) +
      geom_line() +
      geom_line(aes(x = index, y = means), col = "darkred") +
      geom_label(aes(x = index, y = value, label = label), col = "darkred", 
                 nudge_x = 5, nudge_y = 1) +
      theme_classic() 
  } else {
    ready_df <- ts_df %>% 
      left_join(ts_cp, by = "index") %>% 
      mutate(
        cpts = case_when(
          !is.na(position) ~ index
        )
      )
    
    cp_plot <- ggplot(ready_df, aes(x = index, y = value)) +
      geom_line() +
      geom_vline(aes(xintercept = as.numeric(cpts)), col = "darkred") +
      geom_label(aes(x = index, y = value, label = label), col = "darkred", 
                 nudge_x = 5, nudge_y = 1) +
      theme_classic() 
  }
  
  return(list(
    cp_plot = cp_plot,
    cp_df = ready_df
  ))
}