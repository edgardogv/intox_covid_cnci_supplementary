tibl_to_ts <- function(data, date_column, frequency = "monthly",
                       grouping_variables = NULL) {
  
  agg_data <- data %>% 
    mutate(
      "{date_column}" := as.Date(!!rlang::sym({date_column}))
    ) %>% 
    group_by(across(all_of(c(date_column, grouping_variables)))) %>% 
    summarise(TOTAL = n())
  
  min_date <- min(agg_data[[date_column]])
  max_date <- max(agg_data[[date_column]])
  dates_seq <- seq(min_date, max_date, by = "month")
  # "day", "week", "month", "quarter" or "year"
  ts_list <- list()
  
  if(tolower(frequency) == "monthly") {
    ts_freq <- 12
    ts_start_year <- lubridate::year(min_date)
    ts_start_month <- lubridate::month(min_date, abbr = FALSE, label = FALSE)
    ts_end_year <- lubridate::year(max_date)
    ts_end_month <- lubridate::month(max_date, abbr = FALSE, label = FALSE)
    
    if(is.null(grouping_variables)) {
      agg_data <- tibble(DATE = dates_seq) %>% 
        left_join(agg_data, by = c("DATE" = date_column))
      colnames(agg_data)[colnames(agg_data) == "DATE"] <- date_column
      ts_list[["TOTAL"]] <- ts(agg_data[["TOTAL"]], frequency = ts_freq, 
                               start = c(ts_start_year, ts_start_month), 
                               end = c(ts_end_year, ts_end_month)) %>% xts::as.xts()
    } else {
      agg_data <- agg_data %>% 
        unite(TEMP_KEY, all_of(grouping_variables), remove = FALSE)
      
      temp_list <- lapply(unique(agg_data[["TEMP_KEY"]]), function(key) {
        temp_data <- agg_data %>% 
          filter(TEMP_KEY == key)
        
        temp_data <- tibble(DATE = dates_seq) %>% 
          left_join(temp_data, by = c("DATE" = date_column) ) %>% 
          fill(all_of(c("TEMP_KEY", grouping_variables)), .direction = "down") %>% 
          replace_na(list(TOTAL = 0))
        colnames(temp_data)[colnames(temp_data) == "DATE"] <- date_column
        
        ts_temp <- list()
        ts_temp[[key]] <- ts(temp_data[["TOTAL"]], frequency = ts_freq, 
                             start = c(ts_start_year, ts_start_month), 
                             end = c(ts_end_year, ts_end_month)) %>% xts::as.xts()
        
        list(
          temp_data = temp_data,
          ts = do.call(cbind, ts_temp)
        )
      })
      
      agg_data <- lapply(temp_list, function(list_element) {
        list_element$temp_data
      }) %>% bind_rows()
      
      ts_list <- lapply(temp_list, function(list_element) {
        list_element$ts
      })
    }
  }
  
  return(
    list(
      data = agg_data,
      ts = do.call(cbind, ts_list)
    )
  )
  
}
