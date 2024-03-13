generate_counts <- function(data, date_column, frequency = "monthly",
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
  
  if(tolower(frequency) == "monthly") {
    
    if(is.null(grouping_variables)) {
      agg_data <- tibble(DATE = dates_seq) %>% 
        left_join(agg_data, by = c("DATE" = date_column))
      colnames(agg_data)[colnames(agg_data) == "DATE"] <- date_column
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
        
        return(temp_data)
      })
      
      agg_data <- lapply(temp_list, function(list_element) {
        list_element$temp_data
      }) %>% bind_rows()
    }
  }
  
  return(agg_data)
  
}