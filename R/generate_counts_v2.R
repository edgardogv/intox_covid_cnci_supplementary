generate_counts2 <- function(data, date_column, frequency = "monthly",
                            grouping_variables = NULL, 
                            standardize_counts = FALSE) {
  
  agg_data <- data %>% 
    mutate(
      "{date_column}" := as.Date(!!rlang::sym({date_column}))
    ) %>% 
    group_by(across(all_of(c(date_column, grouping_variables)))) %>% 
    summarise(TOTAL = n()) %>% ungroup()
  
  if (standardize_counts) {
    agg_data <- agg_data %>% 
      group_by(across(all_of(grouping_variables))) %>% 
      mutate(
        TOTAL = scale(TOTAL)
      )
  }
  
  min_date <- min(agg_data[[date_column]])
  max_date <- max(agg_data[[date_column]])
  dates_seq <- seq(min_date, max_date, by = "month")
  # "day", "week", "month", "quarter" or "year"
  
  if(is.null(grouping_variables)) {
      agg_data <- tibble(DATE = dates_seq) %>% 
        left_join(agg_data, by = c("DATE" = date_column))
      colnames(agg_data)[colnames(agg_data) == "DATE"] <- date_column
  } else {
      grouping_categories <- agg_data %>% 
        select(all_of(grouping_variables)) %>% 
        distinct()
      
      base_categories <- expand_grid(
        tibble("{date_column}" := dates_seq),
        grouping_categories
      )
      
      agg_data <- agg_data %>% 
        full_join(base_categories, by = c(date_column, grouping_variables)) %>% 
        replace_na(list(TOTAL = 0))
  }
  
  return(agg_data)
  
}