get_cumulative_frequencies <- function (raw_data, grouping_column) {
  freq_table <- raw_data %>% 
    count(across(all_of(grouping_column))) %>% 
    mutate(TOX_FREQ = (n / sum(n)) * 100) %>%
    rename(TOX_N = n) %>% 
    arrange(desc(TOX_FREQ)) %>% 
    mutate(TOX_FREQ_CUMSUM = cumsum(TOX_FREQ))
  
  return(freq_table)
}