get_ts_from_agg_df <- function(agg_data, 
                               filtro_etario = NULL, 
                               filtro_sexo = NULL, 
                               filtro_toxico = NULL) {
  if(!is.null(filtro_etario)) {
    agg_data <- agg_data %>% 
      filter(
        GRUPO_ETARIO2 %in% filtro_etario
      )
  }
  
  if(!is.null(filtro_sexo)) {
    agg_data <- agg_data %>% 
      filter(
        SEXO %in% filtro_sexo
      )
  }
  
  if(!is.null(filtro_toxico)) {
    agg_data <- agg_data %>% 
      filter(
        TOXICO_INTERES1 %in% filtro_toxico
      )
  }
  
  time_agg_data <- agg_data %>% 
    group_by(TIEMPO) %>% 
    summarise(TOTAL = sum(TOTAL))
  
  
  ts_full <- ts(time_agg_data$TOTAL, frequency = 12, start = c(2015, 01), end = c(2022, 12))
  ts_train <- window(ts_full, start = c(2015, 01), end = c(2020, 02))
  ts_test <- window(ts_full, start = c(2020, 03), end = c(2022, 12))
  
  return(
    list(
      agg_data = time_agg_data,
      ts_full = ts_full,
      ts_train = ts_train,
      ts_test = ts_test,
      train_decomp = decompose(ts_train),
      full_decomp = decompose(ts_full)
    )
  )
}