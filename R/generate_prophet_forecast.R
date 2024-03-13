generate_prophet_forecast <- function(train_ts, test_ts, periods,
                                      changepoint_prior_scale = 0.05,
                                      changepoint_range = 0.8,
                                      seasonality_prior_scale = 10) {
  library(prophet)
  
  if("ts" %in% class(train_ts)) {
    train_ts_df <- train_ts %>% 
      timetk::tk_tbl() %>% 
      rename(
        ds = index,
        y = value
      )
    
    test_df <- test_ts %>% 
      timetk::tk_tbl() %>% 
      rename(
        ds = index,
        y = value
      )  %>% 
      mutate(ds = lubridate::as_date(ds))
  } else {
    train_ts_df <- train_ts %>% 
      rename(
        ds = TIEMPO,
        y = TOTAL
      )
    
    test_df <- test_ts %>% 
      rename(
        ds = TIEMPO,
        y = TOTAL
      )  %>% 
      mutate(ds = lubridate::as_date(ds))
  }
  
  change_point_prior_scale = c(0.001, 0.01, 0.1, 0.5)
  seasonality_prior_scale = c(0.01, 0.1, 1.0, 10.0)
  changepoint_range = c(0.2, 0.4, 0.6, 0.8)
  tunning_df <- expand.grid(change_point_prior_scale, seasonality_prior_scale, changepoint_range)
  names(tunning_df) <- c("change_point_prior_scale", "seasonality_prior_scale", "changepoint_range")

  results_list <- lapply(1:nrow(tunning_df), function(row) {
    temp_df <- tunning_df[row,]
    print(paste(row, "of", nrow(tunning_df)))
    m <- prophet(train_ts_df, 
                 changepoint.prior.scale = temp_df$change_point_prior_scale, #0.001 a 0.5, def 0.05
                 changepoint.range = temp_df$changepoint_range, # between 0 and 1, def 0.8
                 seasonality.prior.scale = temp_df$seasonality_prior_scale, #0.01 a 10. def 10.
                 interval.width = 0.95, 
                 daily.seasonality = FALSE,
                 weekly.seasonality = FALSE,
                 yearly.seasonality = TRUE)
    future <- make_future_dataframe(m, periods = periods, freq = "month")
    forecast <- predict(m, future)
    nrow_forecast <- nrow(forecast)
    
    acc_df <- forecast::accuracy(forecast$yhat[(nrow_forecast - length(test_df$y)):nrow_forecast], 
                                 test_df$y) %>% 
      as.data.frame() %>% 
      mutate(
        Modelo = "Prophet",
        Metric_Type = "Test set",
        changepoint_prior_scale = temp_df$change_point_prior_scale,
        seasonality_prior_scale = temp_df$seasonality_prior_scale,
        changepoint_range = temp_df$changepoint_range
      )
    
    forecast_tb <- forecast %>% as_tibble() %>%
      select(ds, yhat, yhat_lower, yhat_upper) %>% 
      mutate(ds = as.Date(ds)) %>% 
      filter(ds >= as.Date(min(test_ts$TIEMPO)))
    
    return(list(
      forecast_tb = forecast_tb,
      acc_df = acc_df
    ))
    
  })
  
  # df.cv <- cross_validation(m, 
  #                           # initial = 730/12, period = 365/12, 
  #                           horizon = 34, units = 'days')
  # df.p <- performance_metrics(df.cv)
  # plot_cross_validation_metric(df.cv, metric = 'mape')

  # plot(m, forecast) + add_changepoints_to_plot(m)
  # prophet_plot_components(m, forecast)
  
  acc_list <- lapply(results_list, function(model_result) {
    model_result$acc_df
  })
  
  forecast_tb_list <- lapply(results_list, function(model_result) {
    model_result$forecast_tb
  })
  
  acc_consolidated <- bind_rows(acc_list)
  
  return(list(
    forecast_tb_list = forecast_tb_list,
    acc_consolidated = acc_consolidated
  ))
}

#================================================================================

generate_prophet_plot <- function(train_ts, test_ts, forecast_tb) {
  
  if("ts" %in% class(train_ts)) {
    train_ts_df <- train_ts %>% 
      timetk::tk_tbl() %>% 
      rename(
        ds = index,
        y = value
      ) %>% 
      mutate(ds = lubridate::as_date(ds))
    
    test_df <- test_ts %>% 
      timetk::tk_tbl() %>% 
      rename(
        ds = index,
        y = value
      )  %>% 
      mutate(ds = lubridate::as_date(ds))
  } else {
    train_ts_df <- train_ts %>% 
      rename(
        ds = TIEMPO,
        y = TOTAL
      ) %>% 
      mutate(ds = lubridate::as_date(ds))
    
    test_df <- test_ts %>% 
      rename(
        ds = TIEMPO,
        y = TOTAL
      )  %>% 
      mutate(ds = lubridate::as_date(ds))
  }
  
  prophet_plot <- ggplot() +
    geom_line(train_ts_df, mapping = aes(x = ds, y = y, color = "Pre-Pandemia"),
              linewidth = 1) +
    geom_line(forecast_tb, mapping = aes(x = ds, y = yhat, color = "Pronosticado"),
              linewidth = 1) +
    geom_line(forecast_tb, mapping = aes(x = ds, y = yhat_lower, color = "Intervalo 95%"),
              linewidth = 1, lty = 2) +
    geom_line(forecast_tb, mapping = aes(x = ds, y = yhat_upper, color = "Intervalo 95%"),
              linewidth = 1, lty = 2) +
    geom_line(test_df, mapping = aes(x = ds, y = y, color = "Pandemia"),
              linewidth = 1) +
    theme_classic() +
    ylab("Casos de intoxicación reportados") +
    xlab(NULL) +
    scale_color_manual(name=NULL,
                       breaks=c('Pre-Pandemia', 'Pandemia', "Pronosticado", 'Intervalo 95%'),
                       values=c('Pre-Pandemia'='black', 'Pandemia'='darkred', 'Pronosticado'='darkgreen', "Intervalo 95%" = "darkblue")) +
    labs(caption = "Intervalo de incertidumbre estimado al 95%") +
    theme(legend.position = "top")
  
  return(prophet_plot)
}

#================================================================================

combine_model_metrics <- function(arima_metrics, prophet_metrics) {
  metrics_df <- bind_rows(arima_metrics, prophet_metrics) %>% 
    filter(Metric_Type == "Test set") %>% 
    select(-Metric_Type) %>% 
    relocate(Modelo) %>% 
    select(Modelo, ME, RMSE, MAE, MPE, MAPE)
  row.names(metrics_df) <- NULL
  return(metrics_df)
}

#================================================================================

combine_model_metrics2 <- function(arima_metrics, prophet_metrics) {
  
  metrics_df <- bind_rows(arima_metrics, prophet_metrics) %>% 
    select(-Metric_Type) %>% 
    relocate(Modelo) %>% 
    select(Modelo, ME, RMSE, MAE, MPE, MAPE)
  row.names(metrics_df) <- NULL
  return(metrics_df)
}

