geenerate_arima_model <- function(ts_data, 
                                  ts_test = NULL,
                            orders_list,
                            xreg = NULL,
                            period = 12) {
  
  results_list <- lapply(orders_list, function(order_list) {
    
    if (!any(is.na(order_list$seasonal))) {
      if(is.null(xreg)) {
        arima_mod <- forecast::Arima(ts_data, 
                                     order = order_list$order,
                                     seasonal = list(order = order_list$seasonal, period = period),
                                     method = "ML")
      } else {
        arima_mod <- forecast::Arima(ts_data, 
                                     order = order_list$order,
                                     xreg = xreg,
                                     seasonal = list(order = order_list$seasonal, period = period),
                                     method = "ML")
      }
      
      mod_name <- paste0("ARIMA", " (", paste(order_list$order, collapse = ","), ")",
                         "(", paste(order_list$seasonal, collapse = ","), ")", 
                         "[", period, "]")
    } else {
      if(is.null(xreg)) {
        arima_mod <- forecast::Arima(ts_data, 
                                     order = order_list$order,
                                     method = "ML")
      } else {
        arima_mod <- forecast::Arima(ts_data, 
                                     order = order_list$order,
                                     xreg = xreg,
                                     method = "ML")
      }
      mod_name <- paste0("ARIMA", " (", paste(order_list$order, collapse = ","), ")")
    }
    
    sum_mod <- summary(arima_mod)
    if(is.null(xreg)) {
      pron_ts <- forecast::forecast(arima_mod, h = 34)
    } else {
      pron_ts <- forecast::forecast(arima_mod, h = 34, xreg = xreg)
    }
    
    if(is.null(ts_test)) {
      acc_mod <- forecast::accuracy(pron_ts) %>% 
        as.data.frame() 
    } else {
      acc_mod <- forecast::accuracy(pron_ts, ts_test) %>% 
        as.data.frame() 
    }
    
    # pron_ts_df <- pron_ts$mean %>% 
    #   timetk::tk_tbl() %>% 
    #   mutate(index = lubridate::as_date(index))
    # 
    # acc_mod2 <- forecast::accuracy(pron_ts_df$value, ts_test) %>% 
    #   as.data.frame() 
    
    acc_mod$AIC <- sum_mod$aic
    acc_mod$BIC <- sum_mod$bic
    acc_mod$Modelo <- mod_name
    acc_mod$Metric_Type <- row.names(acc_mod)
    row.names(acc_mod) <- NULL
    
    return(list(
      mod = arima_mod,
      sum_mod = sum_mod,
      pron_ts = pron_ts,
      mod_metrics = acc_mod
    ))
  })
  
  mod_metrics_df <- lapply(results_list, function(result) {
    result$mod_metrics
  }) %>% bind_rows() %>% 
    relocate(Modelo, Metric_Type) %>% 
    mutate_if(is.numeric, ~round(., 2))

  return(list(
    mod_metrics_df = mod_metrics_df,
    results_list = results_list
  ))
  
}

#===============================================================================

gen_pronostic_plot <- function(train_ts, test_ts, models, model_num) {
  
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
  
  pron_df <- models$results_list[[model_num]]$pron_ts$mean %>% 
    timetk::tk_tbl() %>% 
    rename(
      ds = index,
      yhat = value
    ) %>% 
    mutate(ds = lubridate::as_date(ds),
           y = test_df$y, 
           yhat_lower = models$results_list[[model_num]]$pron_ts$lower[,2],
           yhat_upper = models$results_list[[model_num]]$pron_ts$upper[,2])
  
  arima_plot <- ggplot() +
    geom_line(train_ts_df, mapping = aes(x = ds, y = y, color = "Pre-Pandemia"),
              linewidth = 1) +
    geom_line(pron_df, mapping = aes(x = ds, y = yhat, color = "Pronosticado"),
              linewidth = 1.5) +
    geom_line(pron_df, mapping = aes(x = ds, y = yhat_lower, color = "Intervalo de Confianza"),
              linewidth = 1.5, lty = 2) +
    geom_line(pron_df, mapping = aes(x = ds, y = yhat_upper, color = "Intervalo de Confianza"),
              linewidth = 1.5, lty = 2) +
    geom_line(test_df, mapping = aes(x = ds, y = y, color = "Pandemia"),
              linewidth = 1.5) +
    theme_classic() +
    ylab("Casos de intoxicación reportados") +
    xlab(NULL) +
    scale_color_manual(name=NULL,
                       breaks=c('Pre-Pandemia', 'Pandemia', "Pronosticado", 'Intervalo de Confianza'),
                       values=c('Pre-Pandemia'='black', 'Pandemia'='darkred', 'Pronosticado'='darkgreen', "Intervalo de Confianza" = "darkblue")) +
    labs(caption = "Intervalo de confianza estimado al 95%") +
    theme(legend.position = "top")
  
  return(
    list(
      arima_plot = arima_plot,
      pron_df = pron_df
    ))
  
}

#===============================================================================

gen_pronostic_plot2 <- function(train_ts, test_ts, model, h_forecast = 12) {
  
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
  
  model_forecast <- model |>
    forecast(h=h_forecast, ) |>
    hilo(level = c(95))  |>
    unpack_hilo("95%")

  pron_df <- model_forecast %>% 
    rename(
      ds = TIEMPO,
      yhat = .mean,
      yhat_lower = `95%_lower`,
      yhat_upper = `95%_upper`
    ) %>% 
    mutate(ds = lubridate::as_date(ds)) %>% 
    left_join(test_df, by = "ds")
  
  arima_plot <- ggplot() +
    geom_line(train_ts_df, mapping = aes(x = ds, y = y, color = "Pre-Pandemia"),
              linewidth = 1) +
    geom_line(pron_df, mapping = aes(x = ds, y = yhat, color = "Pronosticado"),
              linewidth = 1) +
    geom_line(pron_df, mapping = aes(x = ds, y = yhat_lower, color = "Intervalo 95%"),
              linewidth = 1, lty = 2) +
    geom_line(pron_df, mapping = aes(x = ds, y = yhat_upper, color = "Intervalo 95%"),
              linewidth = 1, lty = 2) +
    geom_line(test_df, mapping = aes(x = ds, y = y, color = "Pandemia"),
              linewidth = 1) +
    theme_classic() +
    ylab("Casos de intoxicación reportados") +
    xlab(NULL) +
    scale_color_manual(name=NULL,
                       breaks=c('Pre-Pandemia', 'Pandemia', "Pronosticado", 'Intervalo 95%'),
                       values=c('Pre-Pandemia'='black', 'Pandemia'='darkred', 'Pronosticado'='darkgreen', "Intervalo 95%" = "darkblue")) +
    labs(caption = "Intervalo de confianza estimado al 95%") +
    theme(legend.position = "top")
  
  return(
    list(
      arima_plot = arima_plot,
      pron_df = pron_df
    ))
  
}
