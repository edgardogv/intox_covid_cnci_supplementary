gg_tsdisplay_spanish <- function (data, y = NULL, plot_type = c("auto", "partial", "season", 
                                        "histogram", "scatter", "spectrum"), lag_max = NULL) 
{
  require("grid")
  y <- feasts:::guess_plot_var(data, !!enquo(y))
  plot_type <- rlang::arg_match(plot_type)
  if (plot_type == "auto") {
    period <- get_frequencies(NULL, data, .auto = "all")
    if (all(period <= 1)) {
      plot_type <- if (any(is.na(data[[as_name(y)]]))) 
        "partial"
      else "spectrum"
    }
    else {
      plot_type <- "season"
    }
  }
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
  
  p1 <- ggplot(data, aes(x = as_date(TIEMPO), y = !!y)) + geom_line() + 
    geom_point() +
    xlab(NULL) +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y-%m")
  p2 <- autoplot(ACF(data, !!y, lag_max = lag_max))
  if (plot_type == "partial") {
    p3 <- autoplot(PACF(data, !!y, lag_max = lag_max))
    p2_yrange <- ggplot2::layer_scales(p2)$y$range$range
    p3_yrange <- ggplot2::layer_scales(p3)$y$range$range
    yrange <- range(c(p2_yrange, p3_yrange))
    p2 <- p2 + ylim(yrange)
    p3 <- p3 + ylim(yrange)
  }
  else if (plot_type == "season") {
    p3 <- gg_season(data, !!y)
  }
  else if (plot_type == "histogram") {
    p3 <- ggplot(data, aes(x = !!y)) + geom_histogram(bins = min(500, 
                                                                 grDevices::nclass.FD(na.exclude(data[[rlang::as_name(y)]])))) + 
      ggplot2::geom_rug() +
      xlab("Residuales") +
      ylab("Conteo")
  }
  else if (plot_type == "scatter") {
    p3 <- data %>% mutate(`:=`(!!paste0(as_name(y), "_lag"), 
                               lag(!!y, 1))) %>% .[complete.cases(.), ] %>% ggplot(aes(y = !!y, 
                                                                                       x = !!sym(paste0(as_name(y), "_lag")))) + geom_point() + 
      xlab(expression(Y[t - 1])) + ylab(expression(Y[t]))
  }
  else if (plot_type == "spectrum") {
    spec <- safely(stats::spec.ar)(eval_tidy(y, data), plot = FALSE)
    p3 <- if (is.null(spec[["result"]])) {
      if (spec$error$message == "missing values in object") {
        warn("Spectrum plot could not be shown as the data contains missing values. Consider using a different `plot_type`.")
      }
      else {
        warn(sprintf("Spectrum plot could not be shown as an error occurred: %s", 
                     spec$error$message))
      }
      ggplot() + ggplot2::labs(x = "frequency", y = "spectrum")
    }
    else {
      spec[["result"]] %>% {
        tibble(spectrum = drop(.$spec), frequency = .$freq)
      } %>% ggplot(aes(x = !!sym("frequency"), y = !!sym("spectrum"))) + 
        geom_line() + ggplot2::scale_y_log10()
    }
  }
  structure(list(p1, p2, p3), class = c("gg_tsensemble", "gg"))
}
