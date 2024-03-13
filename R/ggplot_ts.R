ggplot_ts <- function(aggr_data, x_var, y_var, 
                      color_var = NULL, 
                      facet_var = NULL,
                      frequency = "monthly",
                      x_lab = x_var, 
                      y_lab = y_var, 
                      color_lab = color_var,
                      facet_cols = 3,
                      free_y = FALSE,
                      interactive = FALSE) {
  
  aggr_data <- aggr_data %>% 
    mutate(!!rlang::sym(x_var) := as.Date(!!rlang::sym(x_var)))
  
  min_date <- min(aggr_data[[x_var]])
  max_date <- max(aggr_data[[x_var]])
  
  date_range <- seq(min_date, max_date, length.out = 21)
  
  if(is.null(color_var)) {
    graph <- ggplot(aggr_data, aes(x = !!rlang::sym(x_var), 
                                   y = !!rlang::sym(y_var), 
                                   group = 1))
  } else {
    graph <- ggplot(aggr_data, aes(x = !!rlang::sym(x_var),
                                   y = !!rlang::sym(y_var), 
                                   color = !!rlang::sym(color_var))) +
      labs(color = color_lab)
  }

  graph <- graph +
    geom_line(linewidth = 1) +
    ylim(0, max(aggr_data[[y_var]])) +
    xlab(x_lab) + 
    ylab(y_lab) +
    theme_classic() +
    theme(legend.position="top", 
          axis.text.x = element_text(angle = 45, hjust=1),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          text = element_text(size = 12))
  
  if(frequency == "monthly") {
    graph <- graph +
      scale_x_date(limits = c(min_date, max_date),
                   breaks = date_range,
                   date_labels = "%b-%Y")
  }
  
  if(!is.null(facet_var)) {
    if(free_y) {
      graph <- graph +
        facet_wrap(as.formula(paste("~", facet_var)), 
                   scales = "free_y", ncol = facet_cols)
    } else {
      graph <- graph +
        facet_wrap(as.formula(paste("~", facet_var)), ncol = facet_cols)
    }
  }
  
  if(interactive) {
    graph <- plotly::ggplotly(graph)
  }
  
  return(graph)
}