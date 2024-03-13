plot_map <- function(data, 
                     st_data,
                     years,
                     sexo = NULL,
                     grupo_etario = NULL,
                     toxico = NULL,
                     pob_ref = 1000) {
  
  ready_data <- data %>%
    filter(YEAR %in% years)
  
  if(!is.null(sexo)) {
    ready_data <- ready_data %>%
      filter(SEXO == sexo) 
  }
  
  if(!is.null(grupo_etario)) {
    ready_data <- ready_data %>%
      filter(GRUPO_ETARIO2 %in% grupo_etario)
  }
  
  if(!is.null(toxico)) {
    ready_data <- ready_data %>%
      filter(TOXICO_INTERES1 == toxico)
  }
  
  cantones <- ready_data %>%
    group_by(CANTON, YEAR, .add = TRUE) %>%
    summarise(
      POBLACION = unique(Poblacion),
      TOTAL_CASOS = sum(TOTAL, na.rm = TRUE)
    ) %>% 
    group_by(CANTON, YEAR) %>% 
    summarise(
      POBLACION = sum(POBLACION, na.rm = T),
      TOTAL_CASOS = sum(TOTAL_CASOS, na.rm = TRUE),
      TASA = (TOTAL_CASOS/POBLACION) * pob_ref
    )
  
  space <- st_data %>%
    left_join(cantones, by = c("NAME" = "CANTON"))
  
  pob_ref2 <- ifelse(pob_ref <= 100, pob_ref, paste(pob_ref/1000, "mil"))
  
  geo_plot <- ggplot() +
    geom_sf(data = space, aes(fill = TASA, geometry=geometry, text = paste(NAME, round(TASA, 2))), color = "black") +
    xlim(c(-86, -82.5)) +
    ylim(c(8., 11.3)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, na.value = 'transparent', #limits = c(min(mn,-mx),max(mx,-mn))
    ) +
    labs(fill = paste("Por cada", pob_ref2 ,"habitantes")) +
    theme_void() +
    facet_wrap(~space$YEAR, ncol = 2)
  # theme( legend.key.size = unit(1, 'cm'),
  #        legend.title = element_text(size=30),
  #        legend.text = element_text(size=30),
  #        strip.text = element_text(size = 30))
  
  return(
    list(
      map_data = space,
      geo_plot = geo_plot
    )
  )
  
}