generate_dtw_cluster_plot <- function(agg_data, k_clusters = 2, 
                                      lab_y = "Total", lab_x = "Mes y Año") {
  
  if("SEXO" %in% colnames(agg_data) & "GRUPO_ETARIO2" %in% colnames(agg_data)) {
    agg_data2 <- agg_data %>% 
      mutate(
        SEXO_CD = case_when(
          SEXO == "Femenino" ~ "F",
          SEXO == "Masculino" ~ "M"
        ),
        DEMO = paste(SEXO_CD, "-", GRUPO_ETARIO2),
        FULL_LABEL = paste(TOXICO_INTERES1, "-", DEMO)
      ) %>% 
      group_by(TIEMPO, FULL_LABEL) %>% 
      summarise(
        TOTAL = sum(TOTAL)
      ) %>% 
      arrange(FULL_LABEL, TIEMPO)
  } else if("SEXO" %in% colnames(agg_data)) {
    agg_data2 <- agg_data %>% 
      mutate(
        FULL_LABEL = paste(TOXICO_INTERES1, "-", SEXO)
      ) %>% 
      group_by(TIEMPO, FULL_LABEL) %>% 
      summarise(
        TOTAL = sum(TOTAL)
      ) %>% 
      arrange(FULL_LABEL, TIEMPO)
  } else if("GRUPO_ETARIO2" %in% colnames(agg_data)) {
    agg_data2 <- agg_data %>% 
      mutate(
        FULL_LABEL = paste(TOXICO_INTERES1, "-", GRUPO_ETARIO2)
      ) %>% 
      group_by(TIEMPO, FULL_LABEL) %>% 
      summarise(
        TOTAL = sum(TOTAL)
      ) %>% 
      arrange(FULL_LABEL, TIEMPO)
  } else {
    agg_data2 <- agg_data %>% 
      mutate(
        FULL_LABEL = TOXICO_INTERES1
      ) %>% 
      group_by(TIEMPO, FULL_LABEL) %>% 
      summarise(
        TOTAL = sum(TOTAL)
      ) %>% 
      arrange(FULL_LABEL, TIEMPO)
  }
  
  agg_wide <- agg_data2 %>% 
    pivot_wider(names_from = TIEMPO, values_from = TOTAL)
  agg_wide_mat <- agg_wide %>% 
    select(-FULL_LABEL) %>% as.matrix()
  rownames(agg_wide_mat) <- agg_wide$FULL_LABEL
  
  library(dtwclust)
  cluster_dtw_h2 <- dtwclust::tsclust(agg_wide_mat, 
                                      type = "h", 
                                      k = k_clusters,  
                                      distance = "dtw", 
                                      control = hierarchical_control(method = "complete"),
                                      preproc = NULL, 
                                      args = tsclust_args(dist = list(window.size = 5L)))
  
  hclus <- stats::cutree(cluster_dtw_h2, k = k_clusters) %>% 
    as.data.frame(.) %>%
    dplyr::rename(.,cluster_group = .) %>%
    tibble::rownames_to_column("type_col")
  
  hcdata <- ggdendro::dendro_data(cluster_dtw_h2)
  names_order <- hcdata$labels$label
  
  p1 <- hcdata %>%
    ggdendro::ggdendrogram(., rotate=TRUE, leaf_labels=FALSE, theme_dendro = FALSE) +
    ylab("Distancia") +
    xlab(NULL) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle = 45, hjust=1),
          text=element_text(size=12))
  
  t_agg_wide_mat <-  t(agg_wide_mat)
  
  p2 <- t_agg_wide_mat %>%
    as.data.frame() %>% 
    dplyr::mutate(index = rownames(t_agg_wide_mat) %>% as.Date()) %>%
    tidyr::gather(key = type_col,value = value, -index) %>%
    dplyr::full_join(., hclus, by = "type_col") %>% 
    mutate(type_col = factor(type_col, levels = rev(as.character(names_order)))) %>% 
    ggplot(aes(x = index, y = value, colour = cluster_group)) +
    geom_line() +
    facet_wrap(~type_col, ncol = 1, strip.position="left") + 
    guides(color=FALSE) +
    theme_bw() + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank(),
          text=element_text(size=12)) +
    ylab(lab_y) +
    xlab(lab_x)
  
  gp1<-ggplotGrob(p1)
  gp2<-ggplotGrob(p2) 
  
  gridExtra::grid.arrange(gp2, gp1, ncol=2, widths=c(4,2))
}
