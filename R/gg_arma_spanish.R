gg_arma_spanish <- function (data) 
{
  if (!fabletools::is_mable(data)) {
    abort("gg_arma() must be used with a mable containing models that compute ARMA roots")
  }
  
  # fcts <- c(key(data), sym(".model"))
  data <- data %>% 
    fabletools::glance() %>% 
    gather("type", "root", !!sym("ar_roots"), !!sym("ma_roots")) %>% 
    unnest("root") %>% 
    filter(!is.na(!!sym("root"))) %>% 
    mutate(type = factor(!!sym("type"), 
                         levels = c("ar_roots", "ma_roots"), 
                         labels = c("Raíces AR", "Raíces MA")), 
           `Círculo Unitario` = factor(abs(1/!!sym("root")) >  1, levels = c(TRUE, FALSE), 
                               labels = c("Fuera", "Dentro")))
  ggplot(data, aes(x = Re(1/!!sym("root")), y = Im(1/!!sym("root")), 
                   colour = !!sym("Círculo Unitario"))) + 
    ggplot2::annotate("path", 
                      x = cos(seq(0, 2 * pi, length.out = 100)), 
                      y = sin(seq(0,  2 * pi, length.out = 100))) +
    ggplot2::geom_vline(xintercept = 0) + 
    ggplot2::geom_hline(yintercept = 0) + 
    geom_point() + 
    ggplot2::coord_fixed(ratio = 1) + 
    facet_grid(cols = vars(!!sym("type"))) +
    ylab("lm(1/raíz)") +
    xlab("Re(1/raíz)") 
}