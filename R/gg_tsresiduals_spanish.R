gg_tsresiduals_spanish <- function (data, type = "innovation", ...) 
{
  if (!fabletools::is_mable(data)) {
    abort("gg_tsresiduals() must be used with a mable containing only one model.")
  }
  data <- stats::residuals(data, type = type)
  out <- gg_tsdisplay_spanish(data, !!sym(".resid"), plot_type = "histogram", 
                      ...) 
  out[[1]] <- out[[1]] + ggplot2::ylab("Residuales")
  out
}