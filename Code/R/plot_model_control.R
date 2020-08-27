require(multipanelfigure)

plot_control <- function(m){
  p <- list()
  # qqplot
  p[[1]] <- ggplot() + 
    geom_qq(aes(sample = residuals(m)), alpha = 0.2) +
    geom_qq_line(aes(sample = residuals(m)), col = 'tomato', size = 1.5, linetype = 'dashed') +
    monkey_theme
  # residuals
  p[[2]] <- ggplot() +
    geom_point(aes(x = 1:length(residuals(m)), y = residuals(m)), alpha = 0.2) +
    xlab('Index') +
    ylab('Residuals') +
    geom_abline(aes(intercept = 0, slope = 0), col = 'tomato', size = 1.5, linetype = 'dashed') +
    monkey_theme
  # fitted vs residuals
  p[[3]] <- ggplot() +
    geom_point(aes(x = fitted(m), y = residuals(m)), alpha = 0.2) +
    xlab('Fitted') +
    ylab('Residuals') +
    geom_abline(aes(intercept = 0, slope = 0), col = 'tomato', size = 1.5, linetype = 'dashed') +
    monkey_theme
  # figure
  figure <- multi_panel_figure(columns = 2, rows = 2, panel_label_type = "none")
  figure %<>%
    fill_panel(p[[1]], column = 1, row = 1:2) %<>%
    fill_panel(p[[2]], column = 2, row = 1) %<>%
    fill_panel(p[[3]], column = 2, row = 2)
  
  return(figure)
}
