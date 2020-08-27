mix_color <- function(x, y, alpha = 0.5) {
  col1 <- colorspace::RGB(t(col2rgb(x)) / 255)
  col2 <- colorspace::RGB(t(col2rgb(y)) / 255)
  new_col <- colorspace::mixcolor(alpha, col1, col2)
  new_col <- rgb(new_col@coords[1],
                 new_col@coords[2],
                 new_col@coords[3])
  return(new_col)
}
