get_graph <- function(g){
  pos = layout_as_tree(g)
  pos[1, ] <- c(-2.5, 2)
  pos[2, ] <- c(2.5, 2)
  pos[3, ] <- c(0, 3)
  pos[4, ] <- c(0, 4)
  pos[5, ] <- c(0, 5)
  
  g_layout <- create_layout(
    g, layout = "manual", 
    node.position = data.frame(x = pos[ , 1], y = pos[ , 2])
  )
  
  p <- ggraph(g_layout) +
    geom_node_point(shape = 4, size = 10, show.legend = F, col = "white") + 
    geom_edge_arc(aes(edge_width = E(g)$weight), show.legend = F, curvature = 0, fold = F, 
                  start_cap = circle(0, 'mm'), end_cap = circle(0, 'mm')) +
    geom_rect(aes(xmin = -3.5, xmax = -1.5, ymin = 1.9, ymax = 2.1), fill = "white", col = "gainsboro") +
    geom_rect(aes(xmin = 1.5, xmax = 3.5, ymin = 1.9, ymax = 2.1), fill = "white", col = "gainsboro") +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = 2.9, ymax = 3.1), fill = "white", col = "gainsboro") +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = 3.9, ymax = 4.1), fill = "white", col = "gainsboro") +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = 4.9, ymax = 5.1), fill = "white", col = "gainsboro") +
    geom_text(aes(x = -2.5, y = 2), label = "Microherbivores", size = 5) +
    geom_text(aes(x = 2.5, y = 2), label = "Megaherbivore", size = 5) +
    geom_text(aes(x = 0, y = 3), label = "Microcarnivore", size = 5) +
    geom_text(aes(x = 0, y = 4), label = "Mesocarnivore", size = 5) +
    geom_text(aes(x = 0, y = 5), label = "Megacarnivore", size = 5) +
    theme(
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
  return(E(g)$weight, p)
}