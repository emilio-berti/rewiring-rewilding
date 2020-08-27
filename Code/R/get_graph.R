get_graph <- function(g){
<<<<<<< HEAD
  V <- length(vertices(g)[[1]][2])
  if(V < 5){
    g <- add_vertices(g, nv = 5 - V)
=======
  V <- igraph::vcount(g)
  if(V < 6){
    g <- add_vertices(g, nv = 6 - V)
>>>>>>> 12b6b2da9189d1eccc59266d2db8099edab0a4b2
  }
  pos = layout_as_tree(g)
  pos[1, ] <- c(-10, 0)
  pos[2, ] <- c(0, 0)
  pos[3, ] <- c(10, 0)
  pos[4, ] <- c(-2.5, 1)
  pos[5, ] <- c(2.5, 1)
  pos[6, ] <- c(0, 2)
  
  if(ecount(g) > 0){
    links <- cbind(get.edgelist(g), E(g)$weight) %>% 
      as_tibble() %>% 
      transmute(i = V1, j = V2, w = V3)
    
    g_layout <- create_layout(
      g, layout = "manual", 
      node.position = data.frame(x = pos[ , 1], y = pos[ , 2])
    )
    
    p <- ggraph(g_layout) +
      geom_edge_arc(aes(edge_width = E(g)$weight), show.legend = F, curvature = 0, fold = F,
                    start_cap = circle(0, 'mm'), end_cap = circle(0, 'mm')) +
      geom_node_point(aes(fill = factor(1:6)), shape = 21, size = 10, show.legend = F) +
      theme(
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()
      ) +
      scale_fill_manual(values = c("#00637c", wesanderson::wes_palette("Zissou1")))
    
    return(list(Links = links, plot = p))
  } else{
    return(0)
  }
}


