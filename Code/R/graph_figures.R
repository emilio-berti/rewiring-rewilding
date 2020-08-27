plot_edges <- function(eco){
  df <- raw_edges %>% 
    filter(Ecozone == eco) %>% 
    mutate(Current = CU / PN, Rewilding = RW / PN) %>% 
    group_by(I, J) %>% 
    summarize(
      Current = mean(Current),
      Rewilding = mean(Rewilding)
    )
  
  g <- graph(edges = t(cbind(df$I, df$J)))
  V <- length(vertices(g)[[1]][2])
  if(V < 5){
    g <- add_vertices(g, nv = 5 - V)
  }
  vertex_attr(g)$name[(V + 1):5] <- setdiff(unique(raw_levels$Size), names(igraph::vertices(g)[[1]][1]))
  edge_attr(g, "weight_cu", index = E(g)) <- NA
  edge_attr(g, "weight_rw", index = E(g)) <- NA
  for(i in 1:length(edge_attr(g)$weight_cu)){
    edge_attr(g)$weight_cu[i] <- df[i, "Current"]
    edge_attr(g)$weight_rw[i] <- df[i, "Rewilding"]
  }
  edge_attr(g)$weight_cu <- unlist(edge_attr(g)$weight_cu)
  edge_attr(g)$weight_rw <- unlist(edge_attr(g)$weight_rw)
  
  pos = layout_as_tree(g)
  pos[which(vertex_attr(g)$name == "Microherbivore"), ] <- c(-10, 0)
  pos[which(vertex_attr(g)$name == "Megaherbivore"), ] <- c(10, 0)
  pos[which(vertex_attr(g)$name == "Microcarnivore"), ] <- c(-2.5, 10)
  pos[which(vertex_attr(g)$name == "Mesocarnivore"), ] <- c(2.5, 10)
  pos[which(vertex_attr(g)$name == "Megacarnivore"), ] <- c(0, 20)
  
  links <- cbind(get.edgelist(g), E(g)$weight) %>% 
    as_tibble() %>% 
    transmute(i = V1, j = V2, w = 1)
  
  pn_layout <- create_layout(
    g, layout = "manual", 
    node.position = data.frame(x = pos[ , 1], y = pos[ , 2])
  )
  
  Category <- factor(vertex_attr(g)$name, levels = c("Microherbivore", "Megaherbivore", "Microcarnivore", "Mesocarnivore", "Megacarnivore"))
  
  p <- ggraph(pn_layout) +
    geom_edge_arc(aes(edge_width = 1), show.legend = F, curvature = 0, fold = F,
                  start_cap = circle(0, 'mm'), end_cap = circle(0, 'mm'), edge_colour = "#00A08A") +
    geom_edge_arc(aes(edge_width = edge_attr(g)$weight_rw), show.legend = F, 
                  curvature = 0, fold = F, start_cap = circle(0, 'mm'), 
                  end_cap = circle(0, 'mm'), edge_colour = "black") +
    geom_edge_arc(aes(edge_width = edge_attr(g)$weight_cu), show.legend = F, 
                  curvature = 0, fold = F, start_cap = circle(0, 'mm'), 
                  end_cap = circle(0, 'mm'), edge_colour = "#F2AD00") +
    geom_node_point(show.legend = F) +
    theme(
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none"
    ) +
    geom_node_circle(aes(r = 0.5, fill = Category)) +
    scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 5)) +
    ggtitle(eco)
  
  return(p)
  # geom_rect(aes(xmin = -13, xmax = -8, ymin = -0.125, ymax = 0.125), fill = "white", col = "gainsboro") +
  # geom_rect(aes(xmin = 8, xmax = 13, ymin = -0.125, ymax = 0.125), fill = "white", col = "gainsboro") +
  # geom_rect(aes(xmin = -8, xmax = -2, ymin = 1 - 0.125, ymax = 1 + 0.125), fill = "white", col = "gainsboro") +
  # # geom_rect(aes(xmin = 1, xmax = 11, ymin = 2.9, ymax = 3.1), fill = "white", col = "gainsboro") +
  # # geom_rect(aes(xmin = -5, xmax = 5, ymin = 3.9, ymax = 4.1), fill = "white", col = "gainsboro") +
  # geom_text(aes(x = -10.5, y = 0), label = "Microherbivores", size = 5) +
  # geom_text(aes(x = 10.5, y = 0), label = "Megaherbivore", size = 5) +
  # geom_text(aes(x = -6, y = 3), label = "Microcarnivore", size = 5) 
  # geom_text(aes(x = 6, y = 3), label = "Mesocarnivore", size = 5) +
  # geom_text(aes(x = 0, y = 4), label = "Megacarnivore", size = 5) +
}