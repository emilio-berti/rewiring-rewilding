library(tidyverse)
library(igraph)

files <- list.files("../../Results/Webs/", pattern = "csv")
files <- files[grep("Web", files)]

fw <- read_csv(paste0("../../Results/Webs/", files[100]), col_types = cols()) %>% 
  mutate(Color = modify(Scenario, function(x)
    if(x == "PN")
      return("green4")
    else if(x == "CU")
      return("#E1AF00")
    else if(x == "RW")
      return("#78B7C5")
    ))

pn <- fw %>% filter(Scenario == "PN")
cu <- fw %>% filter(Scenario == "CU")
rw <- fw %>% filter(Scenario == "RW")

g <- make_undirected_graph(t(fw[ , c(2, 1)]))
plot.igraph(
  g, layout = layout_in_circle(g), 
  edge.width = 0.3,
  vertex.size = 10,
  vertex.label = NA,
  vertex.color = fw$Color
)

pn <- make_undirected_graph(t(fw[fw$Scenario == "PN" , c(2, 1)]))
plot.igraph(
  pn, layout = layout_in_circle(pn), 
  edge.width = 0.3,
  vertex.size = 10,
  vertex.label = NA,
  edge.color = "grey30"
)
