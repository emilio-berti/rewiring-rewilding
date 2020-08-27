library(tidyverse)
library(igraph)
library(ggraph)
library(sf)
library(tmap)


data('World')

WDPA <- read_sf('Data/PA_5000km_bioregions.shp') %>%
  mutate(Area = as.numeric(st_area(geometry)) / 10^6) %>%
  filter(Area > 5000)

random_p <- read_sf('Data/Random_points.shp')

ggplot() +
  geom_sf(data = World, fill = NA) +
  geom_sf(data = random_p[126, ], col = 'tomato') +
  geom_sf(data = WDPA[203, ], col = 'green')

# Yellowstone - Web94
# Swedish protected area - Web203
# Italy - Web_random188


# figure ------------------------------------------------------------------
phy <- read_csv('Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv', col_type = cols())
fw <- read_csv('Results/Webs/Web203.csv', col_type = cols())
#fw <- read_csv('Results/Webs/Random_web188.csv', col_type = cols())

pn <- graph(t(cbind(fw$Prey[fw$Scenario == 'PN'], fw$Predator[fw$Scenario == 'PN'])), directed = F)
cu <- graph(t(cbind(fw$Prey[fw$Scenario == 'CU'], fw$Predator[fw$Scenario == 'CU'])), directed = F)
rw <- graph(t(cbind(fw$Prey[fw$Scenario == 'RW'], fw$Predator[fw$Scenario == 'RW'])), directed = F)

V(pn)$mass <- sapply(V(pn)$name, function(x) log10(phy$Mass.g[phy$Binomial.1.2 == x]))
V(cu)$mass <- sapply(V(cu)$name, function(x) log10(phy$Mass.g[phy$Binomial.1.2 == x]))
V(rw)$mass <- sapply(V(rw)$name, function(x) log10(phy$Mass.g[phy$Binomial.1.2 == x]))

V(pn)$col <- sapply(V(pn)$name, function(x){
  if(!x %in% V(cu)$name){
    'tomato'
  } else{
    'steelblue'
  }
})

V(rw)$col <- sapply(V(rw)$name, function(x){
  if(x %in% V(cu)$name){
    'steelblue'
  } else{
    'darkolivegreen3'
  } 
})

pn_edges <- get.edgelist(pn) %>% 
  as_tibble() %>% 
  mutate(Edge = modify2(V1, V2, function(x, y) paste0(x, ' - ', y)))
cu_edges <- get.edgelist(cu) %>% 
  as_tibble() %>% 
  mutate(Edge = modify2(V1, V2, function(x, y) paste0(x, ' - ', y)))
rw_edges <- get.edgelist(rw) %>% 
  as_tibble() %>% 
  mutate(Edge = modify2(V1, V2, function(x, y) paste0(x, ' - ', y)))

E(pn)$col <- sapply(pn_edges$Edge, function(x){
  if(!x %in% cu_edges$Edge){
    'tomato'
  } else{
    'steelblue'
  }
})

E(rw)$col <- sapply(rw_edges$Edge, function(x){
  if(x %in% cu_edges$Edge){
    'steelblue'
  } else{
    'darkolivegreen3'
  }
})

svg('Manuscript/Figures/sweden.svg', width = 16, height = 6)
par(mfrow = c(1, 3), 
    bg = 'white',
    col.main = 'white',
    cex.main = 4)
plot(
  pn,
  layout = layout.sphere,
  vertex.size = V(pn)$mass + 2,
  vertex.label = NA,
  edge.curved = T,
  vertex.color = V(pn)$col,
  edge.color = E(pn)$col,
  edge.width = 1,
  main = 'Present natural'
)

plot(
  cu,
  layout = layout.sphere,
  vertex.size = V(cu)$mass + 2,
  vertex.label = NA,
  edge.curved = T,
  vertex.color = 'steelblue',
  edge.color = 'steelblue',
  edge.width = 1,
  main = 'Current'
)

plot(
  rw,
  layout = layout.sphere,
  vertex.size = V(rw)$mass + 2,
  vertex.label = NA,
  edge.curved = T,
  vertex.color = V(rw)$col,
  edge.color = E(rw)$col,
  edge.width = 1,
  main = 'Rewilded'
)

dev.off()

