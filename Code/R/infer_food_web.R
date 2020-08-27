library(tidyverse)
library(raster)
library(sf)
library(foreach)
library(doParallel)
library(igraph)
library(ggraph)
library(cowplot)
library(tmap)

# set temporary directories
# Sys.setenv(R_SESSION_TMPDIR = "/NewSpace/Temp_R/")
# rasterOptions(tmpdir = "/NewSpace/Temp_R/")
rasterOptions(maxmemory = 10 * 10^9)

source("SERVER_present-natural_webs.R")
source("SERVER_current_webs.R")
source("SERVER_rewilded_webs.R")
<<<<<<< HEAD
=======
source("get_ED.R")
>>>>>>> 12b6b2da9189d1eccc59266d2db8099edab0a4b2
source("get_web.R")
source("get_trophic_levels.R")
source("get_TL_links.R")
source("get_graph.R")
data(World)

# import fully connected family-level food web for all mammals
family_food_web <- read_csv("../../Results/phylogenetic_food_web.csv", col_types = cols())
# import niche model parameters of optimization algorithm
niche_model <- read_csv("../../Results/convergence_optimization.csv", col_types = cols()) %>% 
  transmute(Parameter = Par, Value = `mean(Final)`)
# import niche model parameters of quantile regression
quantile_model <- read_csv("../../Results/pars_gravel.csv", col_types = cols()) %>% 
  transmute(Parameter = term, Value = estimate, Quantile = tau)

marine_families <- c("Balaenidae", "Balaenopteridae", "Delphinidae", "Dugongidae", "Eschrichtiidae", "Iniidae", "Monodontidae", "Neobalaenidae", "Odobenidae", "Otariidae", "Phocidae", "Phocoenidae", "Physeteridae", "Platanistidae", "Trichechidae", "Ziphiidae")
phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types= cols()) %>% 
  filter(
    !Family.1.2 %in% marine_families,
    Binomial.1.2 != "Ursus_maritimus",
    Genus.1.2 != "Homo",
    Order.1.2 != "Chiroptera"
  ) %>% 
  dplyr::select(Binomial.1.2, Family.1.2, Mass.g, IUCN.Status.1.2)

species <- phy %>% pull(Binomial.1.2)
species_rw <- phy %>% 
  filter(!IUCN.Status.1.2 %in% c("EX", "EP", "EW")) %>% 
  pull(Binomial.1.2)

current <- stack(paste0("../../Data/PHYLACINE_1.2/Data/Ranges/Current/", species, ".tif"), quick = T)
pn <- stack(paste0("../../Data/PHYLACINE_1.2/Data/Ranges/Present_natural/", species, ".tif"), quick = T)
rw <- stack(paste0("../../Data/Rewilding_ranges/", species_rw, ".tif"), quick = T)

WDPA <- read_sf("../../Data/PA_5000km_bioregions.shp") %>%
  st_transform(st_crs(crs(current))) %>%
  mutate(Area = as.numeric(st_area(geometry)) / 10^6) %>%
  filter(Area > 5000)

<<<<<<< HEAD
### For loop over protected areas ----
cl <- makeCluster(detectCores() - 2, type='PSOCK')
registerDoParallel(cl)
for(fw in 1:nrow(WDPA)){#c(94)){ # this is Yellowstone
=======
Ecozone <- read_sf("../../Data/Ecozone.shp") %>% 
  filter(REALM != "OC") %>% 
  st_transform(st_crs(WDPA))

set.seed(666)
Random_fw <- Ecozone
for(i in 1:nrow(Ecozone)){
  if(i < 6){
    ecozone <- Ecozone[i, ] %>% 
      st_crop(st_bbox(Ecozone[i, ])) %>% 
      st_cast("POLYGON")
    geom <- ecozone$geometry[which.max(st_area(ecozone))]
    rand_points <- st_sample(
      geom, 
      size = 37, 
      type = "random", 
      exact = TRUE
    ) %>% 
      st_buffer(40000) %>% 
      st_union()
  } else{
    Europe <- World %>% 
      filter(continent == "Europe", name != "Russia") %>% 
      st_transform(st_crs(Ecozone))
    rand_points <- st_sample(
      Europe, 
      size = 15, 
      type = "random", 
      exact = TRUE
    ) %>% 
      st_buffer(40000) %>% 
      st_union()
    ecozone <- Ecozone[i, ] %>% 
      st_crop(st_bbox(Ecozone[i, ])) %>% 
      st_cast("POLYGON")
    geom <- ecozone$geometry[which.max(st_area(ecozone))]
    rand_points <- st_sample(
      geom, 
      size = 22, 
      type = "random", 
      exact = TRUE
    ) %>% 
      st_buffer(40000) %>% 
      st_union() %>% 
      st_union(rand_points)
  }
  Random_fw$geometry[i] <- rand_points
}

ggplot(mutate(Ecozone, Ecozone = REALM) %>% st_transform(st_crs(4326))) +
  geom_sf(aes(fill = Ecozone)) +
  geom_sf(data = st_transform(Random_fw, st_crs(4326)), fill = "black", size = 1) +
  theme(
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Random areas")

cl <- makeCluster(detectCores() - 5, type='PSOCK')
registerDoParallel(cl)

### loop over protected areas ----
for(fw in 1:nrow(WDPA)){
>>>>>>> 12b6b2da9189d1eccc59266d2db8099edab0a4b2
  print(paste0("Food web ", fw, " out of ", nrow(WDPA))) #counter
  #infer communities under the three scenarios
  pn_community <- infer_pn(WDPA[fw, ], pn)
  #exclude PA without mammals in present-natural
  if(length(pn_community) == 0){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Food web ", fw, " has not mammals in current and present-natural"))
    sink()
    next()
  }
  current_community <- infer_current(WDPA[fw, ], current)
  rw_community <- infer_rewilded(WDPA[fw, ], rw)
  
  #get food webs under the three scenarios with species ED
  web_current <- get_web(current_community, method = "QR") %>% 
    mutate(Scenario = "CU")
  web_pn <- get_web(pn_community, method = "QR") %>% 
    mutate(Scenario = "PN")
  web_rw <- get_web(rw_community, method = "QR") %>% 
    mutate(Scenario = "RW")
  
  pn_TL <- trophic_levels(pn_community)$TL_structure #this return the first result
  g <- get_TL_links(web_pn)
  if(class(g) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Food web ", fw, " does not have herbivores"))
    sink()
    next()
  }
  if(class(get_graph(g)) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Food web ", fw, " does not have present-natural edges"))
    sink()
    next()
  }
  pn_edges <- get_graph(g)$Links #this return the second result
  pn_p <- get_graph(g)$plot + ggtitle("Present natural") + 
    theme(plot.margin = unit(c(0,0.5,0,0.5), "lines"), 
          plot.title = element_text(hjust = 0.5))
  
  cu_TL <- trophic_levels(current_community)$TL_structure #this return the first result
  g <- get_TL_links(web_current)
  if(class(g) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Food web ", fw, " does not have herbivores"))
    sink()
    next()
  }
  if(class(get_graph(g)) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Food web ", fw, " does not have current edges"))
    sink()
    next()
  }
  cu_edges <- get_graph(g)$Links #this return the second result
  cu_p <- get_graph(g)$plot + ggtitle("Current") + 
    theme(plot.margin = unit(c(0,0.5,0,0.5), "lines"), 
          plot.title = element_text(hjust = 0.5))
  
  rw_TL <- trophic_levels(rw_community)$TL_structure #this return the first result
  g <- get_TL_links(web_rw)
  if(class(g) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Food web ", fw, " does not have herbivores"))
    sink()
    next()
  }
  if(class(get_graph(g)) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Food web ", fw, " does not have rewilding edges"))
    sink()
    next()
  }
  rw_edges <- get_graph(g)$Links #this return the second result
  rw_p <- get_graph(g)$plot + ggtitle("Rewilded") + 
    theme(plot.margin = unit(c(0,0.5,0,0.5), "lines"), 
          plot.title = element_text(hjust = 0.5))
  
<<<<<<< HEAD
  # Figure ----
  plot_grid(pn_p, cu_p, rw_p, ncol = 3)
  # legend("right", border = F, horiz = F, cex = 1.2,
  #        legend = c("Microherbivores", "Megaherbivores", "Mircocarnivores", "Mesocarnivores", "Megacarnivores"), 
  #        fill = wesanderson::wes_palette("Zissou1"))
=======
  # Figure of networks
  plot_grid(pn_p, cu_p, rw_p, ncol = 3)
>>>>>>> 12b6b2da9189d1eccc59266d2db8099edab0a4b2
  ggsave(paste0("../../Results/Webs/Web_", fw, ".pdf"), width = 11, height = 4)
  
  # Statistics to csv
  tibble(
    Size = c(pn_TL$Size, cu_TL$Size, rw_TL$Size),
    n = c(pn_TL$n, cu_TL$n, rw_TL$n),
    Scenario = factor(c(rep("PN", nrow(pn_TL)), rep("CU", nrow(cu_TL)), rep("RW", nrow(rw_TL))),
                      levels = c("PN", "CU", "RW"))
  ) %>% 
    spread(Scenario, n, fill = 0) %>% 
    write_csv(paste0("../../Results/Webs/TL_web", fw, ".csv"))
  
  tibble(
    i = c(pn_edges$i, cu_edges$i, rw_edges$i),
    j = c(pn_edges$j, cu_edges$j, rw_edges$j),
    w = c(pn_edges$w, cu_edges$w, rw_edges$w),
    Scenario = factor(c(rep("PN", nrow(pn_edges)), rep("CU", nrow(cu_edges)), rep("RW", nrow(rw_edges))), 
                      levels = c("PN", "CU", "RW"))
  ) %>% 
    spread(Scenario, w, fill = 0) %>% 
    arrange(i, j) %>% 
    write_csv(paste0("../../Results/Webs/Graph_web", fw, ".csv"))
  
  # Web to csv file 
  web_current %>%
    mutate(Scenario = "CU") %>%
    rbind(web_pn %>% mutate(Scenario = "PN")) %>% 
    rbind(web_rw %>% mutate(Scenario = "RW")) %>% 
    filter(Predator != Prey) %>% 
    write_csv(paste0("../../Results/Webs/Web", fw, ".csv"))
}
<<<<<<< HEAD
=======


#loop over random points ----
Random_fw <- Random_fw %>% st_cast("POLYGON")
write_sf(Random_fw, "../../Data/Random_points.shp")
for(fw in 1:nrow(Random_fw)){
  print(paste0("Random food web ", fw, " out of ", nrow(Random_fw))) #counter
  #infer communities under the three scenarios
  pn_community <- infer_pn(Random_fw[fw, ], pn)
  #exclude PA without mammals in present-natural
  if(length(pn_community) == 0){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Random food web ", fw, " has not mammals in current and present-natural"))
    sink()
    next()
  }
  current_community <- infer_current(Random_fw[fw, ], current)
  rw_community <- infer_rewilded(Random_fw[fw, ], rw)
  
  #get food webs under the three scenarios
  if(all(get_web(pn_community, method = "QR") != 0,
         get_web(current_community, method = "QR") != 0,
         get_web(rw_community, method = "QR") != 0)){
    web_pn <- get_web(pn_community, method = "QR") %>% 
      mutate(Scenario = "PN")
    web_current <- get_web(current_community, method = "QR") %>% 
      mutate(Scenario = "CU")
    web_rw <- get_web(rw_community, method = "QR") %>% 
      mutate(Scenario = "RW")
  } else{
    sink("../../Results/Log.txt", append = T)
    print(paste0("Random food web ", fw, " has one scenario withtout links"))
    sink()
  }
  pn_TL <- trophic_levels(pn_community)$TL_structure #this return the first result
  g <- get_TL_links(web_pn)
  if(class(g) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Random food web ", fw, " does not have herbivores"))
    sink()
    next()
  }
  if(class(get_graph(g)) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Random food web ", fw, " does not have present-natural edges"))
    sink()
    next()
  }
  pn_edges <- get_graph(g)$Links #this return the second result
  pn_p <- get_graph(g)$plot + ggtitle("Present natural") + 
    theme(plot.margin = unit(c(0,0.5,0,0.5), "lines"), 
          plot.title = element_text(hjust = 0.5))
  
  cu_TL <- trophic_levels(current_community)$TL_structure #this return the first result
  g <- get_TL_links(web_current)
  if(class(g) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Random food web ", fw, " does not have herbivores"))
    sink()
    next()
  }
  if(class(get_graph(g)) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Random food web ", fw, " does not have current edges"))
    sink()
    next()
  }
  cu_edges <- get_graph(g)$Links #this return the second result
  cu_p <- get_graph(g)$plot + ggtitle("Current") + 
    theme(plot.margin = unit(c(0,0.5,0,0.5), "lines"), 
          plot.title = element_text(hjust = 0.5))
  
  rw_TL <- trophic_levels(rw_community)$TL_structure #this return the first result
  g <- get_TL_links(web_rw)
  if(class(g) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Random food web ", fw, " does not have herbivores"))
    sink()
    next()
  }
  if(class(get_graph(g)) == "numeric"){
    sink("../../Results/Log.txt", append = T)
    print(paste0("Random food web ", fw, " does not have rewilding edges"))
    sink()
    next()
  }
  rw_edges <- get_graph(g)$Links #this return the second result
  rw_p <- get_graph(g)$plot + ggtitle("Rewilded") + 
    theme(plot.margin = unit(c(0,0.5,0,0.5), "lines"), 
          plot.title = element_text(hjust = 0.5))
  
  # Figure of networks
  plot_grid(pn_p, cu_p, rw_p, ncol = 3)
  ggsave(paste0("../../Results/Webs/Random_web_", fw, ".pdf"), width = 11, height = 4)
  
  # Statistics to csv
  tibble(
    Size = c(pn_TL$Size, cu_TL$Size, rw_TL$Size),
    n = c(pn_TL$n, cu_TL$n, rw_TL$n),
    Scenario = factor(c(rep("PN", nrow(pn_TL)), rep("CU", nrow(cu_TL)), rep("RW", nrow(rw_TL))),
                      levels = c("PN", "CU", "RW"))
  ) %>% 
    spread(Scenario, n, fill = 0) %>% 
    write_csv(paste0("../../Results/Webs/TL_random_web", fw, ".csv"))
  
  tibble(
    i = c(pn_edges$i, cu_edges$i, rw_edges$i),
    j = c(pn_edges$j, cu_edges$j, rw_edges$j),
    w = c(pn_edges$w, cu_edges$w, rw_edges$w),
    Scenario = factor(c(rep("PN", nrow(pn_edges)), rep("CU", nrow(cu_edges)), rep("RW", nrow(rw_edges))), 
                      levels = c("PN", "CU", "RW"))
  ) %>% 
    spread(Scenario, w, fill = 0) %>% 
    arrange(i, j) %>% 
    write_csv(paste0("../../Results/Webs/Graph_random_web", fw, ".csv"))
  
  # Web to csv file 
  web_current %>%
    mutate(Scenario = "CU") %>%
    rbind(web_pn %>% mutate(Scenario = "PN")) %>% 
    rbind(web_rw %>% mutate(Scenario = "RW")) %>% 
    filter(Predator != Prey) %>% 
    write_csv(paste0("../../Results/Webs/Random_web", fw, ".csv"))
}
>>>>>>> 12b6b2da9189d1eccc59266d2db8099edab0a4b2
stopImplicitCluster()
