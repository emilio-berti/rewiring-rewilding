library(tidyverse)
library(ape)
library(picante)

forest <- read.nexus("../../Data/PHYLACINE_1.2.0/Data/Phylogenies/Complete_phylogeny.nex")

get_ED <- function(species, extinct, replaced = "none"){
  ED_PN <- list()
  ED_CU <- list()
  ED_RW <- list()
  for(i in 1:30){
    tree <- forest[[sample(1000, 1)]]
    tree <- keep.tip(tree, species)
    ED_PN[[i]] <- evol.distinct(tree)
    ED_CU[[i]] <- ED_PN[[i]]
    ED_RW[[i]] <- ED_PN[[i]]
    for(j in 1:length(extinct)){
      dead <- which(tree$tip.label == extinct[j])
      inner <- max(mrca(tree)[tree$tip.label == tree$tip.label[dead]])
      lost_length <- tree$edge.length[tree$edge[ , 1] == inner & tree$edge[ , 2] == dead]
      ED_CU[[i]]$w[ED_CU[[i]]$Species == tree$tip.label[dead]] <- 0
      if(extinct[j] %in% replaced){
        ED_RW[[i]]$w[ED_RW[[i]]$Species == tree$tip.label[dead]] <- ED_RW[[i]]$w[ED_RW[[i]]$Species == tree$tip.label[dead]] - lost_length
      } else{
        ED_RW[[i]]$w[ED_RW[[i]]$Species == tree$tip.label[dead]] <- 0
      }
    }
  }
  pn <- do.call("rbind", ED_PN) %>% 
    as_tibble() %>% 
    group_by(Species) %>% 
    summarise(ED = mean(w)) %>% 
    add_column(Scenario = "PN") %>% 
    mutate(Species = as.character(Species))
  cu <- do.call("rbind", ED_CU) %>% 
    filter(w > 0) %>% 
    as_tibble() %>% 
    group_by(Species) %>% 
    summarise(ED = mean(w)) %>% 
    add_column(Scenario = "CU") %>% 
    mutate(Species = as.character(Species))
  rw <- do.call("rbind", ED_RW) %>% 
    filter(w > 0) %>% 
    as_tibble() %>% 
    group_by(Species) %>% 
    summarise(ED = mean(w)) %>% 
    add_column(Scenario = "RW") %>% 
    mutate(Species = as.character(Species))
  ed <- bind_rows(pn, cu, rw)
  return(ed)
}

web <- read_csv("../../Results/Webs/Random_web100.csv", col_types = cols())

species <- sort(unique(c(web$Predator, web$Prey)))

species <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols()) %>% 
  sample_n(500) %>%
  pull(Binomial.1.2)
extinct <- sample(species, 30)

a <- get_ED(species, extinct, replaced)
a %>% 
  ggplot() +
  geom_histogram(aes(ED, y = ..density..), binwidth = 0.1) +
  geom_density(aes(ED)) +
  facet_wrap(Scenario ~ ., ncol = 1, scales = "free") +
  scale_x_log10()

qqnorm(log10(a$ED))
qqline(log10(a$ED))

# qqnorm(a$ED)
# qqline(a$ED)

a %>% 
  group_by(Scenario) %>% 
  summarize(mu = mean(ED))
