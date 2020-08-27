library(tidyverse)
library(ape)
library(ggtree)
library(phytools)

marine_families <- c("Balaenidae", "Balaenopteridae", "Delphinidae", "Dugongidae", "Eschrichtiidae", "Iniidae", "Monodontidae", "Neobalaenidae", "Odobenidae", "Otariidae", "Phocidae", "Phocoenidae", "Physeteridae", "Platanistidae", "Trichechidae", "Ziphiidae")

phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_type = cols()) %>% 
  filter(
    !Family.1.2 %in% marine_families,
    Genus.1.2 != "Homo",
    Binomial.1.2 != "Ursus_maritimus",
    Order.1.2 != "Chiroptera"
  )

forest <- read.nexus("../../Data/PHYLACINE_1.2.0/Data/Phylogenies/Complete_phylogeny.nex")
tree <- forest[[1]]
tree <- keep.tip(tree, phy$Binomial.1.2)
extinct <- sapply(tree$tip.label, function(x) 
  if((phy %>% filter(Binomial.1.2 == x) %>% pull(IUCN.Status.1.2)) %in% c("EX", "EW", "EP"))
    TRUE
  else
    FALSE
) %>% as_vector()

ggtree(tree)
collapseTree(tree)
