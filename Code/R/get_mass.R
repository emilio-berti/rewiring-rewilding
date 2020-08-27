if(!"phy" %in% ls()){
  phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols())
}

get_mass <- function(x){
  phy %>% 
    filter(Binomial.1.2 %in% x) %>% 
    transmute(Species = Binomial.1.2,
              Mass = Mass.g,
              IUCN = IUCN.Status.1.2)
}