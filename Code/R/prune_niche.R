# x is the body mass of predator
niche <- function(x){
  center <- x * 0.69
  radius <- x * 0.51
  return(cbind(center, radius))
}

# x is preadator body mass
# y is prey body mass
prune_niche <- function(x, y){
  center <- x * 0.69
  radius <- x * 0.51
  if(y < (center - radius) | y > (center + radius)){
    return(F)
  } else{
    return(T)
  }
}

# dataset should be a tibble of this form:
# FamPred FamPrey        
# Canidae Sciuridae      
# Canidae Cervidae       
# Canidae Geomyidae      
# ....... .........
fw_inference <- function(dataset, i){
  
  # get all species of the family in i-th row and predator
  pred <- Family %>% 
    filter(Family.1.2 == dataset$FamPred[i]) %>% 
    pull(Species)
  
  # get all species of the family in i-th row and prey
  prey <- Family %>% 
    filter(Family.1.2 == dataset$FamPrey[i]) %>% 
    pull(Species)
  
  # create full connected bipartite graph and get body masses
  all_combinations <- expand.grid(pred, prey) %>% 
    as_tibble() %>% 
    mutate(
      `Mass Pred` = map(Var1, function(x){
        Family %>% filter(Species == x) %>% pull(Mass.g) %>% log10()
      }) %>% unlist(),
      `Mass Prey` = map(Var2, function(x){
        Family %>% filter(Species == x) %>% pull(Mass.g) %>% log10()
      }) %>% unlist()
    )
  
  # remove interactions not supported by niche model
  pruned_web <- all_combinations %>% 
    mutate(Niche = map2(`Mass Pred`, `Mass Prey`, prune_niche) %>% unlist()) %>% 
    filter(Niche == T) %>% 
    transmute(Pred = Var1, Prey = Var2,
              `Mass Pred`, `Mass Prey`)
  
  return(pruned_web)
  
}


Family <- read_csv("/home/GIT/Trophic_restoration/Data/PHYLACINE_1.2.0/Data/Traits/Trait_data.csv") %>% 
  mutate(Species = gsub("_", " ", Binomial.1.2)) %>% 
  select(Species, Mass.g, Family.1.2)

# add family to species
interactions %>% 
  mutate(
    FamPred = map(Predator, function(x){
      Family %>% 
        filter(Species == x) %>% 
        pull(Family.1.2)
    }) %>% 
      unlist(),
    FamPrey = map(Prey, function(x){
      Family %>% 
        filter(Species == x) %>% 
        pull(Family.1.2)
    }) %>% 
      unlist()
  ) -> interactions

# create femily-family interaction networks with unique entries
phylo_web <- interactions %>% 
  select(FamPred, FamPrey) %>% 
  unique()


x <- log10(seq(10, 1000000, length = 20))
y <- niche(x)
model <- tibble(X = x, Center = y[ , 1], Radius = y[ , 2])

# fw <- fw_inference(phylo_web, 62)
# lapply all 1:nrow(phylo_web)
T0 <- Sys.time()
library(foreach)
library(doParallel)

registerDoParallel(4)
foreach(i = 1:nrow(phylo_web)) %dopar% {
  fw_inference(phylo_web, i)
}

fw <- lapply(1:nrow(phylo_web), function(x) fw_inference(phylo_web, x)) %>% 
  do.call(rbind, .)
Sys.time() - T0

ggplot() +
  geom_line(data = model, aes(X, Center), linetype = "dashed") +
  geom_line(data = model, aes(X, Center + Radius)) +
  geom_line(data = model, aes(X, Center - Radius)) +
  geom_point(data = fw, aes(`Mass Pred`, `Mass Prey`), alpha = 0.3, size = 0.5, pch = 21) +
  geom_point(data = interactions, aes(log10(`Pred Mass`), log10(`Prey Mass`)), alpha = 1, size = 2, col = "tomato") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line()
  )

plot(c(3, 5), c(-2, 8))
lines(x, y[ , 1], lt = "dashed")
lines(x, (y[ , 1] + y[ , 2]))
lines(x, (y[ , 1] - y[ , 2]))
points(a$`Mass Pred`, a$`Mass Prey`, pch = 20, col = rgb(0, 0, 0, 0.2))


# graph -------------------------------------------------------------------
library(igraph)

rbind(fw$Pred, fw$Prey)


