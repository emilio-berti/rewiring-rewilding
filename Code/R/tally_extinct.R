library(tidyverse)

source("select_species.R")

phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv")

repl <- read_csv("../../Results/Replacements.csv")

fam <- repl %>% 
  filter(Replacement != "No Replacement") %>% 
  pull(Extinct) %>% 
  unique() %>% 
  sort()

d <- phy %>% 
  filter(Binomial.1.2 %in% c(extinct, alive),
         Genus.1.2 != "Homo") %>% 
  mutate(Group = Family.1.2) %>% 
  mutate(Group = map2(Order.1.2, Group, function(x, y) {
    if (x == "Proboscidea") 
    {
      "Proboscidea"
    } else {
      y
    }
  }) %>% unlist()) %>% 
  transmute(Species = Binomial.1.2,
            Group,
            Extinct = map(IUCN.Status.1.2, function(x) {
              if (x %in% c("EX", "EP", "EW")) {
                TRUE
              } else {
                FALSE
              }
            }) %>% unlist()) %>% 
  filter(Extinct == TRUE) %>% 
  group_by(Group) %>% 
  add_tally() %>% 
  ungroup()

d <- d %>% 
  mutate(Replaced = map(Species, function(x) {
    ans <- repl %>% 
      filter(Extinct == x, Replacement != "No Replacement") %>% 
      pull(Replacement)
    if (!is_empty(ans)) {
      "Replaced through rewilding"
    } else {
      "Not replaced"
    }
  }) %>% unlist())

d %>% 
  ggplot() +
  geom_bar(aes(Group, fill = Replaced), position = "identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("tomato", "green4")) +
  xlab("") +
  ylab("Number of species") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.4),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

ggsave("../../Manuscript/Figures/tally_replaced.png", width = 10, height = 6)
