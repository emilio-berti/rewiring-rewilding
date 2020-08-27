library(tidyverse)

source("get_mass.R")

repl <- read_csv("../../Results/Replacements.csv", col_types = cols()) %>% 
  filter(
    Ecozone != "Ecozone",
    Replacement != "No Replacement"
  ) %>% 
  mutate(
    `Extinct species` = as.numeric(modify(Extinct, function(x) get_mass(x)$Mass)),
    `Candidate replacements` = as.numeric(modify(Replacement, function(x) get_mass(x)$Mass))
  ) 

repl %>% 
  select(`Extinct species`, `Candidate replacements`) %>% 
  unique() %>% 
  gather(Taxon) %>% 
  mutate(Taxon = factor(Taxon, levels = c("Extinct species", "Candidate replacements"))) %>% 
  ggplot() +
  geom_histogram(aes(value / 1000), fill = "gainsboro", col = "black", binwidth = 0.25) +
  scale_x_log10(label = scales::comma) +
  ylab("Number of species") +
  xlab("Body mass (kg)") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gainsboro"),
    axis.line = element_line(),
    plot.title = element_text(hjust = 0.5)
  ) +
  facet_wrap(Taxon ~ ., ncol = 1)

ggsave("../../Manuscript/Figures/candidates_mass.png", width = 6, height = 6)

repl <- phy %>% 
  filter(Binomial.1.2 %in% (repl %>% pull(Extinct) %>% unique())) %>% 
  select(Family.1.2, Binomial.1.2, Mass.g) %>% 
  mutate(Extinct = Binomial.1.2) %>% 
  full_join(repl) %>% 
  arrange(desc(Family.1.2))

repl %>% 
  transmute(
    Extinct = gsub("_", " ", Extinct),
    Replacement = gsub("_", " ", Replacement),
    Ecozone
  ) %>% 
  skimr::kable()
