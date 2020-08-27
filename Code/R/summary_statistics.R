library(tidyverse)

# trophic levels ---------------------------
raw_lev <- read_rds("../../Results/raw_level_model.rds")
rand_lev <- read_rds("../../Results/random_level_model.rds")
d <- bind_rows(
  mutate(raw_lev@frame, Area = "Protected"),
  mutate(rand_lev@frame, Area = "Random")
)

means <- d %>% 
  group_by(Area, Size, Scenario) %>% 
  summarise(Mean = mean(Proportion)) %>% 
  pivot_wider(names_from = Scenario, values_from = Mean)

# report for main text
means %>% 
  mutate(CU = CU / PN,
         RW = RW / PN) %>% 
  group_by(Area) %>% 
  summarize(Min_CU = min(CU),
            Average_CU = mean(CU),
            Max_CU = max(CU),
            Min_RW = min(RW),
            Average_RW = mean(RW),
            Max_RW = max(RW))

# report for supplement
means %>% 
  mutate(CU = CU / PN,
         RW = RW / PN) %>% 
  group_by(Area, Size) %>% 
  summarize(Min_CU = min(CU),
            Average_CU = mean(CU),
            Max_CU = max(CU),
            Min_RW = min(RW),
            Average_RW = mean(RW),
            Max_RW = max(RW))
  
# interactions ---------------------------
raw_edge <- read_rds("../../Results/raw_edge_model.rds")
rand_edge <- read_rds("../../Results/random_edge_model.rds")
d <- bind_rows(
  mutate(raw_edge@frame, Area = "Protected"),
  mutate(rand_edge@frame, Area = "Random")
) %>% 
  as_tibble()

means <- d %>% 
  group_by(Area, Edge, Scenario) %>% 
  summarise(Mean = mean(Proportion)) %>% 
  pivot_wider(names_from = Scenario, values_from = Mean)

# report for main text
means %>% 
  mutate(CU = CU / PN,
         RW = RW / PN) %>% 
  group_by(Area) %>% 
  summarize(Min_CU = min(CU),
            Average_CU = mean(CU),
            Max_CU = max(CU),
            Min_RW = min(RW),
            Average_RW = mean(RW),
            Max_RW = max(RW))

# report for supplement
means %>% 
  mutate(CU = CU / PN,
         RW = RW / PN) %>% 
  group_by(Area, Edge) %>% 
  summarize(Average_CU = mean(CU),
            Average_RW = mean(RW)) %>% 
  filter(Area == "Random") %>% 
  arrange(Edge)

