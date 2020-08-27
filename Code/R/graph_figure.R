library(tidyverse)
library(igraph)
library(ggraph)
library(ggsci)
library(grid)
library(modelbased)


levels <- read_rds("../../Results/random_level_model.rds")
d <- levels@frame
v <- d %>% 
  group_by(Size, Scenario) %>% 
  summarize(CI_low = quantile(Proportion, 0.025),
            Mean = mean(Proportion),
            CI_high = quantile(Proportion, 0.975),
            N = round(mean(Proportion)))

edges <- read_rds("../../Results/random_edge_model.rds")
d <- edges@frame
e <- d %>% 
  group_by(Edge, Scenario) %>% 
  summarize(CI_low = quantile(Proportion, 0.025),
            Mean = mean(Proportion),
            CI_high = quantile(Proportion, 0.975),
            N = round(mean(Proportion))) %>% 
  mutate(
    From = map(Edge, function(x) {
      str_split(x, " ", simplify = TRUE)[1]
    }) %>% unlist(),
    To = map(Edge, function(x) {
      str_split(x, " ", simplify = TRUE)[3]
    }) %>% unlist()
  )

p <- list()
atr <- list()
prop <- list()
for (scen in unique(d$Scenario)) {
  pn <- graph(t(unique(cbind(e$From, e$To))), directed = TRUE) %>% 
    add.vertices(1, name = "Megaherbivore")
  V(pn)$Size <- sapply(names(V(pn)), function(x) {
    v %>% 
      filter(Scenario == scen,
             Size == x) %>% 
      pull(Mean)
  })
  atr[[scen]] <- list()
  atr[[scen]][["Size"]] <- V(pn)$Size
  E(pn)$Width <- sapply(1:nrow(get.edgelist(pn)), function(x) {
    e %>% 
      filter(Scenario == scen,
             From == get.edgelist(pn)[x, 1], 
             To == get.edgelist(pn)[x, 2]) %>% 
      pull(Mean)
  })
  atr[[scen]][["Width"]] <- E(pn)$Width
  
  layout <- create_layout(pn, "igraph", algorithm = "mds")
  layout[layout$name == "Megacarnivore", c("x", "y")] <- c(-0.33, 3)
  layout[layout$name == "Megaherbivore", c("x", "y")] <- c(0, 1)
  layout[layout$name == "Mesocarnivore", c("x", "y")] <- c(-0.5+0.33, 2)
  layout[layout$name == "Mesoherbivore", c("x", "y")] <- c(-0.33, 1)
  layout[layout$name == "Microcarnivore", c("x", "y")] <- c(-0.5, 2)
  layout[layout$name == "Microherbivore", c("x", "y")] <- c(-0.66, 1)
  
  V(pn)$Size <- atr[[scen]][["Size"]] / atr[["PN"]][["Size"]]
  E(pn)$Width <- atr[[scen]][["Width"]] / atr[["PN"]][["Width"]]
  
  prop[[scen]][["Size"]] <- atr[[scen]][["Size"]] / atr[["PN"]][["Size"]]
  prop[[scen]][["Width"]] <- atr[[scen]][["Width"]] / atr[["PN"]][["Width"]]
  
  p[[scen]] <- ggraph(layout) +
    geom_edge_link(aes(edge_width = 3 * E(pn)$Width)) +
    geom_node_point(aes(col = name, size = 10 * V(pn)$Size)) +
    scale_edge_width_continuous(breaks = seq(0, 1, length.out = 10)) +
    scale_color_startrek() +
    theme_void()
}

p$PN

cowplot::plot_grid(p$PN, p$CU, p$RW, ncol = 2)
prop

ggsave("../../Manuscript/Figures/graph_result.svg")

levels <- read_csv("../../Results/level_estimates.csv") %>% 
  transmute(Scenario, 
            Level = Size,
            Mean_n = N,
            CI_low_n = CI_low,
            CI_med_low_n = CI_med_low,
            CI_high_n = CI_high,
            CI_med_high_n = CI_med_high,
            Area)
edges <- read_csv("../../Results/edge_estimates.csv") %>% 
  mutate(Size = To)

edges

pn <- edges %>% 
  filter(Scenario == "PN")
pn_n <- levels %>% 
  filter(Scenario == "PN")

g <- edges %>%
  mutate(Level = Size) %>% 
  full_join(edges %>%
              mutate(Level = Size) %>% 
              group_by(Area, Edge) %>% 
              summarize(Max = max(Estimate))) %>% 
  group_by(Scenario, Area, Edge) %>% 
  mutate(Proportion = Estimate / Max) %>%
  ungroup() %>% 
  full_join(levels) %>% 
  select(From, To, Proportion, Mean_n, Scenario, Area) %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
    To = factor(To, levels = c(unique(edges$To), unique(edges$From)[c(2, 4)]))
  ) %>% 
  graph_from_data_frame(directed = TRUE)


ggraph(layout) +
  geom_edge_link(aes(edge_width = Proportion),
                 arrow = arrow(length = unit(0.5, "cm"), 
                               type = "closed")) +
  geom_node_point(aes(col = name), size = 16) +
  facet_grid(Area~Scenario) +
  scale_edge_width_continuous(breaks = seq(0, 1, length.out = 10)) +
  scale_color_startrek() +
  theme_void() +
  theme(text = element_text(size = 50))

ggsave("../../Manuscript/Figures/graph_figure.png", width = 30, height = 15)
ggsave("../../Manuscript/Figures/graph_figure.svg", width = 32, height = 16)

# to manual set the size
levels %>%
  group_by(Scenario, Level, Area) %>% 
  summarise(N = Mean_n) %>% 
  spread(key = "Scenario", value = "N") %>% 
  mutate(CU = CU/PN * 200,
         RW = RW/PN * 200) %>% 
  filter(Level == "Microherbivore")
