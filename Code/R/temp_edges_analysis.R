library(tidyverse)
library(sf)
library(ggExtra)
library(emmeans)
library(lme4)
library(cowplot)
library(easystats)

source("my_theme.R")
source("plot_model_control.R")

WDPA <- read_sf("../../Data/PA_5000km_bioregions.shp") %>%
  mutate(Area = as.numeric(st_area(geometry)) / 10^6) %>%
  filter(Area > 5000)

Random_WDPA <- read_sf("../../Data/Random_points.shp")

web_results <- list.files("../../Results/Webs/", pattern = "csv", full.names = TRUE)
random <- grep("random", web_results)
web_results[random]
edge <- web_results[grep("Graph", web_results)]
edge_random <- edge[grep("random", edge, invert = F)] #only random webs
edge <- edge[grep("random", edge, invert = T)] #remove random webs
trop_lev <- gsub('Graph_web', 'TL_web', edge)

Webs <- sapply(edge, function(x){
  substr(x, str_locate(x, "\\d+")[1], str_locate(x, "\\d+")[2])
}) %>% as.numeric()

Random <- sapply(edge_random, function(x){
  substr(x, str_locate(x, "\\d+")[1], str_locate(x, "\\d+")[2])
}) %>% as.numeric()

edges <- list()
for(i in 1:length(edge)){
  tl <- read_csv(trop_lev[i], col_types = cols()) %>% 
    mutate(k = modify(Size, function(x)
      switch(x, 'Megacarnivore' = 6, 'Mesocarnivore' = 5, 'Microcarnivore' = 4,
             'Megaherbivore' = 3, 'Mesoherbivore' = 2, 'Microherbivore' = 1)
      ) %>% unlist() %>% as.integer())
  ed <- read_csv(edge[i], col_types = cols()) %>% 
    mutate(
      nPN = modify(i, function(x)
        tl$PN[tl$k == x]),
      nCU = modify(i, function(x)
        tl$CU[tl$k == x]),
      nRW = modify(i, function(x)
        tl$RW[tl$k == x]),
      mPN = modify(j, function(x)
        tl$PN[tl$k == x]),
      mCU = modify(j, function(x)
        tl$CU[tl$k == x]),
      mRW = modify(j, function(x)
        tl$RW[tl$k == x])
    ) %>% 
    mutate(
      PN = PN / (nPN),
      CU = CU / (nCU),
      RW = RW / (nRW)
    )
  
  ed$PN[is.na(ed$PN)] <- NA
  ed$PN[is.infinite(ed$PN)] <- NA
  ed$CU[is.na(ed$CU)] <- NA
  ed$CU[is.infinite(ed$CU)] <- NA
  ed$RW[is.na(ed$RW)] <- NA
  ed$RW[is.infinite(ed$RW)] <- NA
  
  edges[[i]] <- ed
}
names(edges) <- Webs

random_edges <- list()
for(i in 1:length(Random)){
  random_edges[[i]] <- read_csv(edge_random[i], col_types = cols()) 
}
names(random_edges) <- Random

# Edges ----------------------------------------------------------
raw_edges <- do.call("rbind", edges) %>% 
  as_tibble() %>% 
  add_column(Web = rep(as.numeric(names(edges)), sapply(edges, function(x) nrow(x)))) %>% 
  mutate(
    Ecozone = map(Web, function(x) WDPA$Bioregion[x]) %>% unlist(),
    From = map(i, function(x){
      switch (x, "Microherbivore", "Mesoherbivore", "Megaherbivore", "Microcarnivore", "Mesocarnivore", "Megacarnivore")
    }) %>% unlist(),
    To = map(j, function(x){
      switch (x, "Microherbivore", "Mesoherbivore", "Megaherbivore", "Microcarnivore", "Mesocarnivore", "Megacarnivore")
    }) %>% unlist()
  )

edges_random <- do.call("rbind", random_edges) %>% 
  as_tibble() %>% 
  add_column(Web = rep(as.numeric(names(random_edges)), sapply(random_edges, function(x) nrow(x)))) %>% 
  mutate(
    Ecozone = map(Web, function(x) WDPA$Bioregion[x]) %>% unlist(),
    From = map(i, function(x){
      switch (x, "Microherbivore", "Mesoherbivore", "Megaherbivore", "Microcarnivore", "Mesocarnivore", "Megacarnivore")
    }) %>% unlist(),
    To = map(j, function(x){
      switch (x, "Microherbivore", "Mesoherbivore", "Megaherbivore", "Microcarnivore", "Mesocarnivore", "Megacarnivore")
    }) %>% unlist()
  )

# Protected areas Edges ----------------------------------------------------------------
dat <- raw_edges %>% #either raw_edges or edges_random
  filter(PN > 0) %>% 
  dplyr::select(PN, CU, RW, Web, Ecozone, From, To) %>% 
  gather(key = "Scenario", value = "Proportion", -From, -To, -Web, -Ecozone) %>% 
  filter(!grepl('herb', To)) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
    Ecozone = factor(Ecozone),
    Web = factor(as.character(Web)),
    Edge = factor(modify2(From, To, function(x, y) paste0(x, ' -> ', y)))
  )

dat$Proportion[is.na(dat$Proportion)] <- 0

model <- glmer(
  Proportion ~ 0 + Scenario * Edge + (1 | Ecozone) + (1 | Web),
  data = dat,
  family = 'binomial'
)

model <- glm(
  Proportion ~ 0 + Scenario * Edge,
  data = dat[dat$Proportion > 0, ],
  family = Gamma()
)

model <- glmer.nb(
  Proportion ~ -1 + Scenario * Edge + (1 | Ecozone) + (1 | Web),
  data = dat,
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10e6))
)

plot_control(model)

ggsave('../../Manuscript/Figures/E_model_control.pdf', plot_control(model), width = 8, height = 6)

blmeco::dispersion_glmer(model) #should not be over 1.4
check_singularity(model) #if TRUE there are problems in the model structure

edge_labeller <- function(var, val){
  size_name = list(
    level_names <- list(
      'Mesocarnivore->Megacarnivore' = 'Large carnivores \n \u2193 \n Megacarnivores',
      'Mesoherbivore->Megacarnivore' = 'Large herbivores \n \u2193 \n Megacarnivores',
      'Mesoherbivore->Megacarnivore' = 'Large Herbivores \n \u2193 \n Large carnivores',
      'Microcarnivore->Megacarnivore' = 'Small carvivores \n \u2193 \n Megacarnivores',
      'Microcarnivore->Mesocarnivore' = 'Small carvivores \n \u2193 \n Large carnivores',
      'Microherbivore->Megacarnivore' = 'Small herbivores \n \u2193 \n Megacarnivores',
      'Microherbivore->Mesocarnivore' = 'Small herbivores \n \u2193 \n Large carnivores',
      'Microherbivore->Microcarnivore' = 'Small herbivores \n \u2193 \n Small carnivores'
    )
  )
}

res <- estimate_means(model, fixed = 'Edge', transform = 'none') %>% 
  as_tibble() %>% 
  mutate(
    Estimate = exp(Mean),
    CI_low = exp(Mean - qnorm(0.975) * SE),
    CI_high = exp(Mean + qnorm(0.975) * SE)
  ) 

res %>% 
  ggplot() +
  geom_point(aes(Edge, Estimate, col = Scenario), position = position_dodge(width = 1)) +
  geom_errorbar(aes(Edge, Estimate, ymin = CI_low, ymax = CI_high, col = Scenario), 
                position = position_dodge(width = 1), width = 0.2) +
  theme(panel.grid.minor = element_line(colour = 'gainsboro')) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), alpha = 0.5) +
  scale_color_manual(labels = c('Present natural', 'Current', 'Rewilding'),
                     values = rev(wesanderson::wes_palette('Rushmore', 5))) +
  scale_x_discrete(labels = edge_labeller()) +
  scale_y_sqrt(breaks = c(1, 5, 10, 20, 35, 55, 80, 110, 145, 185, 230)) +
  xlab('Trophic level') +
  ylab('Difference in number of species') +
  ggtitle(expression('Protected areas \u2265 5,000 km'^2)) +
  monkey_theme +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    legend.position = c(0.90, 0.3),
    legend.background = element_rect(colour = 'black'),
    legend.key = element_blank()
  )

res <- estimate_contrasts(
  model, 
  fixed = 'Edge', 
  transform = 'none', 
  standardize = T,
  adjust = 'bonferroni'
) %>% 
  as_tibble() %>% 
  mutate(Contrast = map2(Level1, Level2, function(x, y) paste(x, y, sep = ' - ')) %>% unlist()) %>% 
  mutate(Contrast = factor(Contrast, levels = c('PN - CU', 'CU - RW', 'PN - RW')))

res %>% 
  mutate(D = Difference / SE) %>% 
  dplyr::select(Edge, Contrast, Std_Difference) %>% 
  spread(key = 'Contrast', value = 'Std_Difference') %>% 
  knitr::kable(digits = 2, align = c('l', 'c', 'c', 'c'), format = 'markdown')

res %>% 
  mutate(D = Difference / SE) %>% 
  dplyr::select(Edge, Contrast, p) %>% 
  spread(key = 'Contrast', value = 'p') %>% 
  knitr::kable(digits = 2, align = c('l', 'c', 'c', 'c'), format = 'markdown')

res %>% 
  mutate(D = Difference / SE) %>% 
  dplyr::select(Edge, Contrast, Difference) %>% 
  spread(key = 'Contrast', value = 'Difference') %>% 
  knitr::kable(digits = 2, align = c('l', 'c', 'c', 'c'), format = 'markdown')

res %>% 
  ggplot() +
  geom_point(aes(Edge, Difference, col = Contrast), position = position_dodge(width = 1)) +
  geom_errorbar(aes(Edge, Difference, ymin = CI_low, ymax = CI_high, col = Contrast), 
                position = position_dodge(width = 1), width = 0.2) +
  theme(panel.grid.minor = element_line(colour = 'gainsboro')) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_color_manual(labels = c('Present natural - current', 'Current - rewilding', 'Present natural - rewilding'),
                     values = rev(wesanderson::wes_palette('Rushmore', 5))) +
  scale_x_discrete(labels = edge_labeller()) +
  xlab('Trophic level') +
  ylab('Difference in number of links between trophic levels') +
  ggtitle(expression('Protected areas \u2265 5,000 km'^2)) +
  monkey_theme +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    legend.position = c(0.90, 0.9),
    legend.background = element_rect(colour = 'black'),
    legend.key = element_blank()
  )

plot_grid(p1 + 
            xlab('') +
            ylab('Number of species') +
            scale_y_sqrt(breaks = c(1, 5, 10, 20, 35, 50)) +
            scale_x_discrete(labels = c('LC \u2192 MC', 'LH \u2192 MC', 'LH \u2192 LC', 'SC \u2192 MC',
                                        'SC \u2192 LC', 'SH \u2192 MC', 'SH \u2192 LC', 'SH \u2192 SC')) +
            theme(legend.position = c(0.175, 0.85),
                  axis.text.y = element_text(size = 16),
                  axis.title = element_text(size = 16),
                  plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 12),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0, size = 16)), 
          p2 + 
            ggtitle(expression('Random areas \u2265 5,000 km'^2)) + 
            xlab('') +
            ylab('Number of species') +
            scale_y_sqrt(breaks = c(1, 5, 10, 20, 35, 50)) +
            scale_x_discrete(labels = c('LC \u2192 MC', 'LH \u2192 MC', 'LH \u2192 LC', 'SC \u2192 MC',
                                        'SC \u2192 LC', 'SH \u2192 MC', 'SH \u2192 LC', 'SH \u2192 SC')) +
            theme(legend.position = c(0.175, 0.85),
                  axis.text.y = element_text(size = 16),
                  axis.title = element_text(size = 16),
                  plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 12),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0, size = 16)), 
          ncol = 2)
ggsave("../../Manuscript/Figures/Edges.png", width = 12, height = 6)
