library(tidyverse)
library(sf)
library(ggExtra)
library(emmeans)
library(lme4)
library(cowplot)
library(easystats)

source("my_theme.R")
source("plot_model_control.R")

# rename_tl <- function(x){
#   y <- switch (x,
#                'Megacarnivore' = 'Megacarnivores \u2265 100 kg',
#                'Mesocarnivore' = 'Large carnivores 21.5 - 99 kg',
#                'Microcarnivore' = 'Small carnivores < 21.5 kg',
#                'Megaherbivore' = 'Megaherbivores \u2265 1,000 kg',
#                'Mesoherbivore' = 'Large herbivores 45 - 999 kg',
#                'Microherbivore' = 'Small herbivores < 45 kg'
#   )
#   return(y)
# }

rename_tl <- function(x){
  y <- switch (x,
               'Megacarnivore' = 'Megacarnivores',
               'Mesocarnivore' = 'Large carnivores',
               'Microcarnivore' = 'Small carnivores',
               'Megaherbivore' = 'Megaherbivores',
               'Mesoherbivore' = 'Large herbivores',
               'Microherbivore' = 'Small herbivores'
  )
  return(y)
}

rename_link <- function(x) {
  switch (x,
          'Mesocarnivore -> Megacarnivore' = 'Large carnivores \u2192 Megacarnivores',
          'Mesoherbivore -> Megacarnivore' = 'Large herbivores \u2192 Megacarnivores',
          'Mesoherbivore -> Megacarnivore' = 'Large Herbivores \u2192 Large carnivores',
          'Microcarnivore -> Megacarnivore' = 'Small carvivores \u2192 Megacarnivores',
          'Microcarnivore -> Mesocarnivore' = 'Small carvivores \u2192 Large carnivores',
          'Microherbivore -> Megacarnivore' = 'Small herbivores \u2192 Megacarnivores',
          'Microherbivore -> Mesocarnivore' = 'Small herbivores \u2192 Large carnivores',
          'Microherbivore -> Microcarnivore' = 'Small herbivores \u2192 Small carnivores'
  )
}
# 
# size_labeller <- function(var, val){
#   size_name = list(
#     level_names <- list(
#       "Megacarnivore" = "Megacarnivores\n (\u2265 100 kg)",
#       "Megaherbivore" = "Megaherbivores\n (\u2265 1,000 kg)",
#       "Mesocarnivore" = "Large carnivores\n (\u2208 [21.5, 100) kg)", 
#       "Mesoherbivore" = "Large herbivores\n (\u2208 [45, 1,000) kg)",
#       "Microcarnivore" = "Small carnivores\n (\u2264 21.5 kg)",
#       "Microherbivore" = "Small herbivores\n (\u2264 45 kg)"
#     )
#   )
# }
# edge_labeller <- function(var, val){
#   size_name = list(
#     level_names <- list(
#       'Mesocarnivore->Megacarnivore' = 'Large carnivores \n \u2193 \n Megacarnivores',
#       'Mesoherbivore->Megacarnivore' = 'Large herbivores \n \u2193 \n Megacarnivores',
#       'Mesoherbivore->Megacarnivore' = 'Large Herbivores \n \u2193 \n Large carnivores',
#       'Microcarnivore->Megacarnivore' = 'Small carvivores \n \u2193 \n Megacarnivores',
#       'Microcarnivore->Mesocarnivore' = 'Small carvivores \n \u2193 \n Large carnivores',
#       'Microherbivore->Megacarnivore' = 'Small herbivores \n \u2193 \n Megacarnivores',
#       'Microherbivore->Mesocarnivore' = 'Small herbivores \n \u2193 \n Large carnivores',
#       'Microherbivore->Microcarnivore' = 'Small herbivores \n \u2193 \n Small carnivores'
#     )
#   )
# }


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

Webs <- sapply(edge, function(x){
  substr(x, str_locate(x, "\\d+")[1], str_locate(x, "\\d+")[2])
}) %>% as.numeric()

Random <- sapply(edge_random, function(x){
  substr(x, str_locate(x, "\\d+")[1], str_locate(x, "\\d+")[2])
}) %>% as.numeric()

edges <- list()
for(i in 1:length(edge)){
  edges[[i]] <- read_csv(edge[i], col_types = cols()) 
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
dat1 <- raw_edges %>% #either raw_edges or edges_random
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

dat2 <- edges_random %>% #either raw_edges or edges_random
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

model <- glmer.nb(
  Proportion ~ 1 + Scenario * Edge + (1 | Ecozone) + (1 | Web),
  data = dat2,
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10e6))
)
# check variance without and with random effects
# Food web ID
boxplot(log(dat1$Proportion) ~ dat1$Web)
boxplot(resid(model, type = "pearson") ~ model@frame$Web)
# Biogeographic realm
boxplot(log(dat1$Proportion) ~ model@frame$Ecozone)
boxplot(resid(model, type = "pearson") ~ model@frame$Ecozone)
# check model assumptions
plot(model, pch = 20)
qqnorm(resid(model), pch = 20)
qqline(resid(model), pch = 20)
check_singularity(model) #if TRUE there are problems in the model structure
blmeco::dispersion_glmer(model) #should not be over 1.4
check_singularity(model) #if TRUE there are problems in the model structure
r2(model)

write_rds(model, "../../Results/random_edge_model.rds")

plot_control(model)

ggsave('../../Manuscript/Figures/E_model_control.pdf', plot_control(model), width = 8, height = 6)

blmeco::dispersion_glmer(model) #should not be over 1.4
check_singularity(model) #if TRUE there are problems in the model structure

raw <- read_rds('../../Results/raw_edge_model.rds')
dat <- raw@frame %>% as_tibble()
res1 <- estimate_means(raw, fixed = 'Edge', transform = 'none') %>% 
  as_tibble() %>% 
  mutate(
    Estimate = exp(Mean),
    CI_low = exp(Mean - qnorm(0.975) * SE),
    CI_high = exp(Mean + qnorm(0.975) * SE)
  ) %>% 
  mutate(
    From = map(Edge, function(x)
      str_split(x, ' -> ', simplify = TRUE)[1]) %>% unlist(),
    To = map(Edge, function(x)
      str_split(x, ' -> ', simplify = TRUE)[2]) %>% unlist()
  ) %>% 
  mutate(Edge = map2(To, From, function(x, y)
    paste0(rename_tl2(y), ' \u2192 ', rename_tl2(x))) %>% unlist())

random <- read_rds('../../Results/random_edge_model.rds')
dat <- random@frame %>% as_tibble()
res2 <- estimate_means(random, fixed = 'Edge', transform = 'none') %>% 
  as_tibble() %>% 
  mutate(
    Estimate = exp(Mean),
    CI_low = exp(Mean - qnorm(0.975) * SE),
    CI_high = exp(Mean + qnorm(0.975) * SE)
  ) %>% 
  mutate(
    From = map(Edge, function(x)
      str_split(x, ' -> ', simplify = TRUE)[1]) %>% unlist(),
    To = map(Edge, function(x)
      str_split(x, ' -> ', simplify = TRUE)[2]) %>% unlist()
  ) %>% 
  mutate(Edge = map2(To, From, function(x, y)
    paste0(rename_tl2(y), ' \u2192 ', rename_tl2(x))) %>% unlist())

res <- bind_rows(
  res1 %>% mutate(Area = "Protected area"),
  res2 %>% mutate(Area = "Random area")
)

write_csv(res, "../../Results/edge_estimates.csv")

# figure ----
res <- read_csv("../../Results/edge_estimates.csv")

res %>% 
  mutate(Scenario = factor(Scenario, levels = c('RW', 'CU', 'PN')),
         Area = map(Area, function(x) {
           switch (x,
                   "Random area" = "Random areas",
                   "Protected area" = "Protected areas"
           )
         }) %>% unlist()) %>% 
  mutate(
    Area = factor(Area, levels = c("Random areas",
                                   "Protected areas")),
    Edge = factor(Edge, levels = c("Large carnivores \u2192 Megacarnivores",
                                   "Large herbivores \u2192 Megacarnivores",
                                   "Small carnivores \u2192 Megacarnivores",
                                   "Small herbivores \u2192 Megacarnivores",
                                   "Large herbivores \u2192 Large carnivores",
                                   "Small carnivores \u2192 Large carnivores",
                                   "Small herbivores \u2192 Large carnivores",
                                   "Small herbivores \u2192 Small carnivores"))) %>%
  mutate(Scenario = map(Scenario, function(x) {
    switch (x,
            "RW" = "Rewilding",
            "CU" = "Current",
            "PN" = "No-extinction"
    )} %>% unlist())) %>% 
  mutate(Scenario = factor(Scenario, levels = c("Rewilding",
                                                "Current",
                                                "No-extinction"))) %>% 
  ggplot() +
  geom_errorbar(aes(Scenario, Estimate, 
                    ymin = CI_low, ymax = CI_high, 
                    col = Scenario, lty = Area), 
                position = position_dodge(width = 0.5), 
                size = 0.5, width = 0.3) +
  geom_point(aes(Scenario, Estimate, col = Scenario, shape = Area), 
             position = position_dodge(width = 0.5), fill = "white") +
  scale_color_manual(name = "Scenario",
                     labels = c('Rewilding', 'Current', 'No-extinction'), 
                     values = c("#0B775E", "#F2300F", "#35274A")) +
  scale_shape_manual(name = "", values = c(21, 20)) +
  scale_linetype_manual(name = "", values = c(2, 1)) +
  xlab('') +
  ylab('Number of interactions') +
  #ggtitle(expression('Protected areas \u2265 5,000 km'^2)) +
  ggtitle("") +
  monkey_theme +
  facet_wrap(Edge ~ ., scales = 'free_x', ncol = 2) +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.box = 'horizontal',
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold', size = 10),
    axis.text = element_text(size = 10)
  )

# ----
ggsave('../../Manuscript/Figures/result2.png', width = 8, height = 8)
ggsave('../../Manuscript/Figures/result2.svg', width = 8, height = 8)

estimate_contrasts(
  raw, 
  fixed = 'Edge', 
  transform = 'none', 
  standardize = T,
  adjust = 'bonferroni'
) %>% 
  as_tibble() %>% 
  mutate(Contrast = map2(Level1, Level2, function(x, y) paste(x, y, sep = ' - ')) %>% unlist()) %>% 
  mutate(Contrast = factor(Contrast, levels = c('PN - CU', 'CU - RW', 'PN - RW'))) %>% 
  select(Edge, p, Std_Difference)

estimate_contrasts(
  random, 
  fixed = 'Edge', 
  transform = 'none', 
  standardize = T,
  adjust = 'bonferroni'
) %>% 
  as_tibble() %>% 
  mutate(Contrast = map2(Level1, Level2, function(x, y) paste(x, y, sep = ' - ')) %>% unlist()) %>% 
  mutate(Contrast = factor(Contrast, levels = c('PN - CU', 'CU - RW', 'PN - RW'))) %>% 
  select(Edge, p, Std_Difference) %>% 
  arrange(desc(Std_Difference))



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

### Final figures -----
##raw numbers
res %>% 
  mutate(Size = as.vector(Size)) %>% 
  mutate(Size = map(Size, function(x) rename_tl(x))) %>% 
  unnest(cols = c(Size)) %>% 
  mutate(Size = factor(Size, levels = c('Megacarnivores', 'Large carnivores', 'Small carnivores', 'Megaherbivores', 'Large herbivores', 'Small herbivores'))) %>% 
  mutate(Scenario = factor(Scenario, levels = c('RW', 'CU', 'PN'))) %>% 
  ggplot() +
  geom_point(aes(Scenario, N, col = Scenario), position = position_dodge(width = 1)) +
  geom_errorbar(aes(Scenario, N, ymin = CI_low, ymax = CI_high, col = Scenario), position = position_dodge(width = 1), width = 0.2) +
  theme(panel.grid.minor = element_line(colour = 'gainsboro')) +
  #geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), alpha = 0.5) +
  scale_color_manual(labels = c('Present natural', 'Current', 'Rewilding'), values = rev(wesanderson::wes_palette('Rushmore', 5))) +
  #scale_x_discrete(labels = size_labeller()) +
  scale_y_sqrt() +
  xlab('Scenario') +
  ylab('Number of species') +
  ggtitle(expression('Protected areas \u2265 5,000 km'^2)) +
  monkey_theme +
  facet_wrap(Size ~ ., scales = 'free', ncol = 1) +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    legend.position = 'none',
    legend.background = element_rect(colour = 'black'),
    legend.key = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold')
  ) -> p2

plot_grid(p1 + coord_flip() + scale_color_manual(labels = c('Present natural', 'Current', 'Rewilding'), values = c("#0B775E", "#F2300F", "#35274A")), 
          p2 + ggtitle(expression('Random areas \u2265 5,000 km'^2)) + coord_flip() + scale_color_manual(labels = c('Present natural', 'Current', 'Rewilding'), values = c("#0B775E", "#F2300F", "#35274A")))

ggsave('../../Manuscript/Figures/result1.png', width = 8, height = 8)


# Maps --------------------------------------------------------------------
Pal <- rev(wesanderson::wes_palette('Zissou1', 8, 'continuous'))

dat %>% 
  mutate(
    From = map(Edge, function(x)
      str_split(x, ' -> ', simplify = TRUE)[1]) %>% unlist(),
    To = map(Edge, function(x)
      str_split(x, ' -> ', simplify = TRUE)[2]) %>% unlist()
  ) %>% 
  mutate(
    Edge = map2(From, To, function(x, y)
      paste0(rename_tl(x), ' \u2192 ', rename_tl(y))) %>% unlist(),
    Ecozone = map(as.character(Ecozone), function(x){
      switch (x,
              'AA' = 'Australasia',
              'AT' = 'Afrotropic',
              'IM' = 'Indomalaya',
              'NA' = 'Neartic',
              'NT' = 'Neotropic',
              'PA' = 'Paleartic'
      )
    }) %>% unlist() %>% as.factor()
  ) %>% 
  ggplot() +
  geom_smooth(aes(Scenario, Proportion, group = Edge, col = Edge), alpha = 0.2, method = 'loess') +
  geom_boxplot(aes(Scenario, Proportion, fill = Edge), alpha = 0.7, outlier.shape = NA) +
  facet_wrap(~Ecozone, scales = 'free') +
  scale_x_discrete(labels = c('Present natural', 'Current', 'Rewilding')) +
  scale_y_sqrt(breaks = c(3, 12, 25, 42, 63, 88, 117, 150, 187) - 2) +
  xlab('Trophic level') +
  ylab('Number of species') +
  scale_fill_manual(values = Pal) +
  scale_color_manual(values = Pal) +
  monkey_theme 

