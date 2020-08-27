library(tidyverse)
library(sf)
library(ggExtra)
library(emmeans)
library(easystats)
library(lme4)
library(cowplot)
library(modelbased)

source("my_theme.R")

rename_tl <- function(x){
  y <- switch (x,
               'Megacarnivore' = 'Megacarnivores \u2265 100 kg',
               'Mesocarnivore' = 'Large carnivores 21.5 - 99 kg',
               'Microcarnivore' = 'Small carnivores < 21.5 kg',
               'Megaherbivore' = 'Megaherbivores \u2265 1,000 kg',
               'Mesoherbivore' = 'Large herbivores 45 - 999 kg',
               'Microherbivore' = 'Small herbivores < 45 kg'
  )
  return(y)
}

rename_tl2 <- function(x){
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

size_labeller <- function(var, val){
  size_name = list(
    level_names <- list(
      "Megacarnivore" = "Megacarnivores\n (\u2265 100 kg)",
      "Megaherbivore" = "Megaherbivores\n (\u2265 1,000 kg)",
      "Mesocarnivore" = "Large carnivores\n (\u2208 [21.5, 100) kg)", 
      "Mesoherbivore" = "Large herbivores\n (\u2208 [45, 1,000) kg)",
      "Microcarnivore" = "Small carnivores\n (\u2264 21.5 kg)",
      "Microherbivore" = "Small herbivores\n (\u2264 45 kg)"
    )
  )
}

#rstan_options(auto_write = TRUE)

WDPA <- read_sf("../../Data/PA_5000km_bioregions.shp") %>%
  mutate(Area = as.numeric(st_area(geometry)) / 10^6) %>%
  filter(Area > 5000)

Random_WDPA <- read_sf("../../Data/Random_points.shp")

web_results <- list.files("../../Results/Webs/", pattern = "csv", full.names = TRUE)
random <- grep("random", web_results)
web_results[random]
TL <- web_results[grep("TL", web_results)]
TL_random <- TL[grep("random", TL, invert = F)] #only random webs
TL <- TL[grep("random", TL, invert = T)] #remove random webs

Webs <- sapply(TL, function(x){
  substr(x, str_locate(x, "\\d+")[1], str_locate(x, "\\d+")[2])
}) %>% as.numeric()

Random <- sapply(TL_random, function(x){
  substr(x, str_locate(x, "\\d+")[1], str_locate(x, "\\d+")[2])
}) %>% as.numeric()

trophic_levels <- list()
for(i in 1:length(TL)){
  trophic_levels[[i]] <- read_csv(TL[i], col_types = cols()) 
}
names(trophic_levels) <- Webs

random_levels <- list()
for(i in 1:length(Random)){
  random_levels[[i]] <- read_csv(TL_random[i], col_types = cols()) 
}
names(random_levels) <- Random

# Trophic levels ----------------------------------------------------------
raw_levels <- do.call("rbind", trophic_levels) %>% 
  as_tibble() %>% 
  add_column(Web = rep(Webs, sapply(trophic_levels, function(x) nrow(x)))) %>% 
  mutate(Ecozone = map(Web, function(x) WDPA$Bioregion[x]) %>% unlist())

random_levels <- do.call("rbind", random_levels) %>% 
  as_tibble() %>% 
  add_column(Web = rep(Random, sapply(random_levels, function(x) nrow(x)))) %>% 
  mutate(Ecozone = map(Web, function(x) WDPA$Bioregion[x]) %>% unlist())

# Protected areas TL ----------------------------------------------------------------
dat1 <- raw_levels %>% #either raw_levels or random_levels 
  gather(key = "Scenario", value = "Proportion", -Size, -Web, -Ecozone) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
    Ecozone = factor(Ecozone),
    Web = factor(as.character(Web))
  )

dat2 <- random_levels %>% #either raw_levels or random_levels 
  gather(key = "Scenario", value = "Proportion", -Size, -Web, -Ecozone) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
    Ecozone = factor(Ecozone),
    Web = factor(as.character(Web))
  )

dat <- bind_rows(
  dat1 %>% mutate(Area = "Protected"),
  dat2 %>% mutate(Area = "Random")
)

Pal <- c("#F21A00", "#78B7C5",
         "#E1AF00", "#3B9AB2",
         "#EBCC2A", "#00637c")

m <- glmer.nb(
  Proportion ~ 1 + Scenario * Size + Area + (1 | Ecozone) + (1 | Web),
  data = dat,
  control = glmerControl(optimizer = "bobyqa")
)

r2(m)

# check variance without and with random effects
# Area
boxplot(log(dat$Proportion) ~ dat$Area)
boxplot(resid(m ,type = "pearson") ~ dat$Area)
# Food web ID
boxplot(log(dat$Proportion) ~ dat$Web)
boxplot(resid(m, type = "pearson") ~ m@frame$Web)
# Biogeographic realm
boxplot(log(dat$Proportion) ~ m1@frame$Ecozone)
boxplot(resid(m, type = "pearson") ~ m@frame$Ecozone)
# check model assumptions
plot(m1, pch = 20)
qqnorm(resid(m), pch = 20)
qqline(resid(m), pch = 20)
check_overdispersion(m)
check_singularity(m) #if TRUE there are problems in the model structure

res <- estimate_means(m, fixed = 'Size', transform = 'none') %>% 
  as_tibble() %>% 
  mutate(
    N = exp(Mean),
    CI_low = exp(Mean - qnorm(0.975) * SE),
    CI_high = exp(Mean + qnorm(0.975) * SE),
    CI_med_low = exp(Mean - qnorm(0.8750) * SE),
    CI_med_high = exp(Mean + qnorm(0.8750) * SE)
  )

res %>% 
  mutate(Size = as.vector(Size),
         Area = factor(Area, levels = c("Random",
                                        "Protected"))) %>% 
  mutate(Size = map(Size, function(x) rename_tl2(x))) %>% 
  unnest(cols = c(Size)) %>% 
  mutate(Size = factor(Size, levels = c('Megacarnivores', 
                                        'Megaherbivores', 
                                        'Large carnivores', 
                                        'Large herbivores', 
                                        'Small carnivores', 
                                        'Small herbivores'))) %>% 
  mutate(Scenario = factor(Scenario, levels = c('RW', 'CU', 'PN'))) %>% 
  ggplot() +
  geom_errorbar(aes(Scenario, N, 
                    ymin = CI_low, ymax = CI_high, 
                    col = Scenario, lty = Area), size = 0.5,
                position = position_dodge(width = 0.5), width = 0.3) +
  geom_point(aes(Scenario, N, col = Scenario, shape = Area, size = Area), 
             position = position_dodge(width = 0.5), fill = "white") +
  theme(panel.grid.minor = element_line(colour = 'gainsboro')) +
  scale_color_manual(name = "Scenario",
                     labels = c('Rewilding', 'Current', 'Present-natural'), 
                     values = c("#0B775E", "#F2300F", "#35274A")) +
  scale_shape_manual(name = "Area", values = c(21, 20)) +
  scale_linetype_manual(name = "Area", values = c(2, 1)) +
  scale_size_manual(values = c(1.5, 2)) +
  xlab('Scenario') +
  ylab('Number of species') +
  monkey_theme +
  facet_wrap(~Size, scales = 'free', ncol = 2) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.background = element_rect(colour = "black"),
    legend.key = element_blank(),
    legend.box = 'horizontal',
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold')
  ) +
  coord_flip()

estimate_contrasts(
  m, 
  fixed = "Size",
  levels = "Scenario",
  transform = "none", 
  standardize = TRUE,
  adjust = "bonferroni"
) %>% 
  as_tibble() %>% 
  select(Level1, Level2, Size, Difference, CI_low, CI_high, p)
  
estimate_contrasts(
  m, 
  fixed = "Size",
  levels = "Area",
  transform = "none", 
  standardize = TRUE,
  adjust = "bonferroni"
) %>% 
  as_tibble() %>% 
  select(Level1, Level2, Size, Difference, CI_low, CI_high, p)
  
# split the model ----
m1 <- glmer(
    Proportion ~ 1 + Scenario * Size + (1 | Ecozone) + (1 | Web),
    data = dat1,
    family = 'poisson',
    control = glmerControl(optimizer = "bobyqa")
  )

# check variance without and with random effects
# Food web ID
boxplot(log(dat1$Proportion) ~ dat1$Web)
boxplot(resid(m1, type = "pearson") ~ m1@frame$Web)
# Biogeographic realm
boxplot(log(dat1$Proportion) ~ m1@frame$Ecozone)
boxplot(resid(m1, type = "pearson") ~ m1@frame$Ecozone)
# check model assumptions
plot(m1, pch = 20)
qqnorm(resid(m1), pch = 20)
qqline(resid(m1), pch = 20)
check_singularity(m1) #if TRUE there are problems in the model structure
blmeco::dispersion_glmer(m1) #should not be over 1.4
check_singularity(m1) #if TRUE there are problems in the model structure
r2(m1)

m2 <- glmer(
  Proportion ~ 1 + Scenario * Size + (1 | Ecozone) + (1 | Web),
  data = dat2,
  family = 'poisson',
  control = glmerControl(optimizer = "bobyqa")
)
# check variance without and with random effects
# Food web ID
boxplot(log(dat2$Proportion) ~ dat2$Web)
boxplot(resid(m2, type = "pearson") ~ m2@frame$Web)
# Biogeographic realm
boxplot(log(dat2$Proportion) ~ m2@frame$Ecozone)
boxplot(resid(m2, type = "pearson") ~ m2@frame$Ecozone)
# check model assumptions
plot(m2, pch = 20)
qqnorm(resid(m2), pch = 20)
qqline(resid(m2), pch = 20)
check_singularity(m2) #if TRUE there are problems in the model structure
blmeco::dispersion_glmer(m2) #should not be over 1.4
check_singularity(m2) #if TRUE there are problems in the model structure
r2(m2)


res <- bind_rows(
  estimate_means(m1, fixed = 'Size', transform = 'none') %>% 
    as_tibble() %>% 
    mutate(
      N = exp(Mean),
      CI_low = exp(Mean - qnorm(0.975) * SE),
      CI_high = exp(Mean + qnorm(0.975) * SE),
      CI_med_low = exp(Mean - qnorm(0.8750) * SE),
      CI_med_high = exp(Mean + qnorm(0.8750) * SE),
      Area = "Protected areas"
    ),
  estimate_means(m2, fixed = 'Size', transform = 'none') %>% 
    as_tibble() %>% 
    mutate(
      N = exp(Mean),
      CI_low = exp(Mean - qnorm(0.975) * SE),
      CI_high = exp(Mean + qnorm(0.975) * SE),
      CI_med_low = exp(Mean - qnorm(0.8750) * SE),
      CI_med_high = exp(Mean + qnorm(0.8750) * SE),
      Area = "Random areas"
    ) 
)

write_csv(res, "../../Results/level_estimates.csv")

estimate_contrasts(
  m1, 
  fixed = 'Size', 
  transform = 'none', 
  standardize = TRUE,
  adjust = 'bonferroni'
) %>% 
  as_tibble()

estimate_contrasts(
  m2, 
  fixed = 'Size', 
  transform = 'none', 
  standardize = TRUE,
  adjust = 'bonferroni'
) %>% 
  as_tibble()

##raw numbers ----
res <- read_csv("../../Results/level_estimates.csv") %>% 
  mutate(Area = map(Area, function(x) {
    switch (x,
            "Random area" = "Random areas",
            "Protected area" =  "Protected areas"
    )
  }) %>% unlist())

res %>% 
  mutate(Size = as.vector(Size),
         Area = factor(Area, levels = c("Random areas",
                                        "Protected areas"))) %>% 
  mutate(Size = map(Size, function(x) rename_tl2(x))) %>% 
  unnest(cols = c(Size)) %>% 
  mutate(Size = factor(Size, levels = c('Megacarnivores', 
                                        'Megaherbivores', 
                                        'Large carnivores', 
                                        'Large herbivores', 
                                        'Small carnivores', 
                                        'Small herbivores'))) %>% 
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
  geom_errorbar(aes(Scenario, N, 
                    ymin = CI_low, ymax = CI_high, 
                    col = Scenario, lty = Area), size = 0.5,
                position = position_dodge(width = 0.5), 
                width = 0.3, show.legend = TRUE) +
  geom_point(aes(Scenario, N, col = Scenario, shape = Area, size = Area), 
             position = position_dodge(width = 0.5), fill = "white",
             show.legend = TRUE) +
  theme(panel.grid.minor = element_line(colour = 'gainsboro')) +
  scale_color_manual(name = "Scenario",
                     labels = c('Rewilding', 'Current', 'No-extinction'), 
                     values = c("#0B775E", "#F2300F", "#35274A")) +
  scale_shape_manual(name = "Area", values = c(21, 20)) +
  scale_linetype_manual(name = "Area", values = c(2, 1)) +
  scale_size_manual(values = c(1.5, 2)) +
  #scale_x_discrete(labels = size_labeller()) +
  xlab('') +
  ylab('Number of species') +
  monkey_theme +
  facet_wrap(~Size, scales = 'free_x', ncol = 2) + 
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
  ) +
  coord_flip()

# ------
ggsave('../../Manuscript/Figures/result1.png', width = 8, height = 6)
ggsave('../../Manuscript/Figures/result1.svg', width = 8, height = 6)

res %>% 
  select(Scenario, Size, CI_low, N, CI_high, Area) %>% 
  print(n = 60)
