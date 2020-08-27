library(tidyverse)
library(sf)
library(ggExtra)
library(rstan)
library(rstanarm)
library(shinystan)
library(emmeans)
library(easystats)
library(bayesplot)
library(lme4)
library(cowplot)

source("my_theme.R")

rstan_options(auto_write = TRUE)

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
dat <- raw_levels %>% 
  gather(key = "Scenario", value = "Proportion", -Size, -Web, -Ecozone) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
    Ecozone = factor(Ecozone),
    Web = factor(as.character(Web))
  )

model <- glmer(
  Proportion ~ 0 + Scenario : Size + (1 | Ecozone) + (1 | Web), 
  data = dat, 
  family = "poisson", 
  control = glmerControl(optimizer = "bobyqa")
)

plot(model, pch = 20)
qqnorm(residuals(model))
qqline(residuals(model))

check_singularity(model) #if TRUE there are problems in the model structure

summary(model)

contrasts_pa <- estimate_contrasts(model, levels = ~Scenario : Size) %>% 
  as_tibble() %>% 
  mutate(Conserve = map2(Level1, Level2, function(x, y){
    if(str_split(x, "-", simplify = T)[2] == str_split(y, "-", simplify = T)[2]){
      TRUE
    } else{FALSE}
  }) %>% unlist()) %>% 
  filter(Conserve == T) %>% 
  select(Level1, Level2, Std_Difference) %>% 
  mutate(Effect = map(Std_Difference, function(x){
    x <- abs(x)
    if(x < 0.20){
      "None"
    } else if(x < 0.50){
      "Small"
    } else if(x < 0.80){
      "Large"
    } else if(x < 1.20){
      "Very large"
    } else{
      "Huge"
    }
  }) %>% unlist()) %>% 
  mutate(
    Size = modify(Level1, function(x)
      str_split(x, " - ", simplify = T)[2]),
    Scenario1 = modify(Level1, function(x)
      str_split(x, " - ", simplify = T)[1]),
    Scenario2 = modify(Level2, function(x)
      str_split(x, " - ", simplify = T)[1])
  ) %>% 
  select(Scenario1, Scenario2, Std_Difference, Effect, Size)

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

p1 <- estimate_means(model) %>% 
  as_tibble() %>% 
  ggplot() +
  geom_point(aes(Size, rate, col = Scenario), 
             position = position_dodge(w = 0.5)) +
  geom_errorbar(aes(Size, col = Scenario,
                    ymin = rate - qnorm(0.975) * SE, 
                    ymax = rate + qnorm(0.975) * SE),
                position = position_dodge(w = 0.5), width = 0) +
  scale_y_sqrt(breaks = c(1:10, 15, 20, 25, 30, 35, 40),
               limits = c(0, 40), expand = c(0, 0)) +
  scale_x_discrete(labels = size_labeller) +
  xlab("Trophic level") +
  ylab("Number of species") +
  monkey_theme +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_vline(xintercept = seq(1.5, 5.5, 1), size = 0.1) +
  scale_color_manual(labels = c("Present-natural", "Current", "Rewilding"),
                     values = rev(wesanderson::wes_palette("Rushmore", 5))) +
  ggtitle(expression("Number of species per trophic level in protected areas \u2265 5,000 km"^2))

ggsave(p1, "../../Manuscript/Figures/TL.png", width = 10, height = 6)

# Random points TL --------------------------------------------------------
dat <- random_levels %>% 
  gather(key = "Scenario", value = "Proportion", -Size, -Web, -Ecozone) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
    Ecozone = factor(Ecozone),
    Web = factor(as.character(Web))
  )

model <- glmer(
  Proportion ~ 0 + Scenario : Size + (1 | Ecozone) + (1 | Web), 
  data = dat, 
  family = "poisson", 
  control = glmerControl(optimizer = "bobyqa")
)

plot(model, pch = 20)
qqnorm(residuals(model))
qqline(residuals(model))

check_singularity(model) #if TRUE there are problems in the model structure

summary(model)

contrasts_random <- estimate_contrasts(model, levels = ~Scenario : Size) %>% 
  as_tibble() %>% 
  mutate(Conserve = map2(Level1, Level2, function(x, y){
    if(str_split(x, "-", simplify = T)[2] == str_split(y, "-", simplify = T)[2]){
      TRUE
    } else{FALSE}
  }) %>% unlist()) %>% 
  filter(Conserve == T) %>% 
  select(Level1, Level2, Std_Difference) %>% 
  mutate(Effect = map(Std_Difference, function(x){
    x <- abs(x)
    if(x < 0.20){
      "None"
    } else if(x < 0.50){
      "Small"
    } else if(x < 0.80){
      "Large"
    } else if(x < 1.20){
      "Very large"
    } else{
      "Huge"
    }
  }) %>% unlist()) %>% 
  mutate(
    Size = modify(Level1, function(x)
      str_split(x, " - ", simplify = T)[2]),
    Scenario1 = modify(Level1, function(x)
      str_split(x, " - ", simplify = T)[1]),
    Scenario2 = modify(Level2, function(x)
      str_split(x, " - ", simplify = T)[1])
  ) %>% 
  select(Scenario1, Scenario2, Std_Difference, Effect, Size)

p2 <- estimate_means(model) %>% 
  as_tibble() %>% 
  ggplot() +
  geom_point(aes(Size, rate, col = Scenario), 
             position = position_dodge(w = 0.5)) +
  geom_errorbar(aes(Size, col = Scenario,
                    ymin = rate - qnorm(0.975) * SE, 
                    ymax = rate + qnorm(0.975) * SE),
                position = position_dodge(w = 0.5), width = 0) +
  scale_y_sqrt(breaks = c(1:10, 15, 20, 25, 30, 35, 40), 
               limits = c(0, 40), expand = c(0, 0)) +
  scale_x_discrete(labels = size_labeller) +
  xlab("Trophic level") +
  ylab("Number of species") +
  monkey_theme +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_vline(xintercept = seq(1.5, 5.5, 1), size = 0.1) +
  scale_color_manual(labels = c("Present-natural", "Current", "Rewilding"),
                     values = rev(wesanderson::wes_palette("Rushmore", 5))) +
  ggtitle(expression("Number of species per trophic level in random areas \u2265 5,000 km"^2))

ggsave(p2, "../../Manuscript/Figures/TL_random.png", width = 10, height = 6)

# comparison --------------------------------------------------------------------
comparison <- full_join(
  contrasts_pa, contrasts_random, 
  by = c('Size', 'Scenario1', 'Scenario2'), 
  suffix = c("Protected", "Random")
) %>% 
  select(Scenario1, Scenario2, Size, EffectProtected, EffectRandom)

comparison

plot_grid(p1 + theme(axis.text.x = element_text(size = 8)), 
          p2 + theme(axis.text.x = element_text(size = 8)),
          ncol = 2)
ggsave("../../Manuscript/Figures/trophic_levels.png", width = 18, height = 8, dpi = 600)


# Ecozones plots ----------------------------------------------------------
ecozone_labeller <- function(var, val){
  eco_name = list(
    eco_name <- list(
      "AA" = "Australasia",
      "AT" = "Afrotropic",
      "IM" = "Indomalaya", 
      "NA" = "Neartic",
      "NT" = "Neotropic",
      "PA" = "Paleartic"
    )
  )
}

size_labeller <- function(var, val){
  size_name = list(
    level_names <- list(
      "Megacarnivore" = "Megacarnivores",
      "Megaherbivore" = "Megaherbivores",
      "Mesocarnivore" = "Large carnivores", 
      "Mesoherbivore" = "Large herbivores",
      "Microcarnivore" = "Small carnivores",
      "Microherbivore" = "Small herbivores"
    )
  )
}

raw_levels %>% 
  gather(key = "Scenario", value = "N", -Size, -Web, -Ecozone) %>% 
  ggplot() +
  geom_boxplot(aes(Size, N, fill = Scenario)) +
  facet_wrap(Ecozone ~., ncol = 1, labeller = ecozone_labeller) +
  scale_y_sqrt(breaks = c(1:10, 15, 20, 25, 30, 35, 40), 
               limits = c(0, 40), expand = c(0, 0)) +
  scale_x_discrete(labels = size_labeller) +
  monkey_theme +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_vline(xintercept = seq(1.5, 5.5, 1), size = 0.1) +
  coord_flip()

# 
# 
# 
# fit <- stan_glmer(
#   Proportion ~ Size : Scenario + (1 | Ecozone) + (1 | Web),
#   data = dat,
#   prior = normal(0, 2.5), 
#   prior_intercept = normal(0, 5),
#   family = neg_binomial_2,
#   adapt_delta = 0.999,
#   iter = 4000,
#   cores = 6
# )
# 
# write_rds(fit, "../../Results/Bayesian_TL.rds")
# #write_rds(fit, "../../Results/Bayesian_TL_random.rds")
# 
# TL <- read_rds("../../Results/Bayesian_TL.rds")
# 
# plot(TL, regex_pars = "Microcarn")
# 
# a <- mcmc_intervals(as.array(TL), regex_pars = "Scenario")
# a$data %>% 
#   mutate(
#     Scenario = map(parameter, function(x)
#       sub("Scenario", "", str_split(x, ":", simplify = T)[2])
#     ) %>% unlist() %>% as.factor(),
#     Class = map(parameter, function(x)
#       sub("Size", "", str_split(x, ":", simplify = T)[1])
#     ) %>% unlist() %>% as.factor()
#   ) %>% 
#   mutate(
#     Scenario = factor(Scenario, levels = c("PN", "CU", "RW"))
#   ) %>% 
#   add_row(Class = "Microherbivore", Scenario = "RW", m = 0, 
#           ll = -0.02 * 2, l = -0.02, hh = 0.02 * 2, h = 0.02) %>% 
#   ggplot() +
#   #geom_boxplot(aes(x = Class, middle = m, lower = l, upper = h, ymin = ll, ymax = hh, col = Scenario), stat = "identity")
#   geom_boxplot(aes(x = Class, middle = exp(m + TL$coefficients[1]), 
#                    lower = exp(l + TL$coefficients[1]), 
#                    upper = exp(h + TL$coefficients[1]), 
#                    ymin = exp(ll + TL$coefficients[1]), 
#                    ymax = exp(hh + TL$coefficients[1]), 
#                    col = Scenario), stat = "identity") +
#   scale_x_discrete(expand = c(0, 0.5), 
#                    labels = c("Megacarnivores\n (\u2265 100 kg)", 
#                               "Megaherbivores\n (\u2265 1,000 kg)",
#                               "Large carnivores\n (\u2208 [21.5, 100) kg)", 
#                               "Small carnivores\n (\u2264 21.5 kg)",
#                               "Herbivores\n (\u2264 1,000 kg)")) +
#   scale_y_log10() +
#   scale_color_manual(labels = c("Present-natural", "Current", "Rewilding"), 
#                      values = rev(wesanderson::wes_palette("FantasticFox1", 3))) +
#   monkey_theme +
#   ylab("Number of species") +
#   xlab("Trophic level") +
#   geom_vline(xintercept = 1.5, size = 0.2) +
#   geom_vline(xintercept = 2.5, size = 0.2) +
#   geom_vline(xintercept = 3.5, size = 0.2) +
#   geom_vline(xintercept = 4.5, size = 0.2) +
#   theme(legend.position = "bottom")
# 
# ggsave("../../Manuscript/Figures/Bayes_TL.png", width = 8, height = 6)
# 
# size_labeller <- function(var, val){
#   size_name = list(
#     hospital_names <- list(
#       "Megacarnivore" = "Megacarnivores\n (\u2265 100 kg)",
#       "Megaherbivore" = "Megaherbivores\n (\u2265 1,000 kg)",
#       "Mesocarnivore" = "Large carnivores\n (\u2208 [21.5, 100) kg)", 
#       "Microcarnivore" = "Small carnivores\n (\u2264 21.5 kg)",
#       "Microherbivore" = "Herbivores\n (\u2264 1,000 kg)"
#     )
#   )
# }
# 
# size_labeller2 <- function(var, val){
#   size_name = list(
#     hospital_names <- list(
#       "Microherbivore" = "SH",
#       "Megaherbivore" = "MH",
#       "Microcarnivore" = "SC",
#       "Mesocarnivore" = "LC",
#       "Megacarnivore" = "MC"
#     )
#   )
# }
# 
# dat %>% 
#   mutate(Size = factor(Size, levels = c("Microherbivore", "Megaherbivore", "Microcarnivore", "Mesocarnivore", "Megacarnivore"))) %>% 
#   ggplot() +
#   geom_boxplot(aes(Size, Proportion, fill = Scenario), show.legend = F, outlier.shape = NA) +
#   facet_wrap(Ecozone ~., ncol = 2, scales = "free") +
#   scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 5)) +
#   monkey_theme + 
#   xlab("Scenario") + 
#   ylab("Number of species") +
#   scale_y_sqrt() +
#   theme(axis.text = element_text(angle = -90)) +
#   # theme(strip.background = element_blank(),
#   #       strip.text.x = element_blank()) +
#   scale_x_discrete(labels = size_labeller2()) +
#   coord_flip()
# 
# ggsave("../../Manuscript/Figures/TL.png", width = 8, height = 8)
# 
# dat %>% 
#   group_by(Size, Scenario) %>% 
#   summarize(mu = round(mean(Proportion), 2)) %>% 
#   spread(key = Size, value = mu) %>% 
#   skimr::kable()
# 
# dat %>% 
#   group_by(Size, Scenario) %>% 
#   summarize(std = round(sd(Proportion), 2)) %>% 
#   spread(key = Size, value = std) %>% 
#   skimr::kable()
# 
# contrasts <- a$data %>% 
#   dplyr::select(parameter, ll, hh) %>% 
#   mutate(
#     Size = map(parameter, function(x)
#       sub("Size", "", str_split(x, ":", simplify = T)[1])
#     ) %>% unlist(),
#     Scenario = map(parameter, function(x)
#       sub("Scenario", "", str_split(x, ":", simplify = T)[2])
#     ) %>% unlist()
#   ) %>% 
#   dplyr::select(Size, Scenario, ll, hh) 
# 
# contrasts %>% 
#   filter(Size == "Microcarnivore")
# 
# #write_rds(fit, "../../Results/Bayesian_TL.rds")
# 
# pp_check(fit)
# 
# shinystan::launch_shinystan(fit)
# 
# plot(fit, regex_pars = "Scenario")
# 
# bay <- describe_posterior(
#   pairs(emmeans(fit, ~ Scenario:Size)),
#   estimate = "median", dispersion = TRUE,
#   ci = .9, ci_method = "hdi",
#   test = c("bayesfactor"),
#   bf_prior = fit
# )
# 
# plot(equivalence_test(fit))
# 
# interpret_bf(bay$BF)
# 
# 
# # Rest --------------------------------------------------------------------
# p <- raw_levels %>% 
#   mutate(
#     CU = CU / PN,
#     RW = RW / PN
#   ) %>% 
#   filter(
#     Size == "Megacarnivore"
#   ) %>% 
#   ggplot() + 
#   geom_point(aes(CU, RW)) +
#   xlab("Current") +
#   ylab("Rewilding") +
#   scale_x_continuous(limits = c(0, 1)) +
#   scale_y_continuous(limits = c(0, 1)) +
#   theme(
#     panel.background = element_blank(),
#     panel.grid.major = element_line(colour = "gainsboro"),
#     axis.line = element_line(),
#     plot.title = element_text(hjust = 0.5)
#   )
# 
# ggMarginal(p, type = "histogram")
# 
# 
# visual_levels <- raw_levels %>% 
#   gather(key = "Scenario", value = "N", -Size, - Web, - Ecozone) %>% 
#   group_by(Scenario) %>% 
#   group_split()
# 
# 
# 
# cowplot::plot_grid(
#   visual_levels[[1]] %>% 
#     ggplot() +
#     geom_histogram(aes(N, fill = Size), position = "identity", alpha = 0.5, binwidth = 1) +
#     facet_wrap(Size ~ ., ncol = 3, scales = "free") +
#     theme(
#       panel.background = element_blank(),
#       panel.grid.major = element_line(colour = "gainsboro"),
#       axis.line = element_line(),
#       plot.title = element_text(hjust = 0.5)
#     ),
#   visual_levels[[2]] %>% 
#     ggplot() +
#     geom_histogram(aes(N, fill = Size), position = "identity", alpha = 0.5, binwidth = 1) +
#     facet_wrap(Size ~ ., ncol = 3, scales = "free") +
#     theme(
#       panel.background = element_blank(),
#       panel.grid.major = element_line(colour = "gainsboro"),
#       axis.line = element_line(),
#       plot.title = element_text(hjust = 0.5)
#     ),
#   visual_levels[[3]] %>% 
#     ggplot() +
#     geom_histogram(aes(N, fill = Size), position = "identity", alpha = 0.5, binwidth = 1) +
#     facet_wrap(Size ~ ., ncol = 3, scales = "free") +
#     theme(
#       panel.background = element_blank(),
#       panel.grid.major = element_line(colour = "gainsboro"),
#       axis.line = element_line(),
#       plot.title = element_text(hjust = 0.5)
#     ),
#   ncol = 1
# )
# 
# Scenarios <- raw_levels %>% 
#   gather(key = "Scenario", value = "N", -Size, - Web, - Ecozone) %>% 
#   group_by(Scenario) %>% 
#   group_split()
# 
# names(Scenarios) <- c("CU", "PN", "RW")
# 
# wilcox.test(
#   Scenarios$CU$N[Scenarios$CU$Size == "Megacarnivore"],
#   Scenarios$RW$N[Scenarios$RW$Size == "Megacarnivore"]
# )
# 
# raw_levels %>% 
#   gather(key = "Scenario", value = "N", -Size, - Web, - Ecozone) %>% 
#   filter(Size == "Megacarnivore") %>% 
#   glm(N ~ Scenario + Ecozone, data = ., family = "poisson") %>% 
#   summary()
# aov() %>% 
#   TukeyHSD()
# 
# # Rest --------------------------------------------------------------------
# 
# 
# raw_edges <- do.call("rbind", edges) %>% 
#   as_tibble() %>% 
#   add_column(Web = rep(Webs, sapply(edges, function(x) nrow(x)))) %>% 
#   mutate(Ecozone = map(Web, function(x) WDPA$Bioregion[x]) %>% unlist()) %>% 
#   mutate(
#     I = map(i, function(x){
#       if(x == 1)
#         "Microherbivore"
#       else if(x == 2)
#         "Megaherbivore"
#       else if(x == 3)
#         "Microcarnivore"
#       else if(x == 4)
#         "Mesocarnivore"
#       else if(x == 5)
#         "Megacarnivore"
#     }) %>% unlist(),
#     J = map(j, function(x){
#       if(x == 1)
#         "Microherbivore"
#       else if(x == 2)
#         "Megaherbivore"
#       else if(x == 3)
#         "Microcarnivore"
#       else if(x == 4)
#         "Mesocarnivore"
#       else if(x == 5)
#         "Megacarnivore"
#     }) %>% unlist(),
#     Ecozone = modify(Ecozone, function(x){
#       if(x == "AT")
#         "Afrotropic"
#       else if(x == "NA")
#         "Neartic"
#       else if(x == "AA")
#         "Australasia"
#       else if(x == "NT")
#         "Neotropic"
#       else if(x == "IM")
#         "Indomalaya"
#       else if(x == "PA")
#         "Paleartic"
#     })
#   )
# 
# raw_levels %>% 
#   group_by(Size) %>% 
#   summarize(
#     Rewilding = mean(RW / PN), 
#     Current = mean(CU / PN),
#     sigmaRW = sd(RW / PN),
#     sigmaCU = sd(CU / PN)
#   )
# 
# analysis_levels <- raw_levels %>% 
#   mutate(
#     Current = CU / PN,
#     Rewilding = RW / PN
#   ) %>% 
#   dplyr::select(Size, Ecozone, Current, Rewilding, Web)
# 
# analysis_levels <- rbind(
#   raw_levels[ , -c(2, 4)] %>% transmute(Scenario = "CU", value = CU, Size, Ecozone, Web),
#   raw_levels[ , -c(2, 3)] %>% transmute(Scenario = "RW", value = RW, Size, Ecozone, Web),
#   raw_levels[ , -c(3, 4)] %>% transmute(Scenario = "PN", value = PN, Size, Ecozone, Web)
# ) %>% 
#   mutate(
#     Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
#     Baseline = rep(raw_levels$PN, 3),
#     Web = factor(Web)
#   ) %>% 
#   filter(value <= Baseline)
# 
# raw_edges %>%
#   dplyr::select(I, J, Ecozone, PN, CU, RW) %>% 
#   gather(key = "Scenario", value = "value", -I, -J, -Ecozone, -PN) %>% 
#   mutate(Fraction = value / PN) %>% 
#   filter(Fraction <= 1, Fraction >= 0) %>% 
#   glm(cbind(value, PN) ~ Scenario + Ecozone, data = ., family = binomial()) %>% 
#   plot()
# 
# analysis_edge <- rbind(
#   raw_edges[ , -c(2, 5)] %>% transmute(Scenario = "CU", value = CU, I, J, Ecozone),
#   raw_edges[ , -c(3, 4)] %>% transmute(Scenario = "RW", value = RW, I, J, Ecozone)
# ) %>% 
#   mutate(
#     Scenario = factor(Scenario),
#     Baseline = rep(raw_edges$PN, 2)
#   ) %>% 
#   filter(value <= Baseline)
# 
# # Data inspection -----
# par(mfrow = c(2, 3))
# sapply(levels(data$Size), function(x)
#   data %>% filter(Size == x) %>% boxplot(value ~ Scenario, data = ., main = x)
# )
# 
# data %>% 
#   group_by(Scenario, Size) %>% 
#   summarize(Mean = mean(value),
#             Variance = var(value),
#             Median = median(value),
#             MAD = mad(value),
#             Normality = shapiro.test(value)$p.value > 0.05)
# 
# #Microherbivore, Microcarnivore,  are good
# par(mfrow = c(2, 3))
# for(cat in levels(data$Size)){
#   a <- glm(
#     value ~ Scenario, 
#     data = analysis_levels[analysis_levels$Size == cat, ], 
#     family = poisson
#   )
#   qqnorm(resid(a), main = paste0(cat, " - Shapiro test p-value = ", round(shapiro.test(resid(a))$p.value, 4)))
#   qqline(resid(a))
# }
# TukeyHSD(aov(a))
# 
# par(mfrow = c(1, 3))
# for(cat in c("Megacarnivore", "Megaherbivore", "Mesocarnivore")){
#   a <- zeroinfl(
#     value ~ Scenario,
#     data = analysis_levels[analysis_levels$Size == cat, ], 
#     dist = "negbin"
#   )
#   qqnorm(resid(a), main = cat)
#   qqline(resid(a))
# }
# 
# # GLM -----
# model <- glm(
#   value ~ Scenario : Size + Ecozone, 
#   data = data, 
#   family = poisson(link = "log")
# )
# 
# plot(fitted(model), resid(model), pch = 20, xlab = "Predicted", ylab = "Fitted")
# abline(h = 0, col = "tomato", lty = 2, lwd = 2)
# qqnorm(resid(model), pch = 20)
# qqline(resid(model), col = "tomato", lty = 2, lwd = 2)
# 
# model <- glm(
#   cbind(value, Baseline) ~ Scenario : Size + Ecozone, 
#   data = data, 
#   family = binomial(link = "logit")
# )
# 
# plot(fitted(model), resid(model), pch = 20, xlab = "Predicted", ylab = "Fitted")
# abline(h = 0, col = "tomato", lty = 2, lwd = 2)
# qqnorm(resid(model), pch = 20)
# qqline(resid(model), col = "tomato", lty = 2, lwd = 2)
# 
# 
# # Zero-finalted -----
# 
# library(pscl)
# model <- zeroinfl(
#   value ~ Scenario : Size + Ecozone, 
#   data = data, 
#   dist = "poisson",
#   link = "log"
# )
# 
# summary(model)
# plot(model)
# 
# # Beta inflated regression ----
# # THIS WORKS!!!!!
# library(gamlss)
# 
# labellers <- c("Current", "Rewilding")
# names(labellers) <- c("CU", "RW")
# 
# analysis_levels %>% 
#   filter(Scenario != "PN") %>% 
#   mutate(Proportion = value / Baseline) %>% 
#   ggplot() +
#   geom_histogram(aes(Proportion, y = ..density..), binwidth = 0.1) +
#   geom_density(aes(Proportion), fill = "gainsboro", alpha = 0.2) +
#   xlab("Proportion of trophic levels compared to present-natural") +
#   ylab("Density") +
#   ggtitle(expression("Zero one inflated " * beta * "-distribution of trophic levels proportion")) +
#   facet_wrap(Scenario ~ ., ncol = 2, labeller = labeller(Scenario = labellers)) +
#   theme(
#     panel.background = element_blank(),
#     panel.grid.major = element_line(colour = "gainsboro"),
#     axis.line = element_line(),
#     plot.title = element_text(hjust = 0.5)
#   )
# 
# ggsave("../../Manuscript/Figures/beta_inflated_TL.png", width = 8, height = 6)
# 
# analysis_levels <- analysis_levels %>% 
#   mutate(Ecozone = factor(Ecozone)) %>% 
#   filter(Scenario != "PN") %>% 
#   mutate(value = modify2(value, Baseline, function(x, y)
#     if(x == y)
#       x - 0.01
#     else
#       x))
# 
# model <- gamlss(
#   formula = value / Baseline ~ 1 + Scenario : Size + random(Ecozone),
#   family = BEINF, #BEINF, BB, BE, BNB, BEOI
#   weigth = Baseline, 
#   data = analysis_levels,
#   contrasts = c("Scenario")
# )
# 
# plot(model)
# summary(model)
# 
# confint(model)
# 
# 
# # Mixed models -----
# library(lme4)
# # If Web as random effect necessary? ----
# NoWeb <- glmer(
#   cbind(value, Baseline) ~ Size : Scenario + (1 | Ecozone),
#   data = analysis_levels[analysis_levels$Scenario != "PN", ], 
#   binomial, nAGQ = 1
# )
# 
# WebRandom <- glmer(
#   cbind(value, Baseline) ~ Size : Scenario + (1 | Ecozone) + (1 | Web),
#   data = analysis_levels[analysis_levels$Scenario != "PN", ], 
#   binomial, nAGQ = 1
# )
# 
# anova(NoWeb, WebRandom) #no
# # Generalized linear mixed model -----
# data <- analysis_levels %>% 
#   mutate(Scenario = factor(Scenario),
#          Size = factor(Size),
#          Ecozone = factor(Ecozone))
# 
# model <- glmer(
#   cbind(value, Baseline) ~ (Size | Scenario) + (1 | Ecozone),
#   data = data[data$Scenario != "PN", ],# & (data$value / data$Baseline) >= 0, ], #it is indeed zero-inflated 
#   binomial(), nAGQ = 1
# )
# 
# plot(fitted(model), resid(model), pch = 20, xlab = "Predicted", ylab = "Fitted")
# abline(h = 0, col = "tomato", lty = 2, lwd = 2)
# qqnorm(resid(model), pch = 20)
# qqline(resid(model), col = "tomato", lty = 2, lwd = 2)
# 
# # Try a zero-inflated model ----
# library(glmmTMB)
# model <- glmmTMB(
#   value ~ Size : Scenario + (1 | Ecozone),
#   data = analysis_levels[analysis_levels$Scenario != "PN", ], 
#   family = nbinom2(link = "log"), 
#   ziformula = ~ Size : Scenario
# )
# fixef(model)
# plot(fitted(model), resid(model), pch = 20, xlab = "Predicted", ylab = "Fitted")
# abline(h = 0, col = "tomato", lty = 2, lwd = 2)
# qqnorm(resid(model), pch = 20)
# qqline(resid(model), col = "tomato", lty = 2, lwd = 2)
# 
# # Bayesian MCMC ----
# library(blme)
# model <- bglmer(
#   cbind(value, Baseline) ~ Size : Scenario + (1 | Ecozone),
#   data = analysis_levels[analysis_levels$Scenario != "PN", ], 
#   binomial()
# )
# 
# plot(fitted(model), resid(model), pch = 20, xlab = "Predicted", ylab = "Fitted")
# abline(h = 0, col = "tomato", lty = 2, lwd = 2)
# qqnorm(resid(model), pch = 20)
# qqline(resid(model), col = "tomato", lty = 2, lwd = 2)
# 
# 
# # EDi ---------------------------------------------------------------------
# edi_results <- web_results[grep("Web\\d+", web_results)]
# 
# Webs <- sapply(edi_results, function(x){
#   substr(x, str_locate(x, "\\d+")[1], str_locate(x, "\\d+")[2])
# }) %>% as.numeric()
# 
# EDI <- list()
# for(i in 1:length(edi_results)){
#   EDI[[i]] <- read_csv(edi_results[i], col_types = cols()) %>% 
#     mutate(Web = Webs[i]) %>% 
#     dplyr::select(EDi, Scenario, Web)
# }
# 
# EDI <- do.call("rbind", EDI) %>% 
#   as_tibble() %>% 
#   mutate(Ecozone = factor(map(Web, function(x) WDPA$Bioregion[x]) %>% unlist()))
# 
# a <- EDI %>% 
#   filter(Web == 220)
# 
# EDI_model <- EDI %>% 
#   mutate(
#     EDi = log10(EDi),
#     Ecozone = factor(Ecozone),
#     Scenario = factor(Scenario),
#     Web = factor(Web)
#   ) %>% 
#   group_by(Web, Scenario, Ecozone) %>% 
#   summarize(Mean = mean(EDi))
# 
# model <- glmer(
#   Mean ~ Scenario + (1 | Ecozone) + (1 | Web),
#   data = EDI_model
# )
# 
# plot(fitted(model), resid(model), pch = 20, xlab = "Predicted", ylab = "Fitted")
# abline(h = 0, col = "tomato", lty = 2, lwd = 2)
# qqnorm(resid(model), pch = 20)
# qqline(resid(model), col = "tomato", lty = 2, lwd = 2)
# 
# summary(model)
# 
# CI <- confint(model)
# 
# EDI %>% 
#   ggplot() +
#   geom_histogram(aes(EDi)) +
#   facet_grid(Scenario ~ .)
