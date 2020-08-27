library(tidyverse)
library(sf)

WDPA <- read_sf("../../Data/PA_5000km_bioregions.shp") %>%
  mutate(Area = as.numeric(st_area(geometry)) / 10^6) %>%
  filter(Area > 5000)

web_results <- list.files("../../Results/Webs/", pattern = "csv", full.names = TRUE)
res <- list()
for(file in web_results[grep("Web\\d+", web_results)]){
  web <- as.numeric(sub(".csv", "", sub("Web", "", strsplit(file, "//")[[1]][2])))
  edi <- read_csv(file, col_types = cols()) #%>% mutate(EDi = ED_Predator * ED_Prey)
  # Mu <- list()
  # if(all(c("PN", "CU", "RW") %in% edi$Scenario)){
  #   for(i in 1:200){
  #     Means <- edi %>% 
  #       group_by(Predator) %>% 
  #       sample_n(1) %>% 
  #       group_by(Prey) %>% 
  #       sample_n(1) %>% 
  #       group_by(Scenario) %>% 
  #       summarize(Mean = mean(EDi))
  #     Mu[[i]] <- Means
  #   }
  #   
  #   res[[which(web_results == file)]] <- do.call("rbind", Mu) %>% 
  #     mutate(Scenario = factor(Scenario, levels = c("PN", "CU", "RW"))) %>% 
  #     glm(Mean ~ Scenario + 0, data = ., family = Gamma(link = "identity")) %>% 
  #     coef() %>% 
  #     data.frame() %>% 
  #     t() %>% 
  #     cbind(Ecozone = WDPA$Bioregion[web]) %>% 
  #     cbind(Web = web)
  # }
  res[[which(web_results == file)]] <- read_csv(file, col_types = cols()) %>% 
    mutate(Ecozone = WDPA$Bioregion[web], Web = web)
}

model <- do.call("rbind", res) %>% 
  as_tibble() %>% 
  mutate(
    EDi = ED_Predator + ED_Prey,
    Ecozone = factor(Ecozone),
    Web = factor(Web)
  ) %>% 
  #arrange(desc(EDi)) %>% 
  #group_by(Predator, Prey, Web) %>% 
  #sample_n(1) %>% 
  lmer(log10(EDi) ~ 0 + Scenario + (1 | Ecozone) + (1 | Web), data = .)

qqnorm(resid(model), pch = 20)  
qqline(resid(model), lty = 2, col = "tomato")

summary(model)$coefficients

tibble(EDi = model@frame$`log10(EDi)`,
       Ecozone = model@frame$Ecozone,
       Scenario = model@frame$Scenario) %>% 
  ggplot() + 
  geom_boxplot(aes(factor(Ecozone), EDi, fill = Scenario)) +
  scale_y_log10()

# do.call("rbind", res) %>% 
#   as_tibble() %>% 
#   transmute(PN = ScenarioPN, CU = ScenarioCU, RW = ScenarioRW, Ecozone = Ecozone) %>% 
#   gather(key = "Scenario", value = "value", -Ecozone) %>% 
#   mutate(Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
#          value = as.numeric(value)) %>% 
#   ggplot() +
#   geom_histogram(aes(value, y = ..density.., fill = Scenario), 
#                  col = "black", binwidth = 0.5, alpha = 1, position = "identity") +
#   geom_density(aes(value), fill = "black", alpha = 0.25) +
#   facet_wrap(Scenario ~ ., ncol = 1, scales = "free") +
#   ggtitle("Evolutionary distinctess of interactions") +
#   theme(
#     panel.background = element_blank(),
#     panel.grid.major = element_line(colour = "gainsboro"),
#     axis.line = element_line(),
#     plot.title = element_text(hjust = 0.5)
#   )

WDPA$Bioregion[web]

dat <- do.call("rbind", res) %>% 
  as_tibble() %>% 
  transmute(PN = ScenarioPN, CU = ScenarioCU, RW = ScenarioRW, Ecozone, Web) %>% 
  gather(key = "Scenario", value = "value", -Ecozone, -Web) %>% 
  mutate(Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
         Ecozone = factor(Ecozone, levels = c("AT", "IM", "PA", "NA", "NT", "AA")),
         value = as.numeric(value))

# model <- lm(value ~ Scenario + Ecozone + 0, data = dat)
# par(mfrow = c(2, 2))
# plot(model)
# summary(model)
# 
# library(lme4)
# model <- lmer(
#   value ~ Scenario + (1 | Ecozone) + 0, 
#   data = dat
# )

fit <- stan_lmer(
  value ~ Scenario + (1 | Ecozone), 
  data = dat,
  cores = 4,
  chains = 4
)

pp_check(fit)

shinystan::launch_shinystan(fit)

plot(fit, regex_pars = "Scenario")

bay <- describe_posterior(
  pairs(emmeans(fit, ~ Scenario)),
  estimate = "median", dispersion = TRUE,
  ci = .9, ci_method = "hdi",
  test = c("bayesfactor"),
  bf_prior = fit
)

plot(equivalence_test(fit))

interpret_bf(bay$BF)

# confint(model)
# par(mfrow = c(1, 1))
# plot(model, pch = 20)
# qqnorm(resid(model))
# qqline(resid(model))
# summary(model)$coefficients
# 
# meta <- summary(model)$coefficients[1:3, 1:2]
# cohen_d <- rep(NA, 3)
# for(i in 1:nrow(comb <- t(combn(3, 2)))){
#   cohen_d[i] <- abs(meta[comb[i, 1]] - meta[comb[i, 2]]) / meta[1, 2] * (1 - 3 / (4 * 208*2 - 1))
# }
# names(cohen_d) <- c("PN - CU", "PN - RW", "CU - RW")
# cohen_d

# meta <- cbind(lower = confint(model)[ , 1], 
#               mean = c(NA, NA, fixef(model)),
#               upper = confint(model)[ , 2])

do.call("rbind", res) %>% 
  as_tibble() %>% 
  transmute(PN = ScenarioPN, CU = ScenarioCU, RW = ScenarioRW, Ecozone = Ecozone) %>% 
  gather(key = "Scenario", value = "value", -Ecozone) %>% 
  mutate(Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
         value = as.numeric(value)) %>% 
  ggplot() +
  geom_boxplot(aes(Scenario, value)) +
  ggtitle("Evolutionary distinctess of interactions") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gainsboro"),
    axis.line = element_line(),
    plot.title = element_text(hjust = 0.5)
  )

meta_sc <- meta[grep("Scenario", row.names(meta)), ]
meta_sc %>% 
  as_tibble() %>% 
  mutate(Scenario = factor(c("Present-natural", "Current", "Rewilding"),
                           levels = c("Present-natural", "Current", "Rewilding"))) %>% 
  ggplot() +
  geom_point(aes(Scenario, Estimate)) +
  geom_errorbar(aes(Scenario, ymin = Estimate - `Std. Error`, ymax =  Estimate + `Std. Error`)) +
  ylab("Average coefficient") +
  xlab("") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gainsboro"),
    axis.line = element_line(),
    plot.title = element_text(hjust = 0.5)
  ) +
  annotate("rect", xmin = 2.4, xmax = 3.6, ymin = 13.65, ymax = 13.9, alpha = 1, fill = "white", col = "black") +
  geom_text(aes(x = 3, y = 13.85, label = "Cohen's d effect size"), fontface = "bold") +
  geom_text(aes(x = 3, y = 13.8, label = "Present-natural - current = 0.42")) +
  geom_text(aes(x = 3.025, y = 13.75, label = "Present-natural - rewilding = 0.38")) +
  geom_text(aes(x = 2.91, y = 13.7, label = "Rewilding - current = 0.05"))

ggsave("../../Manuscript/Figures/EDi_effect_size.png", width = 8, height = 6)





tibble(
  Scenario = factor(c("Present-natural", "Current", "Rewilding"), 
                    levels = c("Present-natural", "Current", "Rewilding")),
  Mean = as.numeric(coefficients(model)$Ecozone[1, 2:4]),
  CIlow = as.numeric(confint(model)[3:5, 1]),
  CIhigh = as.numeric(confint(model)[3:5, 2])
) %>% 
  ggplot() +
  geom_point(aes(Scenario, Mean)) +
  geom_errorbar(aes(Scenario, ymin = Mean - CIlow, ymax = Mean + CIhigh)) +
  ggtitle("Evolutionary distinctess of interactions confidence interval") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gainsboro"),
    axis.line = element_line(),
    plot.title = element_text(hjust = 0.5)
  )
