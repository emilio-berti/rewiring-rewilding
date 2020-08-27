library(tidyverse)
library(sf)

WDPA <- read_sf("../../Data/PA_5000km_bioregions.shp") %>%
  mutate(Area = as.numeric(st_area(geometry)) / 10^6) %>%
  filter(Area > 5000)

web_results <- list.files("../../Results/Webs/", pattern = "csv", full.names = TRUE)
TL <- web_results[grep("TL_random", web_results)]

Webs <- sapply(TL, function(x){
  substr(x, str_locate(x, "\\d+")[1], str_locate(x, "\\d+")[2])
}) %>% as.numeric()

trophic_levels <- list()
for(i in 1:length(TL)){
  trophic_levels[[i]] <- read_csv(TL[i], col_types = cols()) 
}
names(trophic_levels) <- Webs

raw_levels <- do.call("rbind", trophic_levels) %>% 
  as_tibble() %>% 
  add_column(Web = rep(Webs, sapply(trophic_levels, function(x) nrow(x)))) %>% 
  mutate(Ecozone = map(Web, function(x) WDPA$Bioregion[x]) %>% unlist())

analysis_levels <- raw_levels %>% 
  mutate(
    Current = CU / PN,
    Rewilding = RW / PN
  ) %>% 
  dplyr::select(Size, Ecozone, Current, Rewilding, Web)

analysis_levels <- rbind(
  raw_levels[ , -c(2, 4)] %>% transmute(Scenario = "CU", value = CU, Size, Ecozone, Web),
  raw_levels[ , -c(2, 3)] %>% transmute(Scenario = "RW", value = RW, Size, Ecozone, Web),
  raw_levels[ , -c(3, 4)] %>% transmute(Scenario = "PN", value = PN, Size, Ecozone, Web)
) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
    Baseline = rep(raw_levels$PN, 3),
    Web = factor(Web)
  ) %>% 
  filter(value <= Baseline)

# Ordered logistic regression ----
dat <- analysis_levels %>% 
  filter(Scenario != "PN") %>% 
  mutate(
    Fraction = value / Baseline,
    Scenario = droplevels(Scenario),
    Size = factor(Size)
  )

rewilding <- dat %>% 
  filter(Scenario == "RW")

current <- dat %>% 
  filter(Scenario == "CU")

hist(rewilding$value / rewilding$Baseline)
hist(current$value / current$Baseline)

hist(rewilding$value / rewilding$Baseline, breaks = 500)
hist(current$value / current$Baseline, breaks = 500)

dat <- dat %>% 
  mutate(Fraction = round(Fraction, 1) * 10)

glm(
  Fraction / 10 ~ Size : Scenario : Ecozone, 
  data = dat,
  family = "binomial"
) %>% 
  plot()

# Data inspection and naive poisson GLM -----
par(mfrow = c(2, 3))
sapply(levels(data$Size), function(x)
  data %>% filter(Size == x) %>% boxplot(value ~ Scenario, data = ., main = x)
)

data %>% 
  group_by(Scenario, Size) %>% 
  summarize(Mean = mean(value),
            Variance = var(value),
            Median = median(value),
            MAD = mad(value),
            Normality = shapiro.test(value)$p.value > 0.05)

#Microherbivore, Microcarnivore,  are good
par(mfrow = c(2, 3))
for(cat in levels(data$Size)){
  a <- glm(
    value ~ Scenario, 
    data = analysis_levels[analysis_levels$Size == cat, ], 
    family = poisson
  )
  qqnorm(resid(a), main = paste0(cat, " - Shapiro test p-value = ", round(shapiro.test(resid(a))$p.value, 4)))
  qqline(resid(a))
}

clust <- makeCluster(detectCores() - 1)
model <- glmm(
  cbind(value, Baseline) ~ 1 + Scenario : Size, 
  list(cbind(value, Baseline) ~ 0 + Ecozone),
  varcomps.names=c("z1"),
  data = analysis_levels, 
  family.glmm = binomial.glmm,
  m = 10^3,
  cluster = clust
)

par(mfrow = c(2, 2))
plot(model)


# Beta inflated regression ----
library(gamlss)

labellers <- c("Current", "Rewilding")
names(labellers) <- c("CU", "RW")

analysis_levels %>% 
  filter(Scenario != "PN") %>% 
  mutate(Proportion = value / Baseline) %>% 
  ggplot() +
  geom_histogram(aes(Proportion, y = ..density..), binwidth = 0.1) +
  geom_density(aes(Proportion), fill = "gainsboro", alpha = 0.2) +
  xlab("Proportion of trophic levels compared to present-natural") +
  ylab("Density") +
  ggtitle(expression("Zero one inflated " * beta * "-distribution of trophic levels proportion")) +
  facet_wrap(Scenario ~ ., ncol = 2, labeller = labeller(Scenario = labellers)) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gainsboro"),
    axis.line = element_line(),
    plot.title = element_text(hjust = 0.5)
  )

dat <- analysis_levels %>% 
  mutate(
    Ecozone = factor(Ecozone),
    Size = factor(Size)
  )

model <- gamlss(
  formula = value / Baseline ~ 1 + Scenario : Size + Ecozone,
  family = BEINF, #BEINF, BB, BE, BNB, BEOI
  weigth = Baseline, 
  data = analysis_levels,
  contrasts = c("Scenario")
)

plot(model)
summary(model)

p0 <- exp(model$nu.coefficients) / (1 + exp(model$nu.coefficients) + exp(model$tau.coefficients))
p1 <- exp(model$tau.coefficients) / (1 + exp(model$nu.coefficients) + exp(model$tau.coefficients))

CI <- confint(model)

plot(coefficients(model))

abline(h = (CI[1, 2] - CI[1, 1]) / 2)

means <- coefficients(model) %>% 
  broom::tidy() %>% 
  filter(!str_detect(names, c("PN")),
         str_detect(names, "Scenario")) %>% 
  mutate(
    Scenario = factor(rep(c("CU", "RW"), 5)),
    Size = factor(rep(c("Megacarnivore", "Megaherbivore", "Mesocarnivore", "Microcarnivore", "Mircoherbivore"), each = 2))
  )

filter(!is.na(x)) %>% 
  mutate(Scenario = rep(c("CU", "RW")))

confindence_intervals <- CI %>% 
  broom::tidy() %>% 
  names(confindence_intervals) <- c("names", "low", "high")

confindence_intervals <- confindence_intervals %>% 
  filter(!str_detect(names, c("PN")),
         str_detect(names, "Scenario")) %>% 
  mutate(
    Scenario = factor(rep(c("CU", "RW"), 5)),
    Size = factor(rep(c("Megacarnivore", "Megaherbivore", "Mesocarnivore", "Microcarnivore", "Mircoherbivore"), each = 2))
  )

ggplot() +
  geom_point(data = means, aes(Size, x, col = Scenario)) +
  geom_errorbar(data = confindence_intervals, 
                aes(x = Size, ymin = low, ymax = high, col = Scenario)) +
  geom_abline(aes(intercept = coefficients(model)[1], slope = 0)) +
  facet_wrap(Ecozone ~ ., ncol = 2) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gainsboro"),
    axis.line = element_line(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90)
  )


# Non-parametric multiple comparisons -------------------------------------
levels(analysis_levels$Ecozone)
levels(analysis_levels$Size)

for(i in levels(analysis_levels$Ecozone)){
  for(j in levels(analysis_levels$Size)){
    dat <- analysis_levels %>% 
      filter(Ecozone == i) %>% 
      filter(Size == j) %>% 
      mutate(Fraction = value / Baseline)
    pairwise.wilcox.test(dat$Fraction, dat$Scenario)$p.value
  }
}
res

x <- dat %>% 
  filter(Scenario == "PN") %>% 
  mutate(Fraction = value / Baseline) %>% 
  pull(Fraction)

y <- dat %>% 
  filter(Scenario == "CU") %>% 
  mutate(Fraction = value / Baseline) %>% 
  pull(Fraction)

wilcox.test(x, y, paired = F)
