library(tidyverse)
library(cowplot)
source("get_mass.R")
source('my_theme.R')


Stats <- full_join(
  read_csv("/NewSpace/Maps/Maxent/SDM_stats.csv"),
  read_csv("/NewSpace/Maps/CBI/cbi.csv", col_names = c('Species', 'CBI'))
)

Stats %>% 
  filter(!is.na(OR.min)) %>% 
  summarize(Mean = mean(OR.min),
            Std = sd(OR.min),
            Median = median(OR.min),
            Mad = mad(OR.min))

Stats <- read_csv("/NewSpace/Maps/CBI/cbi.csv", col_names = c('Species', 'CBI')) %>% 
  mutate(Mass = as.numeric(modify(Species, function(x) get_mass(x)$Mass)))

CBI <- Stats %>% 
  ggplot() +
  geom_histogram(aes(CBI), binwidth = 0.05, fill = "gainsboro", col = "black") +
  ylab("Number of species") +
  xlab("Continuous Boyce index") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

Stats <- read_csv("/NewSpace/Maps/Maxent/SDM_stats.csv", col_types = cols()) %>% 
  mutate(Mass = as.numeric(modify(Species, function(x) get_mass(x)$Mass)))

AUC <- Stats %>% 
  ggplot() +
  geom_histogram(aes(AUC), binwidth = 0.025, fill = "gainsboro", col = "black") +
  ylab("Number of species") +
  xlab("AUC") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# ORMTP -------------------------------------------------------------------
ORTMP <- Stats %>% 
  ggplot() +
  geom_histogram(aes(OR.min), binwidth = 0.05, fill = "gainsboro", col = "black") +
  ylab("Number of species") +
  xlab(expression("OR"[TMP])) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# ggsave("../../Manuscript/Figures/ORTMP.svg", width = 6, height = 6)

predictions <- read_csv("../../Results/Predictions_introductions.csv", col_names = T, col_types = cols())

predictions %>% 
  mutate(Predicted = Predicted / Introduction) %>% 
  pull(Predicted) %>% 
  qplot()

pred <- predictions %>% 
  mutate(Source = map(Source, function(x){
    if(x == "Lundgren")
      "Lungren et al. (2018)"
    else
      "IUCN (2016)"
  }) %>% unlist()) %>% 
  ggplot() +
  geom_abline(aes(slope = 1, intercept = 0), linetype = 2, alpha = 1, col = 'gray80') +
  geom_point(aes(Introduction + 1, Predicted + 1, col = Source), 
             alpha = 0.4, size = 2, show.legend = FALSE) +
  scale_x_log10(limits = c(1, max(predictions$Introduction)), labels = fancy_scientific, expand = c(0.01, 0)) +
  scale_y_log10(limits = c(1, max(predictions$Introduction)), labels = fancy_scientific, expand = c(0.01, 0)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    panel.grid = element_blank(),
    legend.position = c(0.3, 0.8),
    plot.title = element_text(hjust = 0.5),
    legend.key = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  xlab(expression("Introduced range (km"^2*')')) +
  ylab(expression("Predicted range (km"^2*')')) +
  scale_color_manual(values = c('black', 'tomato')) +
  ggtitle("")

plot_grid(CBI + scale_x_continuous(limits = c(0, 1)), 
          AUC + scale_x_continuous(limits = c(0.5, 1)), 
          ORTMP, 
          pred, 
          nrow = 2, labels = "auto")

ggsave("../../Manuscript/Figures/SDM_stats.png", width = 8, height = 6)


# Replacement mass --------------------------------------------------------
source("select_species.R")

dead <- get_mass(extinct) %>% 
  mutate(Status = "Extinct")

repl <- read_csv("../../Results/Replacements.csv", col_types = cols()) %>% 
  pull(Replacement) %>% 
  sort() %>% 
  unique() %>% 
  get_mass() %>% 
  mutate(Status = "Alive")

bind_rows(dead, repl) %>% 
  mutate(Status = factor(Status, levels = c("Extinct", "Alive"))) %>% 
  ggplot() +
  geom_histogram(aes(Mass, fill = Status), position = "identity", binwidth = 0.25, col = "black") +
  xlab("Body mass (g)") +
  ylab("Number of species") +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = wesanderson::wes_palette("Royal1", 2)) +
  monkey_theme +
  theme(legend.position = c(0.03, 0.875))

ggsave("../../Manuscript/Figures/repl_mass_range.png", width = 8, height = 5)

# inkscape export ---------------------------------------------------------
# run in bash terminal
# inkscape AUC.svg -e AUC.png -d 300
# inkscape CBI.svg -e CBI.png -d 300
# inkscape ORTMP.svg -e ORTMP.png -d 300
