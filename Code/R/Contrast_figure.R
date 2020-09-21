library(tidyverse)
library(magrittr)
library(circlize)
library(see)

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

# levels ------------------------------------------------------------------
res <- read_csv("../../Results/level_estimates.csv")
contr <- read_csv("../../Results/contrasts_levels.csv")

contr %>% 
  mutate(Contrast = map2(Level2, Level1, function(x, y) {
    paste(x, y, sep = " - ")
  }) %>% 
    unlist() %>% 
    factor(levels = c("RW - PN",
                      "RW - CU",
                      "CU - PN"))) %>% 
  mutate(Size = map(Size, function(x) {
    rename_tl2(x)
  }) %>% unlist() %>% 
    factor(levels = c("Megacarnivores", 
                      "Megaherbivores",
                      "Large carnivores",
                      "Large herbivores",
                      "Small carnivores",
                      "Small herbivores"))) %>% 
  mutate(Area = factor(Area, levels = c("Random areas", "Protected areas"))) %>% 
  ggplot() +
  geom_point(aes(Contrast, -Difference, col = Area),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(Contrast, ymin = -CI_low, ymax = -CI_high,
                    col = Area),
                position = position_dodge(width = 0.5),
                width = 0) +
  xlab("") +
  ylab("Standardized difference") +
  scale_x_discrete(labels = c("Rewilding \n vs \n No-extinction",
                              "Rewilding \n vs \n Current",
                              "Current \n vs \n No-extinction")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Size, ncol = 2, scales = "free") +
  coord_flip() +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.text.y = element_text(hjust = 0.5,
                                   size = 7),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(values = as.vector(ggsci:::ggsci_db$startrek$uniform[c(2, 4)]))

ggsave("../../Manuscript/Figures/Contrasts_levels.png",
       width = 6, 
       height = 6)

# edges ------------------------------------------------------------------
res <- read_csv("../../Results/edge_estimates.csv")
contr <- read_csv("../../Results/contrasts_edges.csv")
# \u279E is Latex $\rightarrow$
contr %>% 
  mutate(Contrast = map2(Level2, Level1, function(x, y) {
    paste(x, y, sep = " - ")
  }) %>% 
    unlist() %>% 
    factor(levels = c("RW - PN",
                      "RW - CU",
                      "CU - PN"))) %>% 
  mutate(Area = factor(Area, levels = c("Random areas", "Protected areas"))) %>% 
  mutate(Edge = map(Edge, function(x) {
    ans1 <- str_split(x, " -> ", simplify = TRUE)[1] %>% 
      rename_tl2()
    ans2 <- str_split(x, " -> ", simplify = TRUE)[2] %>% 
      rename_tl2()
    ans <- paste(ans1, "\u279E", ans2)
  }) %>% unlist()) %>% 
  ggplot() +
  geom_point(aes(Contrast, -Difference, col = Area),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(Contrast, ymin = -CI_low, ymax = -CI_high,
                    col = Area),
                position = position_dodge(width = 0.5),
                width = 0) +
  xlab("") +
  ylab("Standardized difference") +
  scale_x_discrete(labels = c("Rewilding \n vs \n No-extinction",
                              "Rewilding \n vs \n Current",
                              "Current \n vs \n No-extinction")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Edge, ncol = 2, scales = "free") +
  coord_flip() +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.text.y = element_text(hjust = 0.5,
                                   size = 7),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(values = as.vector(ggsci:::ggsci_db$startrek$uniform[c(2, 4)]))

ggsave("../../Manuscript/Figures/Contrasts_edges.png",
       width = 6, 
       height = 8)

# Chord-gram --------------------------------------------------------------
lev <- read_csv("../../Results/level_estimates.csv")
edge <- read_csv("../../Results/edge_estimates.csv")

plot_chord <- function(d, scenario, area) {
  d %<>% 
    filter(Scenario == scenario,
           Area == area) %>% 
    transmute(rowname = From,
              key = To,
              value = log10(Estimate))
  d %<>% 
    mutate(
      rowname = modify(rowname, function(x) {
        rename_tl2(x)
      }),
      key = modify(key, function(x) {
        rename_tl2(x)
      }))
  circos.clear()
  circos.par(start.degree = 90, 
             gap.degree = 4, 
             track.margin = c(-0.1, 0.1), 
             points.overflow.warning = FALSE)
  par(mar = rep(0, 4))
  chordDiagram(
    x = d, 
    grid.col = mycolor[1:5],
    row.col = 1:5,
    transparency = 0.25,
    directional = 1,
    direction.type = c("arrows", "diffHeight"), 
    diffHeight  = -0.04,
    annotationTrack = "grid", 
    annotationTrackHeight = c(0.05, 0.1),
    link.arr.type = "big.arrow", 
    link.sort = TRUE, 
    link.largest.ontop = TRUE
  )
  circos.trackPlotRegion(
    track.index = 1, 
    bg.border = NA, 
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      sector.index = get.cell.meta.data("sector.index")
      # Add names to the sector. 
      circos.text(
        x = mean(xlim), 
        y = 3.2, 
        labels = sector.index, 
        facing = "bending", 
        cex = 0.8
      )
      # Add graduation on axis
      circos.axis(
        h = "top", 
        major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
        minor.ticks = 1, 
        major.tick.percentage = 0.5,
        labels.niceFacing = FALSE)
    }
  )
  return(d)
}

mycolor <- c("#E1AF00", "#3B9AB2",
             "#EBCC2A", "#78B7C5",
             "#F21A00", "#00637c")

plot_chord(edge, "PN", "Protected area")

pn <- edge %>% 
  filter(Scenario == "PN",
         Area == "Protected area") %>% 
  transmute(rowname = From,
            key = To,
            value = log10(Estimate))

pn %<>% 
  mutate(
    rowname = modify(rowname, function(x) {
      rename_tl2(x)
    }),
    key = modify(key, function(x) {
      rename_tl2(x)
    }))



chordDiagram(
  x = d, 
  grid.col = mycolor[1:5],
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = 3.2, 
      labels = sector.index, 
      facing = "bending", 
      cex = 0.8
    )
    
    # Add graduation on axis
    circos.axis(
      h = "top", 
      major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
      minor.ticks = 1, 
      major.tick.percentage = 0.5,
      labels.niceFacing = FALSE)
  }
)

# raw values --------------------------------------------------------------
d <- read_csv("../../Results/levels_raw.csv")

mycolor <- c(MC = "#F21A00",
             LC = "#E1AF00", 
             SC = "#EBCC2A",
             MH = "#00637c",
             LH = "#3B9AB2",
             SH = "#78B7C5")

mycolor <- c(MC = "tomato",
             MH = "dodgerblue",
             LC = "orange", 
             LH = "skyblue2",
             SC = "lightgoldenrod2",
             SH = "lightblue2")

source("geom_flat_violin.R")

d %>% 
  filter(Area == "Protected") %>% 
  mutate(Scenario = factor(Scenario, levels = c("PN", "CU", "RW")),
         Size = modify(Size, function(x) {
           rename_tl2(x)
         }) %>% 
           factor(levels = c("Megacarnivores", 
                             "Megaherbivores",
                             "Large carnivores",
                             "Large herbivores",
                             "Small carnivores",
                             "Small herbivores"))) %>% 
  group_by(Size) %>% 
  add_tally() %>% 
  ungroup() %>% 
  mutate(Jitter = modify(as.numeric(Scenario), function(x) {
    x - abs(rnorm(1, 0, 0.1))
    })) %>% 
  ggplot() + 
  geom_jitter(aes(Jitter, Proportion),
             size = 1.5, 
             height = 0.1, 
             alpha = 0.25) +
  # geom_jitter(aes(Scenario, Proportion),
  #             alpha = 0.25,
  #             width = 0.25,
  #             height = 0.1,
  #             size = 0.75) +
  geom_violinhalf(aes(Scenario, Proportion, fill = Size),
              show.legend = FALSE,
              alpha = 0.5,
              #bw = 1,
              scale = "width",
              trim = FALSE,
              #draw_quantiles = 0.5,
              size = 0.5) +
  xlab("") +
  ylab("Number of species") +
  scale_x_discrete(labels = c("No-extinction", "Current", "Rewilding")) +
  scale_fill_manual(values = as.vector(mycolor)) +
  facet_wrap(Size ~ ., scales = "free_y", ncol = 2) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0.5),
        text = element_text(size = 14),
        strip.text.x = element_text(size = 12))

ggsave("../../Manuscript/Figures/raw_levels.png", width = 6.5, height = 8)
ggsave("../../Manuscript/Figures/raw_levels.svg", width = 6.5, height = 8)

