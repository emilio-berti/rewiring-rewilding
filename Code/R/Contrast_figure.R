library(tidyverse)
library(magrittr)
library(circlize)
library(see)
library(gridExtra)
library(gtable)


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
  scale_x_discrete(labels = c("Rewilding - No-extinction",
                              "Rewilding - Current",
                              "Current - No-extinction")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = as.vector(ggsci:::ggsci_db$startrek$uniform[c(2, 4)])) +
  facet_wrap(~Size, ncol = 2, scales = "free_x") +
  coord_flip() +
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

ggsave("../../Manuscript/Figures/Contrasts_levels.pdf", width = 6, height = 6)
ggsave("../../Manuscript/Figures/Contrasts_levels.png", width = 6, height = 6)
ggsave("../../Manuscript/Figures/Contrasts_levels.svg", width = 6, height = 6)

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
    ans <- paste(ans2, "\n \u2191 \n", ans1)
  }) %>% unlist()) %>% 
  mutate(Edge = factor(Edge, levels = c("Megacarnivores \n ↑ \n Large carnivores",
                                        "Megacarnivores \n ↑ \n Large herbivores",
                                        "Megacarnivores \n ↑ \n Small carnivores",
                                        "Megacarnivores \n ↑ \n Small herbivores",
                                        "Large carnivores \n ↑ \n Large herbivores",
                                        "Large carnivores \n ↑ \n Small carnivores",
                                        "Large carnivores \n ↑ \n Small herbivores",
                                        "Small carnivores \n ↑ \n Small herbivores"))) %>% 
  ggplot() +
  geom_point(aes(Contrast, -Difference, col = Area),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(Contrast, ymin = -CI_low, ymax = -CI_high,
                    col = Area),
                position = position_dodge(width = 0.5),
                width = 0) +
  xlab("") +
  ylab("Standardized difference") +
  scale_x_discrete(labels = c("Rewilding - No-extinction",
                              "Rewilding - Current",
                              "Current - No-extinction")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Edge, ncol = 2, scales = "free_x") +
  coord_flip() +
  theme_classic() +
  scale_color_manual(values = as.vector(ggsci:::ggsci_db$startrek$uniform[c(2, 4)])) +
  theme(strip.background = element_blank(),
        text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

ggsave("../../Manuscript/Figures/Contrasts_edges.pdf", width = 6, height = 8)
ggsave("../../Manuscript/Figures/Contrasts_edges.png", width = 6, height = 8)
ggsave("../../Manuscript/Figures/Contrasts_edges.svg", width = 6, height = 8)

fig_table <- tibble(
  `Trophic level` = c("Megacarnivores", "Large carnivores", "Small carnivores",
                      "Megaherbivores", "Large herbivores", "Small herbivores"),
  `Size (kg)` = c("\u2265 100", "21.5-99", "< 21.5",
                  "\u2265 1,000", "45-999", "< 45")
)

tm <- ttheme_minimal(core = list(fg_params = list(hjust = 0, x = 0)),
                     rowhead = list(fg_params = list(hjust = 0, x = 0)))
grid.arrange(tableGrob(fig_table, theme = tm, rows = NULL))

# Chord-gram --------------------------------------------------------------
lev <- read_csv("../../Results/level_estimates.csv")
edge <- read_csv("../../Results/edge_estimates.csv")

mycolor <- c(Megacarnivores = "tomato",
             Megaherbivores = "dodgerblue",
             `Large carnivores` = "orange", 
             `Large herbivores` = "skyblue2",
             `Small carnivores` = "lightgoldenrod2",
             `Small herbivores` = "lightblue2")

plot_chord <- function(d, area) {
  d <- d %>% 
    filter(Area == area) %>% 
    group_by(Scenario) %>% 
    group_split(keep = TRUE)
  cu <- d[[1]]
  pn <- d[[2]]
  rw <- d[[3]]
  cu %<>% 
    transmute(rowname = From,
              key = To,
              value = log10(Estimate)) %>% 
    mutate(
      rowname = modify(rowname, function(x) {
        rename_tl2(x)
      }),
      key = modify(key, function(x) {
        rename_tl2(x)
      })) %>%
    mutate(rowname = factor(rowname, levels = c("Megacarnivores",
                                                "Large carnivores",
                                                "Small carnivores",
                                                "Small herbivores",
                                                "Large herbivores")))
  pn %<>% 
    transmute(rowname = From,
              key = To,
              value = log10(Estimate)) %>% 
    mutate(
      rowname = modify(rowname, function(x) {
        rename_tl2(x)
      }),
      key = modify(key, function(x) {
        rename_tl2(x)
      })) %>%
    mutate(rowname = factor(rowname, levels = c("Megacarnivores",
                                                "Large carnivores",
                                                "Small carnivores",
                                                "Small herbivores",
                                                "Large herbivores")))
  rw %<>% 
    transmute(rowname = From,
              key = To,
              value = log10(Estimate)) %>% 
    mutate(
      rowname = modify(rowname, function(x) {
        rename_tl2(x)
      }),
      key = modify(key, function(x) {
        rename_tl2(x)
      })) %>%
    mutate(rowname = factor(rowname, levels = c("Megacarnivores",
                                                "Large carnivores",
                                                "Small carnivores",
                                                "Small herbivores",
                                                "Large herbivores")))
  par(mfrow = c(1, 3),
      mar = c(1, 1, 1, 1))
  circos.clear()
  circos.par(start.degree = 100, 
             gap.after = c("Megacarnivores" = 5,
                           "Large carnivores" = 5,
                           "Small carnivores" = 20,
                           "Small herbivores" = 5,
                           "Large herbivores" = 20),
             points.overflow.warning = FALSE)
  chordDiagram(pn,
               order = levels(pn$rowname),
               big.gap = 20,
               small.gap = 5,
               grid.col = mycolor,
               transparency = 0.25,
               directional = 1,
               direction.type = c("diffHeight", "arrows"),
               link.arr.type = "big.arrow",
               annotationTrack = c("grid", "axis"), 
               annotationTrackHeight = mm_h(5))
  for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    circos.text(mean(xlim), 
                max(ylim) + 1.5,
                si, 
                sector.index = si, 
                track.index = 1, 
                facing = "bending.inside", 
                niceFacing = TRUE, 
                col = "black")
  }
  gap_cu <- calc_gap(pn, cu, big.gap = 20, small.gap = 5)
  circos.clear()
  circos.par(start.degree = 88, 
             gap.after = c("Megacarnivores" = 5,
                           "Large carnivores" = 5,
                           "Small carnivores" = 20 + gap_cu / 2,
                           "Small herbivores" = 5,
                           "Large herbivores" = 20 + gap_cu / 2),
             points.overflow.warning = FALSE)
  chordDiagram(cu, 
               order = levels(cu$rowname),
               grid.col = mycolor,
               transparency = 0.25,
               directional = 1,
               direction.type = c("diffHeight", "arrows"),
               link.arr.type = "big.arrow",
               annotationTrack = c("grid", "axis"), 
               annotationTrackHeight = mm_h(5))
  for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    circos.text(mean(xlim), 
                max(ylim) + 1.5,
                si, 
                sector.index = si, 
                track.index = 1, 
                facing = "bending.inside", 
                niceFacing = TRUE, 
                col = "black")
  }
  gap_rw <- calc_gap(pn, rw, big.gap = 20, small.gap = 5)
  circos.clear()
  circos.par(start.degree = 88, 
             gap.after = c("Megacarnivores" = 5,
                           "Large carnivores" = 5,
                           "Small carnivores" = 20 + gap_rw / 2,
                           "Small herbivores" = 5,
                           "Large herbivores" = 20 + gap_rw / 2),
             points.overflow.warning = FALSE)
  chordDiagram(rw, 
               order = levels(rw$rowname),
               grid.col = mycolor,
               transparency = 0.25,
               directional = 1,
               direction.type = c("diffHeight", "arrows"),
               link.arr.type = "big.arrow",
               annotationTrack = c("grid", "axis"), 
               annotationTrackHeight = mm_h(5))
  for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    circos.text(mean(xlim), 
                max(ylim) + 1.5,
                si, 
                sector.index = si, 
                track.index = 1, 
                facing = "bending.inside", 
                niceFacing = TRUE, 
                col = "black")
  }
  
  
  
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
        cex = 1.5
      )
      # Add graduation on axis
      circos.axis(
        h = "top", 
        major.at = seq(from = 0, to = xlim[2], 
                       by = ifelse(test = xlim[2] > 10, yes = 2, no = 1)), 
        minor.ticks = 1, 
        major.tick.percentage = 0.5,
        labels.niceFacing = FALSE,
        labels.cex = 1)
    }
  )
}

# data <- edge %>% 
#   filter(Scenario == "PN",
#          Area == "Protected area") %>% 
#   pivot_wider(names_from = To, values_from = Mean) %>% 
#   select(From, 9:11) %>% 
#   mutate_all("replace_na", 0) %>% 
#   mutate(From = factor(From))


mycolor <- c("#E1AF00", "#3B9AB2",
             "#EBCC2A", "#78B7C5",
             "#F21A00", "#00637c")

pdf("../../Manuscript/Figures/chordplot.pdf", width = 8, height = 4)
par(mfrow = c(1, 3))
plot_chord(edge, "PN", "Protected area")
text(0, 1, "No-extinction", font = 2, cex = 2)
plot_chord(edge, "CU", "Protected area")
text(0, 1, "Current", font = 2, cex = 2)
plot_chord(edge, "RW", "Protected area")
text(0, 1, "Rewilding", font = 2, cex = 2)
dev.off()


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
        text = element_text(size = 16),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 30,
                                   vjust = 0.5))

ggsave("../../Manuscript/Figures/raw_levels.pdf", width = 6.5, height = 8)
ggsave("../../Manuscript/Figures/raw_levels.png", width = 6.5, height = 8)
ggsave("../../Manuscript/Figures/raw_levels.svg", width = 6.5, height = 8)

