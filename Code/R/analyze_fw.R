library(tidyverse)

setwd("/home/GIT/Trophic_restoration/Results/Webs")

web <- read_csv("Web210.csv", col_types = cols()) %>% 
  filter(Predator != Prey)



web %>% 
  mutate(`TL` = modify(Predator, function(x){
    if(x %in% web$Prey){
      return("Meso")
    } else{
      return("Apex")
    }
  })) %>% 
  group_by(TL, Scenario) %>% 
  dplyr::select(Predator, TL, Scenario) %>% 
  unique() %>% 
  tally() %>% 
  mutate(Scenario = factor(Scenario, levels = c("PN", "CU"))) %>% 
  ggplot() +
  geom_point(aes(Scenario, n, col = TL)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    panel.grid = element_line(colour = "gainsboro")
  ) +
  ylab("Number of species")

# herbivore_n

# calculate vulnerability
vulnerability <- web %>% 
  mutate(D.Vuln.PN.CU = modify(Prey, function(x) web %>% filter(Scenario == "PN", Prey == x) %>% nrow() - web %>% filter(Scenario == "CU", Prey == x) %>% nrow())) %>% 
  dplyr::select(Prey, D.Vuln.PN.CU) %>% 
  unique() 

#### This to analysis script ----
par(mfrow = c(1, 2))
h1 <- hist(web_pn$EDi, breaks = seq(0, ceiling(max(web_pn$EDi) / 10) * 10, by = 5), 
           probability = F, add = F, col = "steelblue")
E <- h1$counts
h2 <- hist(web_current$EDi, breaks = seq(0, ceiling(max(web_pn$EDi) / 10) * 10, by = 5), 
           probability = F, add = F, col = "tomato")
O <- h2$counts

chisq.test(data.frame(E, O) %>% filter(E > 4 & O > 4))

ks.test(web_pn$EDi, web_current$EDi)

boot <- sapply(1:1000, function(x) 
  wilcox.test(web_pn$EDi[sample(nrow(web_pn), 100)], 
              web_current$EDi[sample(nrow(web_current), 100)])$p.value) 
round(sum(boot > 0.05) / 10)

#### This to figure script ----
# Figure of food webs
new <- web_current %>%
  anti_join(web_pn)

lost <- web_pn %>% 
  anti_join(web_current)

# random model testing
h <- hist(web_pn$EDi, breaks = seq(0, ceiling(max(web_pn$EDi) / 10) * 10, by = 5), 
          probability = T, add = F, col = "steelblue")
E <- h$density
mids <- h$mids
h <- hist(lost$EDi, breaks = seq(0, ceiling(max(web_pn$EDi) / 10) * 10, by = 5), 
          probability = T, add = F, col = "tomato")
O <- h$density
if(!all(mids == h$mids)){
  error("Different mid-points")
}
par(mfrow = c(1, 1))

chi <- sum((E - O)^2 / E, na.rm = T)
pchisq(chi, length(E) - 1, lower.tail = F)

chisq.test(table(c(rep("E", length(E)), rep("O", length(O))), c(E, O)))

web_current %>% 
  mutate(Extinct = modify2(Predator, Prey, function(x, y){
    if(get_mass(x)$IUCN %in% c("VU", "CR", "EN") | get_mass(y)$IUCN %in% c("VU", "CR", "EN")){
      return(T)
    }else{
      return(F)
    }
  })) %>% 
  filter(Extinct == F) -> web_future

# some tests
ks.test(web_pn$EDi, web_current$EDi)

table(c(rep("CU", nrow(web_current)), rep("PN", nrow(web_pn))), c(web_current$EDi, web_pn$EDi)) %>% 
  chisq.test()

web_current %>% 
  mutate(Type = "CU") %>% 
  full_join(web_pn %>% mutate(Type = "PN")) %>% 
  wilcox.test(EDi ~ Type, data = .)

web_current %>% 
  mutate(Type = "CU") %>% 
  full_join(web_pn %>% mutate(Type = "PN")) %>% 
  mutate(Type = factor(Type, levels = c("PN", "CU"))) %>% 
  ggplot(aes(EDi, fill = Type)) +
  geom_histogram(aes(y = ..count..), col = "black", position = "identity", alpha = 0.5, binwidth = 5)
