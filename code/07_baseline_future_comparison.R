## comparing baseline scenario to future scenarios

library(sf)
library(tidyverse)
library(viridis)
# install.packages("wesanderson")
library(wesanderson)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))

## upload elevation for raster mask
elev <- raster("input_data/Elev.tif")


# Upload data -------------------------------------------------------------

## scenario results
scenProbs <- read.csv("ignore/FuturePredictions/03_Av_Probs_Future_RB9_extremes_New.csv") %>%
  select(-c(X, X.1))
head(scenProbs)

## convert prob to binary
scenProbs2 <- scenProbs %>%
  mutate(FuturePresAbs = ifelse(MeanProb < 0.535, 0, 1)) %>%
  mutate(FuturePresAbs = factor(FuturePresAbs, levels = c("1","0"), labels = c("Presence", "Absence"))) %>%
  drop_na(FuturePresAbs, Scenario2)


head(scenProbs2)

# Comparison ---------------------------------------------------------------

## start simple - pairwise correlations with baseline

##spatial info 

sp_dat <- scenProbs2 %>%
  select(cells, x,y) %>%
  distinct()

## make wide

scenProbs2_wideCor <- scenProbs2 %>%
  select(-c(FuturePresAbs, Scenario, x, y)) %>%
  pivot_wider(names_from = Scenario2, values_from = MeanProb)
str(scenProbs2_wideCor)
cor(scenProbs2_wideCor)
## > 0.97
names(scenProbs2_wideCor)

## calculate difference in probability
## get baseline as column
scenProbs2x <- scenProbs2_wideCor %>%
  pivot_longer(c("Large Perturbations, wetter/hotter":"Amplified Extremes", "Hotter":"Large Perturbations, drier/hotter"), 
               names_to = "Scenario", values_to = "Probability")
scenProbs2x

## differences in probability
DiffFuture <- scenProbs2x %>%
  mutate(Diffs = Probability-Baseline) %>%
  mutate(Diffs2 = ifelse(Diffs <= -0.05, "Reduced", NA)) %>%
  mutate(Diffs2 = ifelse(Diffs >= 0.05, "Increased", Diffs2)) %>%
  mutate(Diffs2 = ifelse(is.na(Diffs2), "No Change", Diffs2)) %>%
  mutate(FuturePresAbs = ifelse(Probability < 0.535, 0, 1)) %>%
  mutate(FuturePresAbs = factor(FuturePresAbs, levels = c("1","0"), labels = c("Presence", "Absence"))) %>%
  mutate(BasePresAbs = ifelse(Baseline < 0.535, 0, 1)) %>%
  mutate(BasePresAbs = factor(BasePresAbs, levels = c("1","0"), labels = c("Presence", "Absence"))) %>%
  full_join(sp_dat, by = "cells", relationship = "many-to-many") 

DiffFuture
unique(DiffFuture$Scenario)
  

names(DiffFuture_sp)
## make spatial
DiffFuture_sp <- DiffFuture %>%
  st_as_sf(coords=c( "x", "y"), crs=crs(elev), remove=F)%>%
  mutate(Diffs2 = factor(Diffs2, levels = c("Increased","No Change", "Reduced"))) %>% ## change to factor and define levels
  mutate(Scenario = factor(Scenario, levels = c("Wetter","Drier",
                                                "Hotter","Amplified Extremes", "Small Perturbations, drier/hotter", "Large Perturbations, drier/hotter",
                                                "Small Perturbations, wetter/hotter", "Large Perturbations, wetter/hotter",
                                                "Small Perturbations, extremes/hotter", "Large Perturbations, extremes/hotter"),
                           labels = c("Wetter","Drier",
                                       "Hotter","Amplified Extremes", "Small Changes, dry/hot", "Large Changes, dry/hot",
                                       "Small Changes, wet/hot", "Large Changes, wet/hot",
                                       "Small Changes, extremes/hot", "Large Changes, extremes/hot")))


# DiffFuture_spSub <- DiffFuture_sp %>% 
#   filter(Scenario == "Hotter")
#   
# st_write(DiffFuture_spSub, "ignore/07_prob_occ_all_scens_diffs.shp", append = F)
## plot differences - future

## plot
m1 <- ggplot(data = DiffFuture_sp, aes(col=Diffs2), size = 0.5) +
  geom_sf() + ## differences in probability
  scale_color_discrete(name = "Direction of Change", type = c("#005AB5", "gray76", "#DC3220"))+
  facet_wrap(~Scenario) +
  theme_bw()+
  theme(strip.text = element_text(size = 13)) +
  theme(legend.text=element_text(size=17), legend.title=element_text(size=17)) +
  theme(axis.text=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=17)))+
  ggtitle("")

m1

file.name1 <- "Figures/07_change_in_probability_future_scens.jpg"
ggsave(m1, filename=file.name1, dpi=600, height=10, width=15)

## remove wetter, drier & amp extremes
DiffFuture_spx <- DiffFuture_sp %>%
  filter(!Scenario %in% c("Drier", "Wetter", "Amplified Extremes"))
unique(DiffFuture_sp$Scenario)

## plot
m2 <- ggplot(data = DiffFuture_spx, aes(col=Diffs2), size = 1) +
  geom_sf() + ## differences in probability
  scale_color_discrete(name = "Direction of Change", type = c("#005AB5", "gray76", "#DC3220"))+
  facet_wrap(~Scenario) +
  theme_bw()+
  theme(strip.text = element_text(size = 13)) +
  theme(legend.text=element_text(size=17), legend.title=element_text(size=17)) +
  theme(axis.text=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=17)))+
  ggtitle("")

m2

file.name1 <- "Figures/07_change_in_probability_future_scens_subset.jpg"
ggsave(m2, filename=file.name1, dpi=600, height=10, width=15)

## only wetter, drier & amp extremes
DiffFuture_sp1 <- DiffFuture_sp %>%
  filter(Scenario %in% c("Drier", "Wetter", "Amplified Extremes"))

unique(DiffFuture_sp$Scenario)

## plot
m3 <- ggplot(data = DiffFuture_sp1, aes(col=Diffs2), size = 1) +
  geom_sf() + ## differences in probability
  scale_color_discrete(name = "Direction of Change", type = c("#005AB5", "gray76", "#DC3220"))+
  facet_wrap(~Scenario) +
  theme_bw()+
  theme(strip.text = element_text(size = 13)) +
  theme(legend.text=element_text(size=17), legend.title=element_text(size=17)) +
  theme(axis.text=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=17)))+
  ggtitle("")

m3

file.name1 <- "Figures/07_change_in_probability_future_scens_appendix.jpg"
ggsave(m3, filename=file.name1, dpi=600, height=10, width=15)



# Sensitivity Analysis ----------------------------------------------------

## differences in probability
DiffFuture <- scenProbs2x %>%
  mutate(Diffs = Probability-Baseline) %>%
  mutate(Diffs2 = ifelse(Diffs <= -0.05, "Reduced", NA)) %>%
  mutate(Diffs2 = ifelse(Diffs >= 0.05, "Increased", Diffs2)) %>%
  mutate(Diffs2 = ifelse(is.na(Diffs2), "No Change", Diffs2)) %>%
  mutate(FuturePresAbs = ifelse(Probability < 0.535, 0, 1)) %>%
  mutate(FuturePresAbs = factor(FuturePresAbs, levels = c("1","0"), labels = c("Presence", "Absence"))) %>%
  mutate(BasePresAbs = ifelse(Baseline < 0.535, 0, 1)) %>%
  mutate(BasePresAbs = factor(BasePresAbs, levels = c("1","0"), labels = c("Presence", "Absence"))) %>%
  full_join(sp_dat, by = "cells", relationship = "many-to-many") 



# Differences in Predicted Presences ---------------------------------------

DiffFuture
## baseline range size
rSizebase <- DiffFuture %>%
  select(cells, Baseline, BasePresAbs) %>%
  filter(BasePresAbs == "Presence") %>% ## only presences
  distinct() %>%
  summarise(BaselinePresences = length(BasePresAbs)) 

rSizebase

## range sizes per scenario
rSize <- DiffFuture %>%
  filter(FuturePresAbs == "Presence") %>% ## only presences
  group_by(Scenario) %>%
  summarise(FuturePresences = length(FuturePresAbs)) %>%
  mutate(BaselinePresences = rSizebase$BaselinePresences) %>%
  mutate(DiffInRangeSize = BaselinePresences-FuturePresences) %>%
  mutate(DiffInRangeSizePerc = (DiffInRangeSize/BaselinePresences)*100)

rSize



## range overlap

rOverlap <- DiffFuture %>%
  filter(FuturePresAbs == "Presence") %>% ## only presences
  mutate(OverlapPred = ifelse(BasePresAbs == FuturePresAbs, 1, 0))  %>% 
  group_by(Scenario) %>%
  summarise(OverlapCount = sum(OverlapPred)) %>%
  mutate(BaselinePresences = rSizebase$BaselinePresences) %>%
  mutate(OverlapPerc = (OverlapCount/BaselinePresences)*100) %>%
  full_join(rSize, by = c("Scenario", "BaselinePresences"))

  
  
write.csv(rOverlap, "Tables/07_perc_diffs_range_size_overlap_scenarios.csv")


# Summary of differences --------------------------------------------------

DiffFuture

sumDiffs <- DiffFuture %>%
  group_by(Scenario) %>%
  summarise(meanProb = mean(Diffs),
            seProb = std(Diffs),
            maxProb = max(Diffs),
            minProb = min(Diffs))

sumDiffs

write.csv(sumDiffs, "Tables/07_prob_diffs_scenarios.csv")
