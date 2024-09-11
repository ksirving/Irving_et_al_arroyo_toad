#### elevation of predictions
### predictions on protected land

library(raster)
library(randomForest)
library(sf)
library(mapview)
library(cowplot)
library(tidylog)
library(tidyverse)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))

# Upload data ------------------------------------------------------

## model with hydro
# hYdMod <- read.csv("ignore/ModelResults/Gridded/03_Av_Probs_Current_RB9.csv")
# mean(hYdMod$MeanProb) ## 0.3 - average predicted probability, ok to use as it's as good as more complex methods (Liu, 2005)
# ## 0.535 cut off maximising sensitivity and specificity (more conservative) but better - Liu 2005, 2010?
# head(hYdMod)

## raster results - probability
# myModProb <- raster("ignore/ModelResults/Gridded/Arroyo_Toad_Prob_Occurrence_RB9.tif")
# myModProb

## future data - rename scenarios, drop scenarios not needed
newData <- read.csv("ignore/2024-08-27_RFpred_output_alldata_rb9future26yr_redo_med_dlt_FFM_test12_test2_scaleraw_capT.csv") %>%
  rename(Scenario = scenario) %>%
  mutate(Scenario2 = case_when(Scenario == 21 ~ "Baseline",
                               Scenario == 3 ~ "Drier",
                               Scenario == 39 ~ "Wetter",
                               Scenario == 27 ~ "Hotter",
                               Scenario == 19 ~ "Amplified Extremes",
                               Scenario == 32 ~ "Small Perturbations, drier/hotter",
                               Scenario == 7 ~ "Large Perturbations, drier/hotter",
                               Scenario == 100 ~ "Large Perturbations, wetter/hotter",
                               Scenario == 101 ~ "Small Perturbations, wetter/hotter",
                               Scenario == 102 ~ "Large Perturbations, extremes/hotter",
                               Scenario == 103 ~ "Small Perturbations, extremes/hotter")) %>%
  rename(COMID = comid) %>%
  drop_na(Scenario2) 

sum(is.na(newData))

## upload comids and cells 
coms <- read.csv("ignore/03_comids_cells_to_join_New.csv")
coms
## join with new data

newDataComs <- full_join(coms, newData, by = "COMID", relationship = "many-to-many") %>%
  # rename(Scenario = scenario) %>%
  select(-X)

head(newDataComs)

## scenario results
scenProbs <- read.csv("ignore/FuturePredictions/03_Av_Probs_Future_RB9_extremes_New.csv")%>%
  select(-c(X,-X.1)) %>%
  inner_join(newDataComs, by = c("cells","Scenario", "Scenario2"), multiple = "all") ## join with scenario probabilities

head(scenProbs)
sum(is.na(scenProbs))

length(unique(scenProbs$cells)) ## 15,993

# ## observations
load(file=paste0("ignore/ModelResults/Gridded/Model1/all_presAbs_env_data.RData"))
head(NewDataObsSub)

## take just cells and presabs from obs 

obs <- NewDataObsSub %>%
  select(PresAbs, cells)

head(obs)

## upload elevation
elev <- raster("input_data/Elev.tif")

## convert to data frame
alldataDF <- as.data.frame(elev, xy=T)
alldataDF <- na.omit(alldataDF)
dim(alldataDF) ##  18,244

## row names are cells, add as column

alldataDF$cells <- rownames(alldataDF)
alldataDF$cells <- as.numeric(alldataDF$cells)

## change names
names(alldataDF)

# alldataDF <- alldataDF %>%
#   rename(ProbOcc = Arroyo_Toad_Prob_Occurrence_RB9)

## join with obs

dataObs <- full_join(alldataDF, obs, by = "cells") %>%
  distinct(cells, .keep_all = T)

length(unique(dataObs$cells)) ## 16017
head(dataObs)
# sum(dataObs$ProbOcc > 0.535) ## 2119

dataObs <- full_join(dataObs, scenProbs, by = c("cells")) %>%
  select(-c(x.x, y.x)) %>%
  rename(x = x.y, y = y.y)

head(dataObs)
sum(is.na(dataObs))

## convert prob to binary
dataObs2 <- dataObs %>%
  select(-d_peak_5) %>% ## remove peak 5
  mutate(FuturePresAbs = ifelse(MeanProb < 0.535, 0, 1)) %>%
  mutate(FuturePresAbs = factor(FuturePresAbs, levels = c("1","0"), labels = c("Presence", "Absence"))) %>%
  drop_na(FuturePresAbs, Scenario) %>%
  rename(DS_Mag_50 = d_ds_mag_50,
         FA_Mag = d_fa_mag,
         Peak_10 = d_peak_10,
         Peak_2 = d_peak_2,
         SP_Mag = d_sp_mag,
         Wet_BFL_Mag_10 = d_wet_bfl_mag_10,
         Wet_BFL_Mag_50 = d_wet_bfl_mag_50,
         Q99 = delta_q99) %>%
  pivot_longer(c(Elev, DS_Mag_50:Q99), names_to = "Metric", values_to = "Values") %>% ## change variable names
  mutate(VariableHuman = case_when(Metric == "DS_Mag_50" ~ "Dry Season Baseflow",
                                   Metric == "Peak_10" ~ "Peak Flow: 10-Year Flood",
                                   Metric == "FA_Mag" ~ "Fall Pulse",
                                   Metric == "Peak_2" ~ "Peak Flow: 2-Year Flood",
                                   Metric == "Q99" ~ "Largest Annual Storm",
                                   Metric == "SP_Mag" ~ "Spring Recession",
                                   Metric == "Wet_BFL_Mag_10" ~ "Wet Season Baseflow (Low)",
                                   Metric == "Wet_BFL_Mag_50" ~ "Wet Season Baseflow (Med)",
                                   Metric == "Elev" ~ "Elevation")) %>%
  mutate(ValuesMS = ifelse(Metric == "Elev", Values, Values*0.0283168))
  
head(dataObs2)
# elevation of observations -----------------------------------------------

## keep only 1 and 0 (remove NAs) from presence absence
dataObsx <- dataObs %>%
  drop_na(PresAbs) %>%
  mutate(PresAbs = factor(PresAbs, levels = c("1","0"), labels = c("Presence", "Absence")))

# ?as.factor

b1 <- ggplot(dataObsx, aes(x=PresAbs, y = Elev, fill = PresAbs)) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Elevation (ft)") +
  theme_classic() +
  labs(fill = "")

b1

file.name1 <- "Figures/06_observations_elevation.jpg"
ggsave(b1, filename=file.name1, dpi=300, height=5, width=8)



# ffm and elevation of predictions ------------------------------------------------

#### summary stats on FFM and elevation

sumStats <- dataObs2 %>%
  group_by(FuturePresAbs, Scenario2, VariableHuman) %>%
  summarise(meanVal = round(mean(ValuesMS), digits = 2),
            seVal = round(std(ValuesMS), digits = 2),
            maxVal = max(ValuesMS),
            minVal = min(ValuesMS)) %>%
  mutate(Mean_std = paste(meanVal, " +/- ", seVal)) 

sumStats

write.csv(sumStats, "Tables/06_elevation_FFM_sum_stats.csv")


sumStatsx <- sumStats %>%
  filter(FuturePresAbs == "Presence", !VariableHuman == "Elevation") %>%
  select(Scenario2, VariableHuman, Mean_std) %>%
  pivot_wider(names_from = VariableHuman, values_from = Mean_std)

sumStatsx

write.csv(sumStatsx, "Tables/06_elevation_FFM_sum_stats_wide.csv")

## compare presence only elevation with wilcoxon test - wilcox.test()

## take only presence grids
PresOnly <- dataObs2 %>%
  filter(FuturePresAbs == "Presence") 
  
head(PresOnly)

## get baseline for comparison
basel <- PresOnly %>%
  filter(Scenario2 == "Baseline") %>%
  select(cells, MeanProb, Metric, Values)

## define scenarios
scens <- unique(PresOnly$Scenario2)[-1]
scens
scens[1]
t=5
## define Metrics
mets <- unique(PresOnly$Metric)
mets

## empty dataframe
df <- data.frame(matrix(ncol = 7))
colnames(df) <- c("Metric", "T_Statistic", "PValue", "DegreesOfFreedom", "nBaseline", "nFuture", "Scenario")

dfx <- NULL

for(m in 1:length(mets)) {
  
  PresOnlyx <- PresOnly %>%
    filter(Metric == mets[m])

  baselx <- basel %>%
    filter(Metric == mets[m]) 

  
  for(t in 1:length(scens)) {
    
    futscen <- PresOnlyx %>%
      filter(Scenario2 == scens[t]) %>%
      select(cells, MeanProb, Metric, Values) %>%
      rename(FutureMeanProb = MeanProb,
             FutureVal = Values) 
    
    ## join baseline and future elevation
    
    dat <- full_join(baselx, futscen, by = c("cells")) 
    
    # wtest <- wilcox.test(dat$Elev, dat$FutureElev)
    
    ## t test with log transformed elevation
    ttest <- t.test(log(dat$Values), log(dat$FutureVal))
   # ttest
    ## get stats
    df[t,1] <- mets[m]
    df[t,2] <- ttest$statistic
    df[t,3] <- ttest$p.value
    df[t,4] <- ttest$parameter
    df[t,5] <- length(na.omit(dat$Values))
    df[t,6] <- length(na.omit(dat$FutureVal))
    df[t,7] <- scens[t]
    
    dfx <- bind_rows(dfx, df)
    
  
    }
  
}

ttest$statistic
ttest$parameter

df <- dfx %>%
  distinct()

df$PValueR <- round(df$PValue, digits=5)

write.csv(df, "Tables/06_elevation_ffm_ttests.csv")

head(dataObs2)

df <- read.csv("Tables/06_elevation_ffm_ttests.csv") %>%
  rename(Scenario2 = Scenario) %>% select(-X) 
head(df)

names(dataObs2)
head(dataObs2)

## take only presence grids
PresOnly <- dataObs2 %>%
  full_join(df, by = c("Scenario2", "Metric")) %>%
  filter(FuturePresAbs == "Presence") %>%
  filter(!Metric == "Elev") %>% 
  mutate(pval_star = case_when(PValue < 0.01 ~ "**",
                               PValue < 0.05 ~ "*",
                               .default = ""))
unique(PresOnly$Metric)
# remove outliers to get whisker length for significant **

## function
filter_lims <- function(x){
  l <- boxplot.stats(x)$stats[1]
  u <- boxplot.stats(x)$stats[5]
  
  for (i in 1:length(x)){
    x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
  }
  return(x)
}

## replace outliers with NA
PresOnlyx <- PresOnly %>%
  group_by(Scenario2, VariableHuman) %>%
  mutate(ValuesMS2 = filter_lims(ValuesMS)) %>%
  mutate(Scenario2 = factor(Scenario2, levels = c("Baseline", "Wetter","Drier", "Hotter","Amplified Extremes", "Small Perturbations, drier/hotter", "Large Perturbations, drier/hotter",
                                                  "Small Perturbations, wetter/hotter", "Large Perturbations, wetter/hotter",
                                                    "Small Perturbations, extremes/hotter", "Large Perturbations, extremes/hotter")))

# lims_y <- c(0.4,1.5,18,150,30,41,0.4, 0.75)
str(PresOnlyx)

## box plot for paper
b2 <- ggplot(PresOnlyx, aes(x=Scenario2, y = ValuesMS2, group = Scenario2, fill = Scenario2)) +
  geom_boxplot(outliers = FALSE) + ## removes outliers
  theme(axis.text.x = element_blank()) +
  facet_wrap(~VariableHuman, scales = "free_y") +
  scale_x_discrete(labels = NULL, name = "") +
  scale_y_continuous(name = expression(paste("∆ Flow Magnitude (m"^3~"/s)")), expand = expansion(mult = c(0.1, 0.2))) +  # Adjust the expand values +
  theme_classic() +
  theme(legend.position = "bottom", 
        strip.text = element_text(size=15),
        axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text=element_text(size=15)) +
  labs(fill = "") +
  # geom_text(aes(label = pval_star), nudge_y = 0.05)
  stat_summary(
    aes(label = pval_star), fun = function(x) max(x),
    geom = "text", vjust = -0.5, size = 4, col = "red"
  )

b2

file.name1 <- "Figures/06_Predictions_FFM.jpg"
ggsave(b2, filename=file.name1, dpi=600, height=8, width=14)

## boxplot with outliers

b3 <- ggplot(PresOnlyx, aes(x=Scenario2, y = ValuesMS, group = Scenario2, fill = Scenario2)) +
  geom_boxplot() + 
  theme(axis.text.x = element_blank()) +
  facet_wrap(~VariableHuman, scales = "free_y") +
  scale_x_discrete(labels = NULL, name = "") +
  scale_y_continuous(name = expression(paste("∆ Flow Magnitude (m"^3~"/s)")), expand = expansion(mult = c(0.1, 0.2))) +  # Adjust the expand values +
  theme_classic() +
  theme(legend.position = "bottom", 
        strip.text = element_text(size=15),
        axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text=element_text(size=15)) +
  labs(fill = "") +
  # geom_text(aes(label = pval_star), nudge_y = 0.05)
  stat_summary(
    aes(label = pval_star), fun = function(x) max(x),
    geom = "text", vjust = -0.5, size = 4, col = "red"
  )

b3

file.name1 <- "Figures/06_Predictions_FFM_with_outliers.jpg"
ggsave(b3, filename=file.name1, dpi=600, height=8, width=14)


# Upload protected land data ----------------------------------------------

## camp pendleton
Pend <- st_read("ignore/Protected_Land/Pendleton_95.shp") 
# plot(Pend)
names(Pend)
## open space lands that have been protected for open space uses through fee ownerships
Pland <- st_read("ignore/Protected_Land/CPAD_2023a_Holdings.shp", promote_to_multi = FALSE) 

## upload RB9 poly
cnts <- st_read("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/SD_RB9_boundary.shp") 

## make land use data same CRS
cnts <- cnts %>%
  st_transform(crs=st_crs(Pland)) 

## join with pland - subsets to rb9 region
Pland <- st_join(Pland, cnts, left = F)
names(Pland)


## check crs of pend and pro land
st_crs(Pend) == st_crs(Pland)

## transform crs of pend
Pend <- Pend %>%
  st_transform(crs=st_crs(Pland)) 

## remove unwanted columns
PlandSub <- Pland %>%
  # as.data.frame() %>%
  filter(!UNIT_ID == 5778) %>%
  dplyr::select(UNIT_ID, UNIT_NAME, AREA, RB, RBNAME, geometry)

str(PlandSub)

## rename columnd to match protected land
PendSub <- Pend %>%
  # as.data.frame() %>%
  rename(UNIT_ID = lu, UNIT_NAME = Landuse, AREA = area) %>%
  dplyr::select(-len, - GenLU) %>% ## remove irrelevant columns
  mutate(RB = 9, RBNAME = "San Diego")
head(PendSub)
str(PendSub)

## join pendleton to protected land by rows
PlandP <- bind_rows(PendSub, PlandSub)
PlandP
sum(is.na(PlandP$UNIT_NAME)) ## 0

head(dataObs2)
# raster data df 
## make spatial
dataObsSP <- dataObs2 %>%
  drop_na(x) %>%
  st_as_sf(coords=c( "x", "y"), crs=crs(elev), remove=F) 
dataObsSP
crs(elev)
crs(Pland)
## make land use data same CRS
PlandP <- PlandP %>%
  st_transform(crs=st_crs(elev)) 

## check same crs
crs(PlandP) == crs(dataObsSP)

## join pland poly to point data

PlandDataObsJoin <- st_join(dataObsSP,PlandP)

## take only presences from obs data

dataObsSP2 <- dataObsSP %>%
  filter(FuturePresAbs == "Presence")

## add column of whether in protected land
PlandDataObsJoinx <- PlandDataObsJoin %>%
  mutate(Protected = ifelse(is.na(UNIT_NAME), "No", "Yes"))

names(PlandDataObsJoin)

## get summary states of presences on protected land
sumScens <- PlandDataObsJoinx %>% as.data.frame() %>%
  select(-geometry) %>%
  filter(FuturePresAbs == "Presence") %>% ## only presences
  group_by(Scenario2, Protected) %>%
  summarise(Presences = length(unique(cells))) %>%
  ungroup(Protected) %>%
  mutate(TotalCells = sum(Presences)) %>%
  group_by(Protected) %>%
  mutate(Proportion = (Presences/TotalCells)*100) 

sumScens

write.csv(sumScens, "Tables/06_number_presences_on_prot_land_future_scens_spatial.csv")

sumScensWide <- sumScens %>%
  select(-Proportion) %>%
  pivot_wider(names_from = Protected, values_from = Presences)

sumScensWide

write.csv(sumScensWide, "Tables/06_number_presences_on_critical_habitat_future_scens_wide.csv")


## make percentage table - presences
perctabP <- sumScens %>% as.data.frame() %>%
  select(-c( TotalCells, Presences)) %>%
  pivot_wider(names_from="Protected", values_from = "Proportion") %>%
  # pivot_wider(names_from="Protected", values_from = "ProportionA") %>%
  # select(-No) %>%
  drop_na()
  # pivot_wider(names_from = FuturePresAbs, values_from = Yes)

perctabP


write.csv(perctabP, "Tables/06_proportion_presences_on_prot_land_future_scens.csv")

## same for pendleton
# PendDataObsJoin <- PendDataObsJoin %>%
#   mutate(Protected = ifelse(is.na(Landuse), "No", "Yes"))
# 
# PendDataObsJoin
# 
# sum(PendDataObsJoin$MyPresAbs == "Presence") ## 132993
## convert to p/a
# 
# PlandDataObsJoin <- PlandDataObsJoin %>%
#   mutate(MyPresAbs = ifelse(ProbOcc < 0.535, 0, 1)) %>%
#   mutate(MyPresAbs = factor(MyPresAbs, levels = c("1","0"), labels = c("Presence", "Absence"))) %>%
#   mutate(FuturePresAbs = ifelse(MeanProb < 0.535, 0, 1)) %>%
#   mutate(FuturePresAbs = factor(FuturePresAbs, levels = c("1","0"), labels = c("Presence", "Absence"))) 

## get summary states of presences on protected land
# sumScens <- PendDataObsJoin %>% as.data.frame() %>%
#   select(-c( geometry)) %>%
#   # filter(FuturePresAbs == "Presence") %>% ## only presences
#   group_by(Scenario2, Protected, FuturePresAbs) %>%
#   summarise(Presences = length(FuturePresAbs)) %>%
#   pivot_wider(names_from = "FuturePresAbs", values_from = "Presences") %>%
#   ungroup(Protected) %>%
#   mutate(TotalCellsP = sum(Presence), TotalCellsA = sum(Absence)) %>%
#   group_by(Protected) %>%
#   mutate(ProportionP = (Presence/TotalCellsP)*100) %>%
#   mutate(ProportionA = (Absence/TotalCellsA)*100) 
# 
# sumScens
# 
# write.csv(sumScens, "Tables/06_number_presences_pendleton_future_scens_spatial.csv")
# 
# ## make percentage table - presences
# perctabP <- sumScens %>% as.data.frame() %>%
#   select(-c(Presence, Absence, TotalCellsP,  TotalCellsA, ProportionA)) %>%
#   pivot_wider(names_from="Protected", values_from = "ProportionP") %>%
#   # pivot_wider(names_from="Protected", values_from = "ProportionA") %>%
#   select(-No) %>%
#   drop_na()
# # pivot_wider(names_from = FuturePresAbs, values_from = Yes)
# 
# perctabP
# 
# ## absences
# perctabA <- sumScens %>% as.data.frame() %>%
#   select(-c(Presence, Absence, TotalCellsP,  TotalCellsA, ProportionP)) %>%
#   pivot_wider(names_from="Protected", values_from = "ProportionA") %>%
#   # pivot_wider(names_from="Protected", values_from = "ProportionA") %>%
#   select(-No) %>%
#   drop_na()
# # pivot_wider(names_from = FuturePresAbs, values_from = Yes)
# 
# perctabA
# 
# write.csv(perctabP, "Tables/06_proportion_presences_on_prot_land_future_scens.csv")


### Upload critical habitat data ----------------------------------------------
  
CHab <- st_read("ignore/Critical_Habitat/ArroyoToadFinalCriticalHabitatUSFWSds129.shp")
plot(CHab)
CHab
## make land use data same CRS
cnts <- cnts %>%
  st_transform(crs=st_crs(CHab)) 

## join with pland - subsets to rb9 region
rb9CH <- st_join(CHab, cnts, left = F)
rb9CH ## geom is the critical habitat

head(Pland)
unique(rb9CH$RBNAME)

# raster data df 

## make spatial
dataObsSP <- dataObs2 %>%
  drop_na(x) %>%
  st_as_sf(coords=c( "x", "y"), crs=crs(elev), remove=F) 

class(dataObsSP)

## make land use data same CRS
rb9CH <- rb9CH %>%
  st_transform(crs=st_crs(elev)) 

## check same crs
crs(rb9CH) == crs(dataObsSP)

## join pland poly to point data
CHabDataObsJoin <- st_join(dataObsSP,rb9CH) 
CHabDataObsJoin
sum(CHabDataObsJoin$MyPresAbs == "Presence") ## 132993

length(unique(CHabDataObsJoin$cells))
length(CHabDataObsJoin$cells)


## add column of wehther in protected or not

CHabDataObsJoin <- CHabDataObsJoin %>%
  mutate(Critical = ifelse(is.na(UNIT), "No", "Yes"))

CHabDataObsJoin

## get summary states of presences on protected land
sumScens <- CHabDataObsJoin %>% as.data.frame() %>%
  select(-geometry) %>%
  filter(FuturePresAbs == "Presence") %>% ## only presences
  group_by(Scenario2, Critical) %>%
  summarise(Presences = length(unique(cells))) %>%
  ungroup(Critical) %>%
  mutate(TotalCells = sum(Presences)) %>%
  group_by(Critical) %>%
  mutate(Proportion = (Presences/TotalCells)*100) 
sumScens

write.csv(sumScens, "Tables/06_number_presences_on_critical_habitat_future_scens.csv")

sumScensWide <- sumScens %>%
  select(-Proportion) %>%
  pivot_wider(names_from = Critical, values_from = Presences)

sumScensWide

write.csv(sumScensWide, "Tables/06_number_presences_on_critical_habitat_future_scens_wide.csv")

## make percentage table 
perctab <- sumScens %>% as.data.frame() %>%
  select(-c(Presences, TotalCells)) %>%
  pivot_wider(names_from="Critical", values_from = "Proportion")

perctab

write.csv(perctab, "Tables/06_proportion_presences_on_critical_habitat_future_scens.csv")


