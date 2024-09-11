#### elevation of predictions
### predictions on protected land

library(raster)
library(randomForest)
library(sf)
library(tidyverse)
library(mapview)
library(tidylog)
library(cowplot)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))

# Upload data ------------------------------------------------------

## model with hydro
hYdMod <- read.csv("ignore/ModelResults/Gridded/02_Av_Probs_Current_RB9.csv")
mean(hYdMod$MeanProb) ## 0.3 - average predicted probability, ok to use as it's as good as more complex methods (Liu, 2005)
## 0.535 cut off maximising sensitivity and specificity (more conservative) but better - Liu 2005, 2010
head(hYdMod)

## raster results - probability
myModProb <- raster("ignore/ModelResults/Gridded/Arroyo_Toad_Prob_Occurrence_RB9.tif")
myModProb

## scenario results
# scenProbs <- read.csv("ignore/FuturePredictions/05_Av_Probs_Future_RB9_extremes.csv") %>%
#   select(-c(X, Index:deltaSeasonality))
# scenProbs


# ## observations
load(file=paste0("ignore/ModelResults/Gridded/Model1/all_presAbs_env_data.RData"))
head(NewDataObsSub)

## take just cells and presabs from obs 

obs <- NewDataObsSub %>%
  select(PresAbs, cells)

head(obs)

## upload elevation
elev <- raster("input_data/Elev.tif")

## stack elevation and predictions

alldataR <- stack(myModProb, elev)

## convert to data frame
alldataDF <- as.data.frame(alldataR, xy=T)
alldataDF <- na.omit(alldataDF)
dim(alldataDF) ##  16,021
## row names are cells, add as column

alldataDF$cells <- rownames(alldataDF)
alldataDF$cells <- as.numeric(alldataDF$cells)

## change names
names(alldataDF)

alldataDF <- alldataDF %>%
  rename(ProbOcc = Arroyo_Toad_Prob_Occurrence_RB9)

## join with obs

dataObs <- full_join(alldataDF, obs, by = "cells")


write.csv(dataObs, "ignore/04_prob_occ_current.csv")

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



file.name1 <- "Figures/04_observations_elevation.jpg"
ggsave(b1, filename=file.name1, dpi=300, height=5, width=8)



# elevation of predictions ------------------------------------------------

head(dataObs)

## convert prob to binary
dataObs2 <- dataObs %>%
  mutate(MyPresAbs = ifelse(ProbOcc < 0.535, 0, 1)) %>%
  mutate(MyPresAbs = factor(MyPresAbs, levels = c("1","0"), labels = c("Presence", "Absence"))) %>%
  # select(-c(MeanProb, Scenario2, Scenario)) %>% 
  distinct()

write.csv(dataObs2, "ignore/04_prob_occ_binary_current.csv")


## take only presence grids
PresOnly <- dataObs2 %>%
  filter(MyPresAbs == "Presence")

dataObs2
## box plot
b2 <- ggplot(dataObs2, aes(x=MyPresAbs, y = Elev, fill = MyPresAbs)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank()) +
  scale_x_discrete(name = "Scenario") +
  scale_y_continuous(name = "Elevation (m)") +
  theme_classic() +
  labs(fill = "")

b2

file.name1 <- "Figures/04_CURRENT_Predictions_elevation.jpg"
ggsave(b2, filename=file.name1, dpi=300, height=5, width=8)



# Upload protected land data ----------------------------------------------

## camp pendleton
Pend <- st_read("ignore/Protected_Land/Pendleton_95.shp") 
# plot(Pend)
names(Pend)
## open space lands that have been protected for open space uses through fee ownerships
Pland <- st_read("ignore/Protected_Land/CPAD_2023a_Holdings.shp", promote_to_multi = FALSE) 

## upload RB9 poly
cnts <- st_read("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/SD_RB9_boundary.shp") %>%
  st_transform(crs=st_crs(alldataR)) 

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

class(PlandP)

st_write(PlandP, "ignore/04_pendleton_protected_land_combined_spatial.shp", append = F)
sum(is.na(PlandP$UNIT_NAME)) ## 0

## join pendleton to protected land
# PlandP <- st_union(Pland, Pend)

plot(PlandP[2])

# raster data df 
## make spatial
dataObsSP <- dataObs2 %>%
  drop_na(x) %>%
  st_as_sf(coords=c( "x", "y"), crs=crs(alldataR), remove=F) 
dataObsSP

## make land use data same CRS
PlandP <- PlandP %>%
  st_transform(crs=st_crs(alldataR)) 

## check same crs
crs(PlandP) == crs(dataObsSP)

## join pland poly to point data

PlandDataObsJoin <- st_join(dataObsSP,PlandP)

## save out
st_write(PlandDataObsJoin, "ignore/Protected_Land/04_joined_land_and_mod_data.shp", append=F)

# PlandDataObsJoin <- st_read("ignore/Protected_Land/04_joined_land_and_mod_data.shp")

## take only presences from obs data

dataObsSP2 <- dataObsSP %>%
  filter(MyPresAbs == "Presence")

## add column of whether in protected land
PlandDataObsJoin <- PlandDataObsJoin %>%
  mutate(Protected = ifelse(is.na(UNIT_NAME), "No", "Yes"))

names(PlandDataObsJoin)
## convert to p/a

PlandDataObsJoin <- PlandDataObsJoin %>%
  mutate(MyPresAbs = ifelse(ProbOcc < 0.535, 0, 1)) %>%
  mutate(MyPresAbs = factor(MyPresAbs, levels = c("1","0"), labels = c("Presence", "Absence")))  

str(PlandDataObsJoin)

st_write(PlandDataObsJoin, "ignore/Protected_Land/04_joined_land_and_mod_data_binary.shp", append=F)

## get summary states of presences on protected land
sumScens <- PlandDataObsJoin %>% as.data.frame() %>%
  select(-c( geometry)) %>%
  filter(MyPresAbs == "Presence") %>% ## only presences
  group_by(Protected) %>%
  summarise(Presences = length(unique(cells))) %>%
  # ungroup(Protected) %>%
  mutate(TotalCells = sum(Presences))  %>%
  group_by(Protected) %>%
  mutate(Proportion = (Presences/TotalCells)*100) 

sumScens

write.csv(sumScens, "Tables/04_number_presences_on_prot_land_current_spatial.csv")

## make percentage table - presences
perctabP <- sumScens %>% as.data.frame() %>%
  select(-c(Presences, TotalCells)) %>%
  pivot_wider(names_from="Protected", values_from = "Proportion") %>%
  # pivot_wider(names_from="Protected", values_from = "ProportionA") %>%
  select(-No) %>%
  drop_na()
# pivot_wider(names_from = FuturePresAbs, values_from = Yes)

perctabP


write.csv(perctabP, "Tables/04_proportion_presences_on_prot_land_future_scens.csv")


# Map predicted presences on protected land -------------------------------

## upload RB9 poly
cnts <- st_read("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/SD_RB9_boundary.shp") %>%
  st_transform(crs=st_crs(alldataR)) 
cnts

## polygons of protected land
## join with pland
rb9pl <- st_join(PlandP, cnts, left = F)
rb9pl ## geom is the protected land
# plot(rb9pl[1])

## take only columns of interest and distinct rows, join with model data, and create presabs column based on 0.5 threshold
Plandx <- rb9pl %>%
  select(UNIT_ID, UNIT_NAME) %>%
  # filter(!UNIT_ID == 5778) %>%
  # distinct(UNIT_NAME, .keep_all =T) %>%
  st_join(dataObsSP) %>%
  drop_na(cells) 

## still some protected land outside of study area
names(PlandDataObsJoin)

## Presence Points
## filter from main df
presProt <- PlandDataObsJoin %>%
  filter(MyPresAbs == "Presence", Protected == "Yes") 

presNot <- PlandDataObsJoin %>%
  filter(MyPresAbs == "Presence", Protected == "No")
presNot

## upload ca boundary
ca_sf <- st_read("ignore/SpatialData/California/Ca_State_poly.shp")

## upload watersheds

sheds_sf<- st_read("ignore/SpatialData/SMCSheds2009/SMCSheds2009.shp") %>%
  filter(HUNAME %in% c("SAN JUAN", "SAN DIEGO", "SANTA MARGARITA", "SWEETWATER", "SAN DIEGUITO"))

# plot(sheds_sf)

## create beige palet

beige_pal<-c("#f2dbb7","#eed9c4","#fff0db","#e4d5b7","#d9b99b",
             "#d9c2ba","#9c8481","#e2cbb0","#a69279","#f2dbb7",
             "#f6e6bf","#a69279","#f2dbb7","#9c8481","#e4d5b7")

## map presences on protected land
m1 <- ggplot()+
  # geom_sf(data=ca_sf)+ ## california
  geom_sf(data=sheds_sf, aes(fill=SMC_Name), color=NA)+ ## watersheds
  scale_fill_manual(values=beige_pal, guide= "none") + ## beige colour for watersheds
  # geom_sf(data=cnts) + ## rb9 boundary
  geom_sf(data=Plandx, fill = "yellow") + ## protected land
  geom_sf(data=presNot, colour = "#DC3220", pch=1, size=1) +
  geom_sf(data=presProt, colour = "#005AB5", pch=1, size=1) +
  coord_sf(xlim=c(-117.82106,-116.4), ## axis limits
           ylim=c(32.5, 33.7),
           crs=4326) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.text=element_text(size=14)) 

m1

## create directory 
file.name1 <- paste("Figures/04_presences_on_protected_land_current.jpg")
## save
ggsave(m1, filename=file.name1, dpi=500, height=10, width=15)

### Upload critical habitat data ----------------------------------------------

CHab <- st_read("ignore/Critical_Habitat/ArroyoToadFinalCriticalHabitatUSFWSds129.shp")

## upload RB9 poly
cnts <- st_read("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/SD_RB9_boundary.shp") %>%
  st_transform(crs=st_crs(alldataR)) 

## make land use data same CRS
cnts <- cnts %>%
  st_transform(crs=st_crs(CHab)) 

## join with pland - subsets to rb9 region
CHab <- st_join(CHab, cnts, left = F)

# raster data df 
## make spatial
dataObsSP <- dataObs %>%
  drop_na(x) %>%
  st_as_sf(coords=c( "x", "y"), crs=crs(alldataR), remove=F) 

class(dataObsSP)

## make land use data same CRS
CHab <- CHab %>%
  st_transform(crs=st_crs(alldataR)) 

## check same crs
crs(CHab) == crs(dataObsSP)

## join pland poly to point data

CHabDataObsJoin <- st_join(dataObsSP,CHab) 
CHabDataObsJoin

## save out
st_write(CHabDataObsJoin, "ignore/Protected_Land/04_joined_critical_habitat_and_mod_data.shp", append=F)

## count presences in protected land

## add column of wehther in prootected ,and of=r no

CHabDataObsJoin <- CHabDataObsJoin %>%
  mutate(Critical = ifelse(is.na(SCINAME), "No", "Yes"))
CHabDataObsJoin
## convert to p/a

CHabDataObsJoin <- CHabDataObsJoin %>%
  mutate(MyPresAbs = ifelse(ProbOcc < 0.535, 0, 1)) %>%
  mutate(MyPresAbs = factor(MyPresAbs, levels = c("1","0"), labels = c("Presence", "Absence"))) 

st_write(CHabDataObsJoin, "ignore/Protected_Land/04_joined_critical_habitat_and_mod_data_binary.shp", append=F)

## get summary states of presences on critical habitat
sumScens <- CHabDataObsJoin %>% as.data.frame() %>%
  select(-geometry) %>%
  filter(MyPresAbs == "Presence") %>% ## only presences
  group_by(Critical) %>%
  summarise(Presences = length(unique(cells))) %>%
  # ungroup(Critical) %>%
  mutate(TotalCells = sum(Presences)) %>%
  group_by(Critical) %>%
  mutate(Proportion = (Presences/TotalCells)*100) 
sumScens

write.csv(sumScens, "Tables/04_number_presences_on_critical_habitat_current.csv")

## make percentage table - presences
perctabP <- sumScens %>% as.data.frame() %>%
  select(-c(Presences, TotalCells)) %>%
  pivot_wider(names_from="Critical", values_from = "Proportion") %>%
  # pivot_wider(names_from="Protected", values_from = "ProportionA") %>%
  select(-No) %>%
  drop_na()
# pivot_wider(names_from = FuturePresAbs, values_from = Yes)

perctabP

write.csv(perctabP, "Tables/04_proportion_presences_on_critical_habitat_future_scens.csv")


## counts
length(CHabDataObsJoin$cells) ## 16,021 grid cells
sum(CHabDataObsJoin$MyPresAbs == "Presence") ## 2119 predicted presence in study area
sum(CHabDataObsJoin$Critical == "Yes") ## 2845 points are in critical land in total
sum(CHabDataObsJoin$Critical == "Yes"& CHabDataObsJoin$MyPresAbs == "Presence") ## 1135 presences are in critical land in total
sum(na.omit(CHabDataObsJoin$Critical == "Yes"& CHabDataObsJoin$PresAbs == 1)) ## 496 observations are in critical land in total
# 
# cts <- as.data.frame(CHabDataObsJoin %>% 
#                        group_by(Critical) %>% 
#                        summarise(MyModPLPresence = sum(MyPresAbs == "Presence"),
#                                  MyModPLAbsence = sum(MyPresAbs == "Absence"),
#                                  Obs = sum(na.omit(PresAbs == 1))))
# 
# cts$geometry <- NULL
# cts
# 
# write.csv(cts, "Tables/04_critical_habitat_presences.csv")
# Critical MyModPLPresence        MyModPLAbsence Obs
# No            1183                      12567 544 
# Yes             936                     1335 423 

## 2119 predicted presences in study area
## 936 are on critical land - ~44%
## 1183 are not on critical land - ~56%

## 967 toad observations in total
## 544 are on critical land - ~ 56%
## 423 are not on critical land - ~44%

# Map presences on critical habitat ---------------------------------------

## upload RB9 poly
cnts <- st_read("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/SD_RB9_boundary.shp") %>%
  st_transform(crs=st_crs(alldataR)) 
cnts

## polygons of protected land
## join with CHab
rb9pl <- st_join(CHab, cnts, left = F)
rb9pl ## geom is the protected land

## take only columns of interest and distinct rows, join with model data, and create presabs column based on 0.5 threshold
CHabx <- rb9pl %>%
  # select(UNIT_ID, AGNCY_TYP) %>%
  # filter(!UNIT_ID == 5778) %>%
  # distinct(UNIT_ID, .keep_all =T) %>%
  st_join(dataObsSP) %>%
  drop_na(cells) #%>%
# mutate(MyPresAbs = ifelse(MyMod < 0.5, 0, 1)) %>%
# mutate(MyPresAbs = factor(MyPresAbs, levels = c("1","0"), labels = c("Presence", "Absence")))

CHabx 
length(unique(CHabx$UNIT)) ## 10 units of critical land in total


## Presence Points
## filter from main df
presProt <- CHabDataObsJoin %>%
  filter(MyPresAbs == "Presence", Critical == "Yes") 

presNot <- CHabDataObsJoin %>%
  filter(MyPresAbs == "Presence", Critical == "No")
presNot


# Maps --------------------------------------------------------------------

## map presences on protected land
m2 <- ggplot()+
  # geom_sf(data=ca_sf)+ ## california
  geom_sf(data=sheds_sf, aes(fill=SMC_Name), color=NA)+ ## watersheds
  scale_fill_manual(values=beige_pal, guide= "none") + ## beige colour for watersheds
  # geom_sf(data=cnts) + ## rb9 boundary
  geom_sf(data=CHabx, fill = "yellow") + ## critical habitat
  geom_sf(data=presNot, colour = "#DC3220", pch=1, size=1) +
  geom_sf(data=presProt, colour = "#005AB5", pch=1, size=1) +
  coord_sf(xlim=c(-117.82106,-116.4), ## axis limits
           ylim=c(32.5, 33.7),
           crs=4326) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.text=element_text(size=14)) 

m2

## create directory 
file.name1 <- paste("Figures/04_presences_on_critical_habitat_current.jpg")
## save
ggsave(m2, filename=file.name1, dpi=500, height=10, width=15)



# Map with critical habitat, protectedland and observ ations --------------
# install.packages('devtools')
# 
# library(devtools)
# devtools::install_github('oswaldosantos/ggsn')
# library(ggsn)
# unique(dataObs$PresAbs)

## filter to just obs
obsOnly <- dataObs %>%
  filter(PresAbs == 1) %>%
## make spatial
  drop_na(x) %>%
  st_as_sf(coords=c( "x", "y"), crs=crs(alldataR), remove=F) 


## map presences on critical habitat
m3 <- ggplot()+
  # geom_sf(data=ca_sf)+ ## california
  geom_sf(data=sheds_sf, aes(fill=SMC_Name), color=NA)+ ## watersheds
  scale_fill_manual(values=beige_pal, guide= "none") + ## beige colour for watersheds
  # geom_sf(data=obsOnly, pch=1, size=1) + ## observations
  geom_sf(data=CHabx, fill = "yellow") + ## critical habitat
  coord_sf(xlim=c(-117.82106,-116.4), ## axis limits
           ylim=c(32.5, 33.7),
           crs=4326) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.text=element_text(size=14)) 

m3

## create directory 
file.name1 <- paste("Figures/04_critical_habitat_current.jpg")
## save
ggsave(m3, filename=file.name1, dpi=500, height=10, width=15)

## map presences on protected land
m4 <- ggplot()+
  # geom_sf(data=ca_sf)+ ## california
  geom_sf(data=sheds_sf, aes(fill=SMC_Name), color=NA)+ ## watersheds
  scale_fill_manual(values=beige_pal, guide= "none") + ## beige colour for watersheds
  # geom_sf(data=obsOnly, pch=1, size=1) + ## observations
  geom_sf(data=Plandx, fill = "yellow") + ## critical habitat
  coord_sf(xlim=c(-117.82106,-116.4), ## axis limits
           ylim=c(32.5, 33.7),
           crs=4326) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.text=element_text(size=14)) 

m4

## create directory 
file.name1 <- paste("Figures/04_protected_land_habitat_current.jpg")
## save
ggsave(m4, filename=file.name1, dpi=500, height=10, width=15)

## map observations
m5 <- ggplot()+
  # geom_sf(data=ca_sf)+ ## california
  geom_sf(data=sheds_sf, aes(fill=SMC_Name), color=NA)+ ## watersheds
  scale_fill_manual(values=beige_pal, guide= "none") + ## beige colour for watersheds
  geom_sf(data=obsOnly) + ## observations
  # geom_sf(data=Plandx, fill = "yellow") + ## critical habitat
  coord_sf(xlim=c(-117.82106,-116.4), ## axis limits
           ylim=c(32.5, 33.7),
           crs=4326) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.text=element_text(size=14)) 

m5

## create directory 
file.name1 <- paste("Figures/04_observations.jpg")
## save
ggsave(m5, filename=file.name1, dpi=500, height=10, width=15)

## plot together
allMaps <- plot_grid(m3,m4,m5)
## create directory 
file.name1 <- paste("Figures/04_ALL_ON_ONE_CH_PL_OBS.jpg")
## save
ggsave(allMaps, filename=file.name1, dpi=500, height=10, width=15)

