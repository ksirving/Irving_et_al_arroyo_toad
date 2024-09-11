### current analysis figures

library(viridisLite)
library(tmaptools)
library(RColorBrewer)
library(viridis)
library(cowplot)

library(sf)
library(tidyverse)
library(tidylog)


# Data --------------------------------------------------------------------

## upload elevation for crs
elev <- raster("input_data/Elev.tif")

## upload ca boundary
ca_sf <- st_read("ignore/SpatialData/California/Ca_State_poly.shp")

## upload ca counties
counties_sf <- st_read("ignore/SpatialData/Counties/Counties.shp") %>%
  filter(NAME_PCASE == "San Diego")

## upload watersheds

sheds_sf<- st_read("ignore/SpatialData/SMCSheds2009/SMCSheds2009.shp") %>%
  filter(HUNAME %in% c("SAN JUAN", "SAN DIEGO", "SANTA MARGARITA", "SWEETWATER", "SAN DIEGUITO"))

## create beige palet

beige_pal<-c("#f2dbb7","#eed9c4","#fff0db","#e4d5b7","#d9b99b")

## prob oiccurences with pres/abs
dataObs <- read.csv("ignore/04_prob_occ_current.csv")

## make spatial
dataObsSP <- dataObs %>%
  drop_na(x) %>%
  st_as_sf(coords=c( "x", "y"), crs=crs(elev), remove=F) 

## upload RB9 poly
cnts <- st_read("ignore/SpatialData/SD_RB9_boundary.shp") %>%
  st_transform(crs=st_crs(elev))


# probability of occurrence -----------------------------------------------

## load observations
load(file = "ignore/ModelResults/Gridded/02_obs_probs_current.RData")

## load probabilities
probs_sf <- st_read("ignore/ModelResults/Gridded/Arroyo_Toad_Prob_Occurrence_RB9.shp")

## observations
## change to wg84 fo map,
obs_wg84 <- obs %>%
  st_transform(crs = 4326) 

## make x/y columns
obs_wg84 <- obs_wg84 %>%
  mutate(x = st_coordinates(obs_wg84)[,1], y = st_coordinates(obs_wg84)[,2]) %>%
  filter(PresAbs == 1)

## Predictions
## change to wg84 for  map, create columns of x & y 
probsWG84 <- probs_sf %>%
  ungroup() %>%
  st_transform(crs = 4326) 

## make x/y columns
probsWG84 <- probsWG84 %>%
  mutate(x = st_coordinates(probsWG84)[,1], y = st_coordinates(probsWG84)[,2]) 

## set bounding box 
st_bbox(probsWG84)

## create beige palet

beige_pal<-c("#f2dbb7","#eed9c4","#fff0db","#e4d5b7","#d9b99b")

## map predictions
m1 <- ggplot()+
  geom_sf(data=ca_sf)+ ## california
  geom_sf(data=sheds_sf, aes(fill=SMC_Name), color=NA)+ ## watersheds
  scale_fill_manual(values=beige_pal, guide= "none")+ ## beige colour for watersheds
  # geom_sf(data=counties_socal_sf, fill=NA)+ ## counties
  geom_sf(data=probsWG84, aes(col = MeanProb), size = 1) + ## results
  scale_color_viridis(name = "Probability of Occurrence", option="mako", direction = -1) +
  coord_sf(xlim=c(-117.82106,-116.4), ## axis limits
           ylim=c(32.5, 33.7),
           crs=4326) +
  theme_bw() +
  theme(axis.text=element_text(size=20)) +
  theme(legend.text=element_text(size=20), legend.title = 
          element_text(size=20))


m1

## create directory 
file.name1 <- paste("Figures/05_probability_of_occurence_current.jpg")
## save
ggsave(m1, filename=file.name1, dpi=600, height=10, width=15)



# Critical Habitat --------------------------------------------------------

CHab <- st_read("ignore/Critical_Habitat/ArroyoToadFinalCriticalHabitatUSFWSds129.shp")

## make land use data same CRS
cnts <- cnts %>%
  st_transform(crs=st_crs(CHab)) 

## join with pland - subsets to rb9 region
CHab <- st_join(CHab, cnts, left = F)

## predictions on critical habitat

CHabDataObsJoin <- st_read("ignore/Protected_Land/07_joined_critical_habitat_and_mod_data_binary.shp")

## polygons of protected land
## join with CHab
rb9pl <- st_join(CHab, cnts, left = F) %>%
  st_transform(crs=st_crs(dataObsSP)) 
  
rb9pl ## geom is the protected land

## take only columns of interest and distinct rows, join with model data, and create presabs column based on 0.5 threshold

CHabx <- rb9pl %>%
  st_join(dataObsSP) %>%
  drop_na(cells)

CHabx 
length(unique(CHabx$UNIT)) ## 10 units of critical land in total

## Presence Points
## filter from main df

PresC <- CHabDataObsJoin %>%
  filter(MyPresAbs == "Presence")


## map predicted presences on critical
m2 <- ggplot()+
  # geom_sf(data=ca_sf)+ ## california
  geom_sf(data=sheds_sf, aes(fill=SMC_Name), color=NA)+ ## watersheds
  scale_fill_manual(values=beige_pal, guide= "none") + ## beige colour for watersheds
  # geom_sf(data=cnts) + ## rb9 boundary
  geom_sf(data=CHabx, fill = "yellow") + ## critical habitat
  geom_sf(data = PresC, aes(col = Critical)) +
  labs(col = "Within Boundary") +
  # geom_sf(data=presNot, colour = "#DC3220", pch=1, size=1) +
  # geom_sf(data=presProt, colour = "#005AB5", pch=1, size=1) +
  coord_sf(xlim=c(-117.82106,-116.4), ## axis limits
           ylim=c(32.5, 33.7),
           crs=4326) +
  theme_bw() +
  theme(axis.text=element_text(size=20)) +
  theme(legend.text=element_text(size=20),
        legend.title =  element_text(size=20))

m2

## create directory 
file.name1 <- paste("Figures/05_presences_on_critical_habitat_current_with_legend.jpg")
## save
ggsave(m2, filename=file.name1, dpi=600, height=10, width=15)

# Protected Land ----------------------------------------------------------

## open space lands that have been protected for open space uses through fee ownerships
PlandP <- st_read("ignore/04_pendleton_protected_land_combined_spatial.shp", promote_to_multi = FALSE) %>%
  st_transform(crs=st_crs(cnts))
  
## polygons of protected land
## join with pland
rb9pl <- st_join(PlandP, cnts, left = F) %>%
  st_transform(crs=st_crs(dataObsSP))

rb9pl ## geom is the protected land

st_crs(rb9pl) == st_crs(dataObsSP)

## take only columns of interest and distinct rows, join with model data
Plandx <- rb9pl %>%
  select(UNIT_ID, UNIT_NAME) %>%
  # filter(!UNIT_ID == 5778) %>%
  # distinct(UNIT_NAME, .keep_all =T) %>%
  st_join(dataObsSP) %>%
  drop_na(cells) 

## predictions on protected land
PlandDataObsJoin <- st_read("ignore/Protected_Land/07_joined_land_and_mod_data_binary.shp")

## Presence Points
## filter from main df
PresP <- PlandDataObsJoin %>%
  filter(MyPresAbs == "Presence") 

## map presences on protected land
m3 <- ggplot()+
  # geom_sf(data=ca_sf)+ ## california
  geom_sf(data=sheds_sf, aes(fill=SMC_Name), color=NA)+ ## watersheds
  scale_fill_manual(values=beige_pal, guide= "none") + ## beige colour for watersheds
  # geom_sf(data=cnts) + ## rb9 boundary
  geom_sf(data=Plandx, fill = "yellow") + ## protected land
  geom_sf(data = PresP, aes(col = Protected)) +
  labs(col = "Within Boundary") +
  # geom_sf(data=presNot, colour = "#DC3220", pch=1, size=1) +
  # geom_sf(data=presProt, colour = "#005AB5", pch=1, size=1) +
  coord_sf(xlim=c(-117.82106,-116.4), ## axis limits
           ylim=c(32.5, 33.7),
           crs=4326) +
  theme_bw() +
  theme(axis.text=element_text(size=20)) +
  theme(legend.text=element_text(size=20), 
        legend.title =  element_text(size=20))

## create directory 
file.name1 <- paste("Figures/05_presences_on_protected_land_current_with_legend.jpg")
## save
ggsave(m3, filename=file.name1, dpi=600, height=10, width=15)


