## future predictions

# Code model comparison - current to future
# Range size/contraction
# Change in elevation etc
# Range size to Treglia
# Differences in range, latitude??
# Substrate on predicted occurences
# Protected land/critical habitat

library(raster)
library(randomForest)
library(sf)
library(tidyverse)
library(mapview)
library(tidylog)


# Data and folders --------------------------------------------------------

## folder for models
gridFile <- paste0("ignore/ModelResults/Gridded/")

## folder for future predictions
PredFile <- paste0("ignore/FuturePredictions")

# ## gridded env df  - model build data
load(file = "ignore/01_RB9_grdded_data.RData")
head(data_hyd_sf2)

## read in env data stack
xvars <- stack("ignore/00_all_rasters_200m.tif") ## new rasters @ 200m 

## upload and add layer names 
load(file = "ignore/00_final_raster_layer_names.RData")
names(xvars) <- LayerNames
LayerNames

## gridded env df 
data_hyd_sf <- as.data.frame(xvars, xy=T)

## remove NAs and make spatial
data_hyd_sf <- na.omit(data_hyd_sf) %>%
  dplyr::select(x,y) %>% st_as_sf(coords = c("x", "y"), crs = crs(xvars))

head(data_hyd_sf)
## upload nhd shape
nhd <- st_read("ignore/SpatialData/NHD_reaches_RB9_castreamclassification.shp")

## simplify
nhd <- nhd %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)

### current predictions
curObs <- st_read("ignore/ModelResults/Gridded/02_Arroyo_Toad_Prob_Occurrence_RB9.shp")
curObs

# ### cells and comids
# coms <- curObs %>%
#   select(cells, COMID) 
# 
# write.csv(coms, "ignore/03_comids_cells_to_join_New.csv")

## join comid to main df by cells
# data_hyd_sf2 <- full_join(coms, data_hyd_sf2, by = "cells")
# data_hyd_sf2


# Future data -------------------------------------------------------------

## directory 
rasFile <- "ignore/futurerasters/"

## define scenarios
scen <- c( "S3_", "S7_", "S19_", "S21_", "S27_", "S32_", "S39_", "S100_", "S101_", "S102_", "S103_")

## use as baselayer
elev <- raster("input_data/Elev.tif")

## empty DF
newDatax <- NULL

## get all files with .tif - these are the main rasters
tifs <- list.files(path = rasFile, pattern = c(".tif$") )

## loop over sceanrios to get all in one df

for(s in 1:length(scen)) {
  
  ## get the scenario we need - here to test 21 = baseline
  tifsscen <- grep(paste(scen[s]), tifs)
  tifsscen
  
  scen1 <- tifs[tifsscen]
  scen1 ## has all FFM for one scenario
  
  ## upload rasters for scenario
  hyd1 <- raster(paste0(rasFile, scen1[1]))
  hyd2 <- raster(paste0(rasFile, scen1[2]))
  hyd3 <- raster(paste0(rasFile, scen1[3]))
  hyd4 <- raster(paste0(rasFile, scen1[4]))
  hyd5 <- raster(paste0(rasFile, scen1[5]))
  hyd6 <- raster(paste0(rasFile, scen1[6]))
  hyd7 <- raster(paste0(rasFile, scen1[7]))
  hyd8 <- raster(paste0(rasFile, scen1[8]))
  hyd9 <- raster(paste0(rasFile, scen1[9]))
  
  ## stack all rasters
  ffmR <- stack(hyd1, hyd2, hyd3, hyd4, hyd5, hyd6, hyd7, hyd8, hyd9)
  ffmR
  
  ## resample to base layer
  ffmRes <- resample(ffmR, elev, method = "bilinear")
  
  ## extract future data at grid cells
  newData <- as.data.frame(raster::extract(ffmRes, data_hyd_sf, cellnumbers=TRUE))
  # newData <- as.data.frame(newData)
  
  # head(newData)
  
  ## remove scenario number from column names
  names(newData) <- gsub(paste(scen[s]), "", names(newData))
  
  ## change ffm names to match current names
  newData <- newData %>%
    select(-d_peak_5) %>%
    rename(DS_Mag_50 = d_ds_mag_50,
           FA_Mag = d_fa_mag,
           Peak_10 = d_peak_10,
           Peak_2 = d_peak_2,
           SP_Mag = d_sp_mag,
           Wet_BFL_Mag_10 = d_wet_bfl_mag_10,
           Wet_BFL_Mag_50 = d_wet_bfl_mag_50,
           Q99 = delta_q99) %>%
    mutate(Scenario = paste(scen[s]))
  
  # head(newData)
  
  newDatax <- bind_rows(newDatax, newData)
  
}

unique(newDatax$Scenario)

## save
write.csv(newDatax, "ignore/03_future_ffms.csv")

## reload and call it newData

## call it newData
newData <- read.csv("ignore/03_future_ffms.csv") %>%
  dplyr::select(-X)

head(newData)
# Predictions -------------------------------------------------------------

#Index refers to the right column of probabilities - in this model the second column, which is probs of "1"

## remove NAs
all_data <- na.omit(data_hyd_sf2)
head(all_data)
## remove current ffm to swap in newdata
all_data <- all_data %>%
  select(-c(DS_Mag_50:Wet_BFL_Mag_50))

# all_data <- all_data[,c("COMID", sel.vars)] 
head(all_data)
str(all_data)

## define models
models <- paste0("Model",seq(1, 10,1))
m="Model2"

## define scenarios
scenarios <- unique(newData$Scenario)
s=1

scenarios
## start loop

### loop over models

for(m in models) {

  ## print model number 
  
  print(m)
  
  ## define folder (model) to pull from
  gridFile <- paste0("ignore/ModelResults/Gridded/", m,"/")
 
  ## upload model
  load(file = paste0(gridFile,"rf_model.RData"))
  
  ## define where to save predictions
  PredFile <- paste0("ignore/FuturePredictions/", m,"/")
  
 
  ## loop over scenarios
  
 for(s in 1:length(scenarios)) {
   
   ## print scenarios
   print(paste0("scenario", s))
   
   ### filter to scenario
   newDatax <- newData %>%
     filter(Scenario == scenarios[s])
   
   ## join ffm with other env data
   
   newDatax1 <- inner_join(all_data, newDatax, by = "cells")

  length(unique(newDatax1$cells)) ## 15993
   # pred <- predict(rf.final, all_data, filename= paste0(PredFile, "SppProbs_no_clim_gridded.img"), type="prob",  index=2,
   #                 na.rm=TRUE, overwrite=TRUE, progress="window")
   
   ## predict on new data as df
   pred_df <- as.data.frame(predict(rf.final, newDatax1, filename= paste0(PredFile, "SppProbs_no_clim_gridded_df.img"), type="prob",  index=2, 
                                    na.rm=TRUE, overwrite=TRUE, progress="window"))
   ## add cells
   pred_df$cells <- newDatax1$cells
   # newDatax1$cells
   # pred_df$COMID <- all_data$COMID
 
   
   ## get probability, known occs and env data all together - join by cells
   pred_env <- pred_df %>%
     full_join(newDatax1, by =  c("cells")) %>%
     rename(probOcc = 1) %>%
     # full_join(obs, by = c("cells", "COMID")) %>%
     dplyr::select(probOcc, cells, x, y)
   
   
   write.csv(pred_env, paste0(PredFile, "03_",scenarios[s],"probOccs_gridded_New.csv"))
   PredFile
   
   # head(pred_env)
 }
  
   
 }
  

# Combine scenarios -------------------------------------------------------

## define models
models <- paste0("Model",seq(1, 10,1))
m="Model4"

## define scenarios
scenarios <- unique(newData$scenario)
s=1

## empty dataframe
scenprobsx <- NULL

# l= "03_100_probOccs_gridded_New.csv"

for(m in models) {
  
  print(m)
  ## define folder for models
  PredFile <- paste0("ignore/FuturePredictions/", m,"/")
  # PredFile
  ## list scenarios
  scenList <- list.files(path = PredFile)[-12]
  # scenList
  ## empty dataframe
  scenprobsx <- NULL

    for(l in scenList) {
      
      ## get scenario from file name
      sc <- str_split(l, "_")[[1]]
      sc
      ## upload scenario
      probs <- read.csv(paste0(PredFile, l)) %>%
        select(-X)

      ## add model and scenario   
      probs$Model <- m
      probs$Scenario <- sc[2]

      ## combine
      scenprobsx <- bind_rows(scenprobsx, probs)
         
    }
  
  ## save combined scenarios per model
  write.csv(scenprobsx, paste0(PredFile,"03_combined_", m, "_scenarios_New.csv"))
  
}

scenprobsx
# Means over models -------------------------------------------------------

## define models
models <- paste0("model",seq(1, 10,1))
# m=2
## empty dataframe
probsx <- NULL

## combine all probabilities
for(m in models) {
  
  print(m)

  ## define model folder
  PredFile <- paste0("ignore/FuturePredictions/", m,"/")
  ## upload combined data
  probs <- read.csv(paste0(PredFile,"03_combined_", m, "_scenarios_New.csv")) %>%
    select(-X)

  ## combine
  probsx <- bind_rows(probsx, probs)
  
}

head(probsx)

### summarise probability over models
probsx_mean <- probsx %>%
  group_by(cells, Scenario, x, y) %>%
  summarise(MeanProb = mean(na.omit(probOcc)))

head(probsx_mean)

## save out
write.csv(probsx_mean, "ignore/FuturePredictions/03_Av_Probs_Future_RB9_New.csv")


# Visualise ---------------------------------------------------------------

## boxplots of ranges of probs in scenarios
probsx_mean <- read.csv("ignore/FuturePredictions/03_Av_Probs_Future_RB9_New.csv")

## upload scenarios category
ind <- read.csv("ignore/future_scenario_lookuptable.csv") #%>%
  # mutate(Scenario = as.factor(Index)) %>%
  # mutate(Index = as.factor(Index))

str(probsx_mean)

# probsx_mean <- probsx_mean %>%
#   mutate(Scenario = as.factor(Scenario)) %>%
#   full_join(ind, by = "Scenario")

#baseline (21), P -20% (3), P +20% (39), T +2C (27), seasonality -10% wet wetter, dry drier (19)
#Combos of perturbations of all: small [P -10%, T +1C, season -5%] (14), large [P -20%, T+2C, season -10%] (7)

## add senarios combs 

probsx_mean <- probsx_mean %>%
  mutate(Scenario2 = case_when(Scenario == "S21" ~ "Baseline",
                               Scenario == 'S3' ~ "Drier",
                               Scenario == 'S39' ~ "Wetter",
                               Scenario == 'S27' ~ "Hotter",
                               Scenario == 'S19' ~ "Amplified Extremes",
                               Scenario == 'S32' ~ "Small Perturbations, drier/hotter",
                               Scenario == 'S7' ~ "Large Perturbations, drier/hotter",
                               Scenario == 'S100' ~ "Large Perturbations, wetter/hotter",
                               Scenario == 'S101' ~ "Small Perturbations, wetter/hotter",
                               Scenario == 'S102' ~ "Large Perturbations, extremes/hotter",
                               Scenario == 'S103' ~ "Small Perturbations, extremes/hotter"))

probsx_meanx <- probsx_mean %>%
  drop_na(Scenario2) 

write.csv(probsx_meanx,"ignore/FuturePredictions/03_Av_Probs_Future_RB9_extremes_New.csv")


p1 <- ggplot(subset(probsx_meanx, MeanProb > 0), aes(x=Scenario2, y = MeanProb, fill = Scenario2, col = Scenario2)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())
  

p1

file.name1 <- "Figures/03_future_scenarios.jpg"
ggsave(p1, filename=file.name1, dpi=300, height=5, width=8)


# Summarise scenarios -----------------------------------------------------

## convert probability to presence absence
probsx_meanx <- probsx_meanx %>%
      mutate(FuturePresAbs = ifelse(MeanProb < 0.535, 0, 1)) %>%
      mutate(FuturePresAbs = factor(FuturePresAbs, levels = c("1","0"), labels = c("Presence", "Absence")))

## tally presences by scenario
tallyPres <- probsx_meanx %>%
  group_by(Scenario2, FuturePresAbs) %>%
  tally()
  
tallyPres

