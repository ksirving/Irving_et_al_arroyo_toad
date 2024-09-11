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
PredFile <- paste0("ignore/FuturePredictions/New")

## gridded env df with comids - model build data
load(file = "ignore/00_RB9_grdded_data.RData") 
head(data_hyd_sf2)


## upload nhd shape
nhd <- st_read("ignore/SpatialData/NHD_reaches_RB9_castreamclassification.shp")

## simplify
nhd <- nhd %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)

### current predictions
curObs <- read.csv("ignore/ModelResults/Gridded/Arroyo_Toad_Prob_Occurrence_RB9.csv")
curObs

### cells and comids
coms <- curObs %>%
  select(cells, COMID) 

write.csv(coms, "ignore/03_comids_cells_to_join_New.csv")
## join comid to main df by cells
data_hyd_sf2 <- full_join(coms, data_hyd_sf2, by = "cells")
data_hyd_sf2

## future data
newData <- read.csv("ignore/2024-08-27_RFpred_output_alldata_rb9future26yr_redo_med_dlt_FFM_test12_test2_scaleraw_capT.csv")
head(newData)
str(newData)

## change ffm names to match current names

newData <- newData %>%
  select(-d_peak_5, -n_year) %>%
  rename(DS_Mag_50 = d_ds_mag_50,
         FA_Mag = d_fa_mag,
         Peak_10 = d_peak_10,
         Peak_2 = d_peak_2,
         SP_Mag = d_sp_mag,
         Wet_BFL_Mag_10 = d_wet_bfl_mag_10,
         Wet_BFL_Mag_50 = d_wet_bfl_mag_50,
         Q99 = delta_q99,
         COMID = comid) 
                                  
head(newData)
# Predictions -------------------------------------------------------------

#Index refers to the right column of probabilities - in this model the second column, which is probs of "1"

## remove NAs
all_data <- na.omit(data_hyd_sf2)

## remove current ffm
all_data <- all_data %>%
  select(-c(DS_Mag_50:Wet_BFL_Mag_50))

# all_data <- all_data[,c("COMID", sel.vars)] 
head(all_data)
str(all_data)

## define models
models <- paste0("Model",seq(1, 10,1))
m="Model2"

## define scenarios
scenarios <- unique(newData$scenario)
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
     filter(scenario == scenarios[s])
   
   ## join ffm with other env data
   
   newDatax1 <- inner_join(all_data, newDatax, by = "COMID")
  head(newDatax1)
  length(unique(newDatax1$cells)) ## 15993
   # pred <- predict(rf.final, all_data, filename= paste0(PredFile, "SppProbs_no_clim_gridded.img"), type="prob",  index=2,
   #                 na.rm=TRUE, overwrite=TRUE, progress="window")
   
   ## predict on new data as df
   pred_df <- as.data.frame(predict(rf.final, newDatax1, filename= paste0(PredFile, "SppProbs_no_clim_gridded_df.img"), type="prob",  index=2, 
                                    na.rm=TRUE, overwrite=TRUE, progress="window"))
   ## add comids and cells
   pred_df$cells <- newDatax1$cells
   # newDatax1$cells
   # pred_df$COMID <- all_data$COMID
 
   
   ## get probability, known occs and env data all together - join by cells
   pred_env <- pred_df %>%
     full_join(newDatax1, by =  c("cells")) %>%
     rename(probOcc = 1) %>%
     # full_join(obs, by = c("cells", "COMID")) %>%
     dplyr::select(probOcc, cells, COMID, x, y)
   
   
   write.csv(pred_env, paste0(PredFile, "03_",scenarios[s],"_probOccs_gridded_New.csv"))
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
l
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
                               Scenario == 103 ~ "Small Perturbations, extremes/hotter"))

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

