## predicting distribution

library(raster)
library(randomForest)

library(sf)
library(tidyverse)

library(tidylog)
library(DescTools)
library("scales")

?Mode


# upload data -------------------------------------------------------------

## define folders
gridFile <- paste0("ignore/ModelResults/Gridded/")
COMIDFile <- paste0("ignore/ModelResults/COMID/")

## gridded env df 
load(file = "ignore/01_RB9_grdded_data.RData") 
head(data_hyd_sf2)

## remove all FFM
data_hyd_sf2 <- data_hyd_sf2 %>%
  dplyr::select(-c(DS_Mag_50:Wet_BFL_Mag_50))

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


# ## observations
load(file=paste0("ignore/ModelResults/Gridded/Model1/all_presAbs_env_data.RData"))
head(NewDataObsSub)

## get obs and format. makespatial
obs <- NewDataObsSub %>% as.data.frame() %>%
  dplyr::select(PresAbs, cells)
#   
head(obs)

## upload nhd shape
nhd <- st_read("ignore/SpatialData/NHD_reaches_RB9_castreamclassification.shp")

## simplify
nhd <- nhd %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)

nhd


# Baseline Scenario data --------------------------------------------------

## directory 
rasFile <- "ignore/futurerasters/"

## define scenarios
scen <- c( "S21_")

## use as baselayer
elev <- raster("input_data/Elev.tif")

## remove NAs from main df and make spatial
data_hyd_sf <- na.omit(data_hyd_sf2) %>%
  dplyr::select(x,y) %>% st_as_sf(coords = c("x", "y"), crs = crs(xvars))
data_hyd_sf

## get all files with .tif - these are the main rasters
tifs <- list.files(path = rasFile, pattern = c(".tif$") )
tifs

  ## get the scenario we need (baseline)
  tifsscen <- grep(paste(scen[1]), tifs)
  tifsscen
  
  scen1 <- tifs[tifsscen]
  scen1 ## has all FFM for one scenario
  
  ## upload FFM rasters for scenario
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
  
  head(newData)
  
  ## remove scenario number from column names
  names(newData) <- gsub(paste(scen), "", names(newData))
  
  ## change ffm names to match current names
  newData <- newData %>%
    rename(DS_Mag_50 = d_ds_mag_50,
           FA_Mag = d_fa_mag,
           Peak_10 = d_peak_10,
           Peak_2 = d_peak_2,
           SP_Mag = d_sp_mag,
           Wet_BFL_Mag_10 = d_wet_bfl_mag_10,
           Wet_BFL_Mag_50 = d_wet_bfl_mag_50,
           Q99 = delta_q99) %>%
    mutate(Scenario = paste(scen))
  
  ## join all other phys data
  data_hyd_sf2 <- data_hyd_sf2 %>%
    inner_join(newData, by = "cells")
  
  data_hyd_sf2
# Predict on all rb9 region-------------------------------------------------------

#Index refers to the right column of probabilities - in this model the second column, which is probs of "1"
all_data <- na.omit(data_hyd_sf2)
# all_data <- all_data[,c("COMID", sel.vars)] 
head(all_data)
str(all_data)

## empty df for var imp
VarImpx <- NULL

## empty DF for validation
Valsx <- NULL

## define models
models <- paste0("model",seq(1, 10,1))
m="model2"

# Create a sequence of thresholds from 0 to 1 (e.g., every 0.05)
thresholds <- seq(0.1, 0.95, by = 0.05)
thresholds
# Calculate sensitivity and specificity for each threshold
sensitivity <- numeric(length(thresholds))
specificity <- numeric(length(thresholds))

## start loop

for(m in models) {
  ## define folder (model) to pull from
  gridFile <- paste0("ignore/ModelResults/Gridded/", m,"/")
  
  ## upload model
  load(file = paste0(gridFile,"rf_model.RData"))
  
  ##  upload presence absence
  load(file=paste0(gridFile, "all_presAbs_env_data.RData"))

 
 ## calculate oob error rate  
  conf <- rf.final$confusion[,-ncol(rf.final$confusion)]
  oob <- 1 - (sum(diag(conf))/sum(conf))
  oob <- oob * 100
  
  ## upload validation data
  load(file=paste0(gridFile, "validation.RData"))
  Vals <- model$results
  Vals <- Vals[1,]
  Vals$Error <- 100-oob
  Vals$TSS <- Vals$Sens + Vals$Spec -1
  Valsx <- rbind(Valsx, Vals)

  ## var imp
  VarImp <- as.data.frame(rf.final$importance) 
  VarImp$Variable <- rownames(VarImp)
  # rownames(VarImp) <- NULL

  VarImpx <- rbind(VarImpx, VarImp)
  
  ## predict on data from only observed toad sites - for max sens/spec calculation
  pred <- predict(rf.final, NewDataObsSub, filename= paste0(gridFile, "SppProbs_no_clim_gridded.img"), type="prob",  index=2, 
                  na.rm=TRUE, overwrite=TRUE, progress="window")
  

## loop around prob thresholds for sens/spec
  for (i in seq_along(thresholds)) {
    # Determine presence/absence based on the current threshold
    predicted_labels <- ifelse(pred[, "1"] >= thresholds[i], 1, 0)

    # Calculate confusion matrix

    conf_matrix <- table(predicted_labels, NewDataObsSub$PresAbs)

    # Calculate sensitivity and specificity
    sensitivity[i] <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
    specificity[i] <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
  }
  
  # Find the threshold that maximizes specificity and sensitivity
  best_threshold_index <- which.max(sensitivity + specificity)
  best_threshold <- thresholds[best_threshold_index]
  best_sensitivity <- sensitivity[best_threshold_index]
  best_specificity <- specificity[best_threshold_index]
  
  # Print the best threshold and corresponding sensitivity and specificity
  print(paste("Best Threshold:", best_threshold))
  print(paste("Best Sensitivity:", best_sensitivity))
  print(paste("Best Specificity:", best_specificity))
  
  
  pred_df <- as.data.frame(predict(rf.final, all_data, filename= paste0(gridFile, "SppProbs_no_clim_gridded_df.img"), type="prob",  index=2, 
                                   na.rm=TRUE, overwrite=TRUE, progress="window"))
  
  pred_dfr <- as.data.frame(predict(rf.final, all_data, filename= paste0(gridFile, "SppResp_no_clim_gridded_df.img"), type="response", 
                                   na.rm=TRUE, overwrite=TRUE, progress="window")) %>%
    rename(PresAbsPredict = 1)
  

  ## add comids and cells
  pred_df$PresAbsPredict <- pred_dfr$PresAbsPredict
  pred_df$cells <- all_data$cells
  pred_df$threshold <- best_threshold
  pred_df$BestSens <- best_sensitivity
  pred_df$BestSpec <- best_specificity
  
  # pred_df$COMID <- all_data$COMID
  
  ## get probability, known occs and env data all together - join by cells and comid
  pred_env <- pred_df %>%
    full_join(all_data, by =  c("cells")) %>%
    rename(probOcc = 1) %>%
    # full_join(obs, by = c("cells", "COMID")) %>%
    dplyr::select(probOcc, PresAbsPredict, threshold, BestSens, BestSpec, cells, x, y)

  
  write.csv(pred_env, paste0(gridFile,"probOccs_gridded.csv"))
  
  # head(pred_env)
}

head(pred_env)
head(pred_df)

# Combining importance ----------------------------------------------------
head(VarImpx)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
## get mean and scale and % to make relative
ImpMean <- VarImpx %>% 
  group_by(Variable) %>%
  summarise(MeanImp = mean(MeanDecreaseAccuracy)) %>%
  mutate(MeanImpScaled = rescale(MeanImp)) %>%
  mutate(MeanImpPerc = (MeanImpScaled/sum(MeanImpScaled))*100)

ImpMean
## humanise variables - check the remote senseing
ImpMean <- ImpMean %>%
  mutate(VariableHuman = case_when(Variable == "DS_Mag_50" ~ "Dry Season Baseflow",
                                   Variable == "Peak_10" ~ "Peak Flow: 10-Year Flood",
                                   Variable == "FA_Mag" ~ "Fall Pulse",
                                   Variable == "Peak_2" ~ "Peak Flow: 2-Year Flood",
                                   Variable == "Q99" ~ "Largest Annual Storm",
                                   Variable == "SP_Mag" ~ "Spring Recession",
                                   Variable == "Wet_BFL_Mag_10" ~ "Wet Season Baseflow (Low)",
                                   Variable == "Wet_BFL_Mag_50" ~ "Wet Season Baseflow (Med)",
                                   Variable == "PercentSand" ~ "Sand (%)",
                                   Variable == "PercentClay" ~ "Clay (%)",
                                   Variable == "MRVBF" ~ "Index of Valley Bottom Flatness",
                                   Variable == "CatchArea" ~ "Catchment Area",
                                   Variable == "AWC_r" ~ "Water Storage Capacity",
                                   Variable == "Elev" ~ "Elevation",
                                   Variable == "Slope" ~ "Slope (%)",
                                   Variable == "VRM18" ~ "Vector Ruggedness Measure (18)",
                                   Variable == "VRM3" ~ "Vector Ruggedness Measure (3)",
                                   Variable == "TC_042014_RB9_1_Med" ~ "April Brightness (Med)",
                                   Variable == "TC_042014_RB9_2_Med" ~ "April Greenness (Med)",
                                   Variable == "TC_042014_RB9_2_Var" ~ "April Greenness (Var)",
                                   Variable == "TC_042014_RB9_3_Var" ~ "April Wetness (Var)",
                                   Variable == "TC_092014_RB9_1_Var" ~ "Sept. Brightness (Var)",
                                   Variable == "TC_092014_RB9_2_Var" ~ "Sept. Greenness (Var)",
                                   Variable == "TC_092014_RB9_3_Med" ~ "Sept. Wetness (Med)",
                                   Variable == "TC_092014_RB9_3_Var" ~ "Sept. Wetness (Var)"))

## order in increasing values

ImpMean$VariableHuman <- factor(ImpMean$VariableHuman, levels=ImpMean[order(ImpMean$MeanImpPerc,decreasing=F),]$VariableHuman)
ImpMean$VariableHuman

## plot
i1 <- ggplot(ImpMean, aes(x=MeanImpPerc, y=VariableHuman)) +
  geom_point() +
  scale_x_continuous("Relative Importance (%)") +
  scale_y_discrete("") +
  theme_bw()
i1

file.name1 <- "ignore/ModelResults/Gridded/02_relative_importance_mean.jpg"
ggsave(i1, filename=file.name1, dpi=300, height=5, width=8)

## save var imp data

write.csv(ImpMean, "ignore/ModelResults/Gridded/02_relative_importance_mean.csv")


# Combining validations  --------------------------------------------------

head(Valsx)
## standard error function
std.error <- function(x) sd(x)/sqrt(length(x))

vdf <- Valsx %>%
  pivot_longer(ROC:TSS, names_to = "Measure", values_to = "Value") %>%
  group_by(Measure) %>%
  summarise(MeanVals = mean(Value), 
            SEVals = std.error(Value),
            SDVals = sd(Value))

vdf

write.csv(vdf, "ignore/ModelResults/Gridded/02_validation_values.csv")

# TSS <-  vdf[] + TNR -1

# Combining probabilities --------------------------------------------------------

## define models
models <- paste0("model",seq(1, 10,1))
# m=2
## empty dataframe
probsx <- NULL

## combine all probabilities
for(m in models) {
  
  ## upload data and format, add model number
  gridFile <- paste0("ignore/ModelResults/Gridded/", m,"/")
  probs <- read.csv(paste0(gridFile,"probOccs_gridded.csv")) %>%
    mutate(Model = m)
  
  ## combine
  probsx <- bind_rows(probsx, probs)

}
probsx

### summarise probability over models
probsx_mean <- probsx %>%
 group_by(cells, x, y) %>%
  summarise(MeanProb = mean(na.omit(probOcc)),
            ModePres = Mode(PresAbsPredict),
            MeanThresh = mean(threshold))

head(probsx_mean)
unique(probsx_mean$MeanThresh) ## 0.535

## save out
write.csv(probsx_mean, "ignore/ModelResults/Gridded/02_Av_Probs_Current_RB9.csv")
probsx_mean <- st_read("ignore/ModelResults/Gridded/02_Av_Probs_Current_RB9.csv")
# Formatting probability of occurrence --------------------------------------


## get mask raster
bmask <- raster("input_data/Elev.tif")

## make probabilities spatial

probs_sf <- probsx_mean %>%
  st_as_sf(coords=c("x", "y"), crs=crs(bmask), remove=F) 
probs_sf
## save out
st_write(probs_sf, "ignore/ModelResults/Gridded/02_Arroyo_Toad_Prob_Occurrence_RB9.shp", append=F)

probs_sf <- st_read("ignore/ModelResults/Gridded/02_Arroyo_Toad_Prob_Occurrence_RB9.shp")
dim(probs_sf)

sum(probs_sf$ModePres == 1) ## 2767

## make spatial and transformCRS
coordinates(probsx_mean) <- ~x+y
projection(probsx_mean) <- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs"

## create template raster
x <- bmask

## make rasters of probability in NAD83

  x<-raster::rasterize(probsx_mean, x, field="MeanProb", na.rm =TRUE, sp = TRUE)
  projection(x)<-"+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs"

  ## save
writeRaster(x, "ignore/ModelResults/Gridded/02_Arroyo_Toad_Prob_Occurrence_RB9.tif", format="GTiff", crs="+proj=geocent +ellps=GRS80 +units=m +no_defs", overwrite=TRUE)
  
## make rasters of presence absence in NAD83

x<-raster::rasterize(probsx_mean, x, field="ModePres", na.rm =TRUE, sp = TRUE)
projection(x)<-"+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs"

## save
writeRaster(x, "ignore/ModelResults/Gridded/02_Arroyo_Toad_PresAbs_Occurrence_RB9.tif", format="GTiff", crs="+proj=geocent +ellps=GRS80 +units=m +no_defs", overwrite=TRUE)

## get obs and format. makespatial 
obs <- obs %>%
  inner_join(probs_sf, by = c("cells")) #%>%
  # st_as_sf(coords=c("x", "y"), crs=crs(bmask), remove=F)
obs
## save out
save(obs, file = "ignore/ModelResults/Gridded/02_obs_probs_current.RData" )

### get comids from nhd

# ProbsComs <- raster::extract(xvars, nhd, cellnumbers=TRUE)
# 
# ProbsComs
