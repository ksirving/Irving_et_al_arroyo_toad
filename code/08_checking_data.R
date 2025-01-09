### checking flow data current vs baseline


## packages
library(raster)
library(randomForest)
library(sf)
library(tidyverse)
library(mapview)
library(tidylog)



# Upload data -------------------------------------------------------------

## Current data

load(file = "ignore/01_RB9_grdded_data.RData") 
head( data_hyd_sf2)

CurrentData <- data_hyd_sf2 %>%
  dplyr::select(cells:Wet_BFL_Mag_50) %>%
  # rename(COMID = NHD_200m) %>%
  # mutate(Scenario2 = "Current") %>% ## add column with current
  pivot_longer(DS_Mag_50:Wet_BFL_Mag_50, names_to = "FFM", values_to = "CurrentDelta")

head(CurrentData)

## comid - 22549123
## cell - 175293

## 20348187
## 144, 145, 182

## check delta rasters

## use as baselayer
elev <- raster("input_data/Elev.tif")

## ffm rasters
hyd1 <- raster("ignore/Delta_Rasters/DS_Mag_50_Delta.tif")
hyd2 <- raster("ignore/Delta_Rasters/FA_Mag_Delta.tif")
hyd3 <- raster("ignore/Delta_Rasters/Peak_2_Delta.tif")
hyd4 <- raster("ignore/Delta_Rasters/Peak_5_Delta.tif")
hyd5 <- raster("ignore/Delta_Rasters/Peak_10_Delta.tif")
hyd6 <- raster("ignore/Delta_Rasters/Q99_Delta.tif")
hyd7 <- raster("ignore/Delta_Rasters/SP_Mag_Delta.tif")
hyd8 <- raster("ignore/Delta_Rasters/Wet_BFL_Mag_10_Delta.tif")
hyd9 <- raster("ignore/Delta_Rasters/Wet_BFL_Mag_50_Delta.tif")

ffmR <- stack(hyd1, hyd2, hyd3, hyd4, hyd5, hyd6, hyd7, hyd8, hyd9)
ffmR

## resample to base layer
ffmRes <- resample(ffmR, elev, method = "bilinear")
ffmRes

### checking data
## read in env data stack
# xvars <- stack("ignore/00_all_rasters_200m.tif") ## new rasters @ 200m


## gridded env df
ffmDF<- as.data.frame(ffmRes, xy=T)

## make spatial
ffmDF <- na.omit(ffmDF) %>%
  dplyr::select(x,y) %>%
  st_as_sf(coords = c("x", "y"), crs = crs(xvars))
# 
# ## extract env data at obs points
CurrentData <- as.data.frame(raster::extract(ffmRes, ffmDF, cellnumbers=TRUE)) %>%
  pivot_longer(DS_Mag_50:Wet_BFL_Mag_50, names_to = "FFM", values_to = "CurrentDelta")
head(CurrentData)



## join current data

# CurrentDataComs <- full_join(CurrentData, coms, by = "cells")
# CurrentDataComs

## ffm values here do not match delta values used to make the rasters
## they seem close - it might be an old verrsion that Abel sent over
## it may be something to do with COMIDs. but less likely


## rasters
## directory 
rasFile <- "ignore/futurerasters/"

## define scenarios
scen <- c( "S3_", "S7_", "S19_", "S21_", "S27_", "S32_", "S39_", "S100_", "S101_", "S102_", "S103_")

## use as baselayer
elev <- raster("input_data/Elev.tif")

## get all files with .tif - these are the main rasters
tifs <- list.files(path = rasFile, pattern = c(".tif$") )

## get the scenario we need - here to test 21 = baseline
tifsscen <- grep(paste(scen[4]), tifs)
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
ffmRes

## extract future data at grid cells
newData <- as.data.frame(raster::extract(ffmRes, data_hyd_sf, cellnumbers=TRUE))
# newData <- as.data.frame(newData)

head(newData)

## remove scenario number from column names
names(newData) <- gsub(paste(scen[4]), "", names(newData))

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
         Q99 = delta_q99) 

head(newData)




head(newData)
head(CurrentData)

# CurrentDataWideComs <- full_join(CurrentDataWide, coms, by = "cells")
# CurrentDataWideComs
# Join Data ---------------------------------------------------------------


## join coms with future data

FutureDataComs <- newData %>%
  # full_join(coms, FutureData, by = "COMID") %>%
  # rename(Scenario = scenario) %>%
  # select(-X) %>%
  pivot_longer(DS_Mag_50:Q99, names_to = "FFM", values_to = "BaselineDelta")

head(FutureDataComs)
head(CurrentData)

### join current and baseline data by cell number


AllData <- full_join(FutureDataComs, CurrentData, by = c("FFM", "cells"))


# > rows only in FutureDataComs    2,421
# > rows only in CurrentData          54
# > matched rows                 144,153
# >                             =========
#   > rows total                   146,628


### find NAs

indCur <- which(is.na(AllData$CurrentDelta)) ## index of current deltas NAs

test <- AllData[indCur,] ## NAs in delta and current - 4 cells
length(unique(test$cells))


remove <- unique(na.omit(test$cells)) ## define cells to remove


## remove from main df
AllData <- AllData %>%
  filter(!cells %in% remove)


## Na test again

indCur <- which(is.na(AllData$CurrentDelta)) ## index of current deltas NAs
## 0 
test <- AllData[indCur,] ## NAs in delta and current - 4 cells
## no cell number, so not included in analysis through rasterisation

AllData <- AllData %>%
  drop_na(cells) ## remove all rows with no cell number

sum(is.na(AllData))

## NA on baseline

indBase <- which(is.na(AllData$BaselineDelta))
test <- AllData[indBase,]
## have delta for current but not for baseline with some comids
length(unique(MissingComs$`na.omit(test$COMID)`)) ## 14

## some NAs we don't have the COMID for but we have current delta, not baseline delta

MissingComs <- as.data.frame(na.omit(test$COMID))
MissingComs


# Compare values ----------------------------------------------------------

## get only delta
deltaMat <- AllData %>%
  select(BaselineDelta, CurrentDelta) %>%
  drop_na()

## correlation
cor(deltaMat) ## 0.16

## empty dataframe
df <- data.frame(ncol = 2, nrow = 9)
colnames(df) <- c("FFM", "Cor")

## define FFMs

ffms <- unique(AllData$FFM)

## get ffm 

for(f in 1:length(ffms)) {
  
  ffmDat <- AllData %>%
    filter(FFM == ffms[f]) %>% ## filter to ffm
    select(BaselineDelta, CurrentDelta) %>% ## select only delta
    drop_na()
  
  ## get correlation
  corDat <- cor(ffmDat)
  
  ## put in df
  df[f,1] <- paste(ffms[f])
  df[f,2] <- corDat[1,2]
  
}


df


# Compare Toad model outputs ----------------------------------------------

## scenario results
scenProbs <- read.csv("ignore/FuturePredictions/03_Av_Probs_Future_RB9_extremes_New.csv") %>%
  select(-c(X,X.1)) %>%
  filter(Scenario2 == "Baseline") %>% ## only baseline
  select(-Scenario, -Scenario2) %>%
  rename(BaselineProb = MeanProb)

head(scenProbs)


## current results
hYdMod <- read.csv("ignore/ModelResults/Gridded/02_Av_Probs_Current_RB9.csv") %>%
  select(-X, -ModePres) %>%
  rename(CurrentProb = MeanProb)
head(hYdMod)


## join together

allProbs <- full_join(scenProbs, hYdMod, by = c("cells", "x", "y"))
head(allProbs)

allcor <- allProbs %>%
  select(BaselineProb, CurrentProb) %>%
  drop_na()

cor(allcor) ## 0.99 

## make binary 

allBins <- allProbs %>%
  mutate(BaselinePA = ifelse(BaselineProb >= MeanThresh, 1, 0), 
         CurrentPA = ifelse(CurrentProb >= MeanThresh, 1, 0)) %>%
  drop_na(BaselinePA)

# ind <- which(is.na(allBins$BaselinePA))
# length(ind) ## 29 cells with no baseline data
# 
# allBins[ind, ]

sum(allBins$BaselinePA == 1) ## 2237 - was 677
sum(allBins$CurrentPA==1) ## 2294 - was 2113
