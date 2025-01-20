### physical data

## data formatting

# packages
library(tidyverse)

library(sf)
library(raster)
# library(nhdplusTools)
library(readr)
library(mapview)
library(stars)
library(rgdal)
library(rgeos)
library(terra)
library(zoo)

library(tidylog)


## upload basse raster with comids

bmask <- raster("input_data/Elev.tif")
# names(bmask) <- "COMID"
bmask


# length(unique(gridsDF$NHD_10m)) ## 2179

# Remote Sensing data -----------------------------------------------------
## upload raw data

sept <- brick("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/Arroyo_toad_RB9_V2/ignore/TC_2014_RB9/TC_092014_RB9.tif")
sept ## is 30m grids, disaggregate to 10m

## aggregate to 180m by median
sept180Med <- aggregate(sept, 6, fun = "median")
names(sept180Med) <- paste0(names(sept180Med), "_Med")

## aggregate to 180m by variance
sept180Var <- aggregate(sept, 6, fun = "var")
names(sept180Var) <- paste0(names(sept180Var), "_Var")

## stack rasters
sept180 <- stack(sept180Med, sept180Var)

## match projection with mask raster
crs(sept180)<-crs(bmask)

## resample to mask layer (200m)
septr <- resample(sept180, bmask, method = "bilinear")
names(septr)

## upload wet season data
april <- brick("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/Arroyo_toad_RB9_V2/ignore/TC_2014_RB9/TC_042014_RB9.tif")

## aggregate to 180m by median
april180Med <- aggregate(april, 6, fun = "median")
names(april180Med) <- paste0(names(april180Med), "_Med")

## aggregate to 180m by variance
april180Var <- aggregate(april, 6, fun = "var")
names(april180Var) <- paste0(names(april180Var), "_Var")

## stack rasters
april180 <- stack(april180Med, april180Var)

## match projection with mask raster
crs(april180)<-crs(bmask)

## resample to mask layer (200m)
aprilr <- resample(april180, bmask, method = "bilinear")
names(aprilr)

tassR <- stack(aprilr, septr)
tassR

Rnames <- names(tassR)
Rnames

## crop to original raster
tassR <- crop(tassR, bmask)
plot(tassR)
plot(bmask)
## save out
writeRaster(tassR, "ignore/00_tass_cap_raster_stack_200.tif", format="GTiff", crs="+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs ", overwrite=TRUE)

## save names
save(Rnames, file= "ignore/00_tass_cap_raster_stack_200_names.RData")

## check
# tassR <- stack("ignore/00_ann_clim_tass_cap_raster_stack_200.tif")
# tassR



# Add other variables -----------------------------------------------------
## made in GIS by Abel

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
# coms <- raster("ignore/NHD_200m/NHD_200m.tif")

# coms

hyd1

ffmR <- stack(hyd1, hyd2, hyd3, hyd4, hyd5, hyd6, hyd7, hyd8, hyd9)
ffmR

# plot(hyd1)

### landscape
elev <- raster("input_data/Elev.tif")
slope <- raster("input_data/Slope.tif")
CatchArea <- raster("input_data/AccFlow.tif")
MRVBF <- raster("input_data/MRVBF.tif")
VRM3 <- raster("input_data/VRM3.tif")
VRM18 <- raster("input_data/VRM18.tif")

lanR <- stack(elev, slope, CatchArea, MRVBF, VRM3,  VRM18)
names(lanR) <- c("Elev", "Slope", "CatchArea", "MRVBF", "VRM3",  "VRM18")
lanR

## Soil
AWC <- raster("input_data/AWC_r.tif") ## big chunk missing, add back when fixed
Clay <- raster("input_data/PercentClay.tif")
Sand <- raster("input_data/PercentSand.tif")
Silt <- raster("input_data/PercentSilt.tif")

# plot(AWC)
# plot(Clay)

soilR <- stack(Clay, Sand, Silt)
names(soilR) <- c("PercentClay", "PercentSand", "PercentSilt")
## stack together
allR <- stack(lanR, soilR)
# plot(allR)
names(allR)
## resample ffm to other rasters

ffmRes <- resample(ffmR, elev, method = "bilinear")
AWCRes <- resample(AWC, elev, method = "bilinear")
# comsRes <- resample(coms, elev, method = "bilinear")

## stack with other

allRx <- stack(ffmRes, AWCRes, allR)
allRx


# Join to climate and tass ------------------------------------------------

## load raster and names
load(file= "ignore/00_tass_cap_raster_stack_200_names.RData")
tassR <- stack("ignore/00_tass_cap_raster_stack_200.tif")
names(tassR) <- Rnames
names(tassR)

tassR <- tassR[[c(1:3, 7:9, 13:15, 19:21)]] ## remove unnecc layers, change as needed
tassR

allR <- stack(allRx, tassR)

LayerNames <- names(allR)
LayerNames <- gsub("_Delta", "", LayerNames)
LayerNames

save(LayerNames, file = "/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Irving_et_al_arroyo_toad/ignore/00_final_raster_layer_names.RData")


writeRaster(allR, "ignore/00_all_rasters_200m.tif", format="GTiff", crs="+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs ", overwrite=TRUE)

## save in other repo
# writeRaster(allR, "/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Irving_et_al_arroyo_toad/ignore/00_all_rasters_200m.tif", format="GTiff", crs="+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs ", overwrite=TRUE)
