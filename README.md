# Irving_et_al_arroyo_toad

## Species distribution model assessing impact of future climate-induced flow alteration on Arroyo Toad

## Code

# Script 0: 00_gridded_200m_data_format.R

formatting environmental/physical data script to raster format

input data needed

## landscape
Elev.tif
Slope.tif
AccFlow.tif
MRVBF.tif
VRM3.tif
VRM18.tif

## soil
AWC_r.tif
PercentClay.tif
PercentSand.tif
PercentSilt.tif

## remote sensing
TC_092014_RB9.tif
TC_042014_RB9.tif

##FFM
"Delta_Rasters/DS_Mag_50_Delta.tif"
"Delta_Rasters/FA_Mag_Delta.tif"
"Delta_Rasters/Peak_2_Delta.tif"
"Delta_Rasters/Peak_5_Delta.tif"
"Delta_Rasters/Peak_10_Delta.tif"
"Delta_Rasters/Q99_Delta.tif"
"Delta_Rasters/SP_Mag_Delta.tif"
"Delta_Rasters/Wet_BFL_Mag_10_Delta.tif"
"Delta_Rasters/Wet_BFL_Mag_50_Delta.tif"

## writes stacked raster will all environmental data 

00_all_rasters_200m.tif
00_final_raster_layer_names.RData ## and raster names

# Script 1: 01_auto_gridded_model_no_climate.R

Random forest SDM. Automates - 
1 RF model
2 Pseudo absenses
3 Partial Plots
4 Proximity
5 Validation
6 Model coeficients
7 Relative importance

Runs 10 x models and saves files separately

# input data needed

00_all_rasters_200m.tif ## environmental data
ToadsObs_SnapToRaster.shp ## Toad observation data (can not share)

## Saves raster stack as dataframe
01_RB9_grdded_data.RData

# Script 2: 02_current_predictions.R

Predicts toad distribution under current conditions and combines probabilities & importance over models

Input data needed - 

01_RB9_grdded_data.RData ## df of raster environmental data
00_final_raster_layer_names.RData ## raster layer names
NHD_reaches_RB9_castreamclassification.shp ## NHD reaches for staudy area
Elev.tif ## base raster

Also, uses baseline scenario as current conditions
ignore/futurerasters -  UPDATE!!!!

## Saves
02_relative_importance_mean.csv ## mean relative importance 
02_validation_values.csv ## mean validation values
02_Av_Probs_Current_RB9.csv ## mean probability of occurrence df
02_Arroyo_Toad_Prob_Occurrence_RB9.shp ## mean probability of occurrence spatial
02_Arroyo_Toad_Prob_Occurrence_RB9.tif ## mean probability of occurrence raster

# Script 3: 03_future predictions.R

Predicts toad under future scenarios and combines probabilities over models

Input data needed
All future scenario rasters - ignore/futurerasters -  UPDATE!!!!

# Saves
03_Av_Probs_Future_RB9_extremes_New.csv ## probabilities for all scenarios


# Script 4: 04_physcial_factors_of_CURRENT_predictions.R

Calulates occurrences predicted on protected land and critical habitat

Input data - 
All predictions from Script 02
02_Av_Probs_Current_RB9.csv ## mean probability of occurrence df
02_Arroyo_Toad_Prob_Occurrence_RB9.shp ## mean probability of occurrence spatial
02_Arroyo_Toad_Prob_Occurrence_RB9.tif ## mean probability of occurrence raster

# Protected Land/Habitat
Pendleton_95.shp ## Camp pendleton 
CPAD_2023a_Holdings.shp ## Protected Land
ArroyoToadFinalCriticalHabitatUSFWSds129.shp ## Critical habitat

# spatial data
SD_RB9_boundary.shp ## study area polygon
Ca_State_poly.shp ## State of Ca
SMCSheds2009.shp ## Watersheds of So Cal


## Saves and maps 
04_prob_occ_current.csv ## probability of occurrence with observed presence/absence

Analysis not really needed as also counted in script 06 and mapped in script 05

# Script 5: 05_figures_current_analysis.R

Making figures for the manuscript for the current analysis

# Input data - 
Elev.tif ## base raster
02_obs_probs_current.RData ## observations
02_Arroyo_Toad_Prob_Occurrence_RB9.shp ## Probabilities

# spatial data
SD_RB9_boundary.shp ## study area polygon
Ca_State_poly.shp ## State of Ca
SMCSheds2009.shp ## Watersheds of So Cal
Counties.shp ## counties CA

## Figure 3 created
05_probability_of_occurence_current ## map of probabilities (Figure 3)
05_presences_on_critical_habitat_current_with_legend.jpg ## within critical habitat (Figure 3)
05_presences_on_protected_land_current_with_legend.jpg ## within protected land

## Script 6: 06_physcial_factors_of_FUTURE_predictions.R

Evaluating ranges within each scenario for differences in -
elevation (t tests, counts and %)
critical habitat (counts and %)
protected land (counts and %)
FFM (t tests)

## Input data
02_Av_Probs_Current_RB9.csv # probabilities
03_future_ffms.csv # FFM scenario data
03_Av_Probs_Future_RB9_extremes_New.csv # future probability of occurrence
all_presAbs_env_data.RData # observations input for model
Elev.tif # elevation

## Figures created 

06_Predictions_FFM.jpg ## FFM figure (Figure 5)

## Script 7: 07_baseline_future_comparison.R

Comparison of baseline/current and future scenarios
Evaluate - 
differences in probability
Range size
range overlap 

# Input data
03_Av_Probs_Future_RB9_extremes_New.csv ## future probability of occurrence
Elev.tif ## base raster

## Figures created
07_change_in_probability_future_scens.jpg # change in probability (Figure 4)

## Script 8: 08_checking_data.R

A place to check data

## Script 9: citations.R

citiations for packages used in analysis

## Metadata links

Observation data 
Can not share at this time

Environmental data for model build

FFM for future scenarios

Spatial Data



