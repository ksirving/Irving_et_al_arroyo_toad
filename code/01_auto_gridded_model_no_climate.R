### automated gridded model

##packages
library(tidylog)
library(tidyverse)

library(sp)
library(randomForest)
library(kernlab)
library(rgl) ## not working with this R version - June 2024
library(ks)
library(sm)
library(sf)
library(caret)
library(rpart)
library("easystats")
library(raster)

set.seed(234) ## reproducibility

# NUMBER OF BOOTSTRAP REPLICATES
b=10001

sp=0.6 ## for dependence plots
# getwd()
## get path for functions
source("input_data/Functions.R")

# Upload physical data ----------------------------------------------------

## read in env data stack
xvars <- stack("ignore/00_all_rasters_200m.tif") ## new rasters @ 200m 

## upload and add layer names 
load(file = "ignore/00_final_raster_layer_names.RData")
names(xvars) <- LayerNames
LayerNames

## gridded env df 
data_hyd_sf <- as.data.frame(xvars, xy=T)

## make spatial
data_hyd_sf <- na.omit(data_hyd_sf) %>%
  dplyr::select(x,y) %>% st_as_sf(coords = c("x", "y"), crs = crs(xvars))

## extract env data at obs points
data_hyd_sfx <- raster::extract(xvars, data_hyd_sf, cellnumbers=TRUE)

## join and for mat coords
data_hyd_sf2 <- as.data.frame(cbind(data_hyd_sfx, data_hyd_sf)) %>%
  mutate(x = unlist(map(geometry,1)),
         y = unlist(map(geometry,2))) #%>%
# rename(AWC = AWC_r)


## format raster for bias surface
mask <- xvars[[1]] >-1000

# Upload bio data -------------------------------------------------------------

## updated and snapped pres/abs
bioSnap <- shapefile("input_data/ToadsObs_SnapToRaster.shp")
head(bioSnap)

orig.sdata <-  bioSnap %>% as.data.frame() %>% ## get coordinates of snapped points
  # filter(!NEAR_DIST > 50) %>%
  dplyr::select(ID, COMID, grid_code, Year:PresAbs, Ras_Long, Ras_Lat) %>%
  st_as_sf(coords=c("Ras_Long", "Ras_Lat"), crs=4326, remove=F) %>%
  distinct(ID, .keep_all=T)


cellsPres0 <- raster::extract(mask, orig.sdata, cellnumbers=TRUE)

orig.sdata <- as.data.frame(cbind(cellsPres0, orig.sdata)) %>%
  st_as_sf(coords=c("Ras_Long", "Ras_Lat"), crs=4326, remove=F) %>%
  distinct(cells, .keep_all=T)


head(orig.sdata)
dim(orig.sdata) ## 1221
length(unique(orig.sdata$ID)) ## 1221
length(unique(orig.sdata$COMID)) ## 333


# Extract physical data at points -----------------------------------------

orig.sdataP <- orig.sdata %>%
  filter(PresAbs == 1)

# sum(orig.sdataP$PresAbs ==1) ## 1033

## get cell numbers at pres/abs sites 
cellsPres1 <- raster::extract(mask, orig.sdataP, cellnumbers=TRUE)
dim(cellsPres1) ## 1033
# head(cellsPres1) ## 

## cell with p/a
# CPDF <- as.data.frame(cellsPres1)
# length(unique(CPDF$cells)) ## 1074 - number of pseudo absences to use

# Begin loop here ---------------------------------------------------------

## format loop stuff

models <- paste0("Model",seq(1, 10,1))
models
m <- "Model2"
m

for(m in models) {
   
  gridFile <- paste0("ignore/ModelResults/Gridded/", m,"/")
  gridFile

# KDE Bias Surface --------------------------------------------------------

## get cell numbers for bias

bias <- cellsPres1[,1]
cells <- unique(sort(bias))

## get coords from bias
kernelXY <- xyFromCell(mask, cells)
samps <- as.numeric(table(bias))

# code to make KDE raster
KDEsur <- sm.density(kernelXY, weights=samps, display="none", ngrid=1700, 
                     ylim=c(3600470,3728270), xlim=c(423689.9, 560489.9), nbins=0)

KDErast=SpatialPoints(expand.grid(x=KDEsur$eval.points[,1], y=KDEsur$eval.points[,2]))
KDErast = SpatialPixelsDataFrame(KDErast, data.frame(kde = array(KDEsur$estimate, 
                                                              length(KDEsur$estimate))))

KDErast <- raster(KDErast)
KDErast <- resample(KDErast, mask)
KDErast <- KDErast*mask
KDEpts <- rasterToPoints(KDErast) #(Potential) PseudoAbsences are created here 

#Now to integrate Pseudoabsences into Presence Data
a=KDEpts[sample(seq(1:nrow(KDEpts)), size=1700, replace=T, prob=KDEpts[,"layer"]),1:2] 
PA.abs<-data.frame(PresAbs=rep(0,nrow(a)))
a.sp<-SpatialPoints(a, proj4string=CRS("+proj=utm +zone=11 +datum=WGS84"))
a.spdf<-SpatialPointsDataFrame(a.sp, PA.abs)


# Add to env data ---------------------------------------------------------

## extarct rastrr values at pseudo abs points
a.spdfx <- raster::extract(xvars, a.spdf, cellnumbers = T, df=T)
## add ID
a.spdfx <- as.data.frame(cbind(a.spdf, a.spdfx)) %>% mutate(ID = paste0("P", ID))

##  extract raster at observations, format to join with pseudos
sdata <- raster::extract(xvars, orig.sdata, cellnumbers = T, df=T)

sdata1 <- as.data.frame(cbind(orig.sdata, sdata)) %>%
  st_as_sf(coords=c("Ras_Long",  "Ras_Lat"), crs = 4326, remove=F) %>%
  st_transform(crs = crs(xvars)) %>%
  mutate(x = unlist(map(geometry,1)),
         y = unlist(map(geometry,2))) %>% as.data.frame() %>%
  dplyr::select(ID, PresAbs, cells, DS_Mag_50:TC_092014_RB9.3_Var, x,y) 


## bind pseudo abs with observed p/a
NewDataObsSub <- bind_rows(sdata1, a.spdfx) %>%
  distinct(cells, .keep_all=T)

NewDataObsSub <- na.omit(NewDataObsSub)

sum(NewDataObsSub$PresAbs == 1) ## 967
sum(NewDataObsSub$PresAbs == 0) ## 972
# dim(NewDataObsSub)

save(NewDataObsSub, file=paste0(gridFile, "all_presAbs_env_data.RData"))

# Multicolinearality ------------------------------------------------------

## gridded
all_data_obs <- NewDataObsSub %>%
  as.data.frame() %>% 
  dplyr::select(ID, PresAbs, DS_Mag_50:TC_092014_RB9.3_Var) 

## colineality, onl
# testData <- NewDataObsSub %>%
#   as.data.frame() %>% 
#   dplyr::select(-c(ID, cells, Peak_5,PercentSilt, TC_042014_RB9.3_Med, 
#                    TC_042014_RB9.1_Var, TC_092014_RB9.1_Med, TC_092014_RB9.2_Med,x,y)) 
# 
# 
# corvars <-cor(all_data_obs[,c(3:ncol(all_data_obs))])
# write.csv(corvars, paste0(gridFile, "all_data_cor.csv"))

# ml <- lm(PresAbs ~ ., data=testData)
# summary(ml)
# check_collinearity(ml)


# # REMOVE MULTI-COLINEAR VARIABLES
all_data_obs <- all_data_obs %>%
  select(-c(Peak_5, PercentSilt, TC_042014_RB9.3_Med, 
            TC_042014_RB9.1_Var, TC_092014_RB9.1_Med, TC_092014_RB9.2_Med))
# all_data_obs 


# Random forests ----------------------------------------------------------
# names(all_data_obs)
ydata <- factor(all_data_obs$PresAbs, levels = c(1,0))
xdata <- all_data_obs[,c(3:ncol(all_data_obs))] 

# class(ydata)
# length(ydata)

# PERCENT OF PRESENCE OBSERVATIONS
( dim(all_data_obs[all_data_obs$PresAbs == 1, ])[1] / dim(all_data_obs)[1] ) * 100 ## 49%

# RUN RANDOM FORESTS MODEL SELECTION FUNCTION
#Also provides variable importance and such
#It is important to look at highest class error - Look at Global OOB errors
#TEST Object is super important; Row number corresponds to paramerers, in output
( rf.model <- rf.modelSel(x=xdata, y=ydata, imp.scale="mir", ntree=b, nodesize=5) ) 

# CREATE NEW XDATA BASED ON SELECTED MODEL AND RUN FINAL RF MODEL (Model 4)
#RF runs differently when you use symbolis languate (using ~ as in an Lin. Model... use the indexing approacy [y=rf.data[,1]...]

sel.vars <- rf.model$PARAMETERS[[1]]# set to use 1 - lowest error rate and has all hydro vars

rf.data <- data.frame(y=ydata, xdata[,sel.vars])	

save(rf.data, file=paste0(gridFile, "data_for_rf_model.RData"))

(rf.final <- randomForest(y=rf.data[,1], x=rf.data[,2:ncol(rf.data)], ntree=b, mtry = 2, nodesize=5,
                          importance=TRUE, norm.votes=TRUE, proximity=TRUE) )

save(rf.final, file=paste0(gridFile, "rf_model.RData"))



# Proximity ------------------------------------------------------------------

### MDS scaling from proximity

# CREATE CMD SCALED DISTANCES OF RF PROXMITIES
distanceMatrix <- dist(1 - rf.final$proximity)

rf.cmd <- cmdscale(distanceMatrix, eig=TRUE, k=4, x.ret=T)  

mdsVarPer <- round(rf.cmd$eig/sum(rf.cmd$eig)*100, 1)

mdsValues <- rf.cmd$points

mdsData <- data.frame(Sample = rownames(mdsValues),
                      X=mdsValues[,1],
                      Y=mdsValues[,2],
                      Status = rf.data$y)

m1 <- ggplot(data = mdsData, aes(x=X, y=Y)) +
  geom_point(aes(color=Status)) +
  theme_bw()+
  xlab(paste("MDS1 - ", mdsVarPer[1], "%", sep=" ")) +
  ylab(paste("MDS2 - ", mdsVarPer[2], "%", sep=" ")) +
  ggtitle(paste("Pres/Abs", "PROXIMITY MATRIX", sep=" - "))

# m1
file.name1 <- paste0(gridFile, "no_clim_model_mds_scale_gridded.jpg")
ggsave(m1, filename=file.name1, dpi=300, height=5, width=6)


# Validation --------------------------------------------------------------

## make y data compatible
rf.data.val <- rf.data %>%
  mutate(y = ifelse(y==1, "Present", "Absent")) %>%
  mutate(y = factor(y, levels = c("Present", "Absent")))

## split into training and testing

setsize <- floor(nrow(rf.data)*0.8)
index <- sample(1:nrow(rf.data), size = setsize)
training <- rf.data.val[index,]
testing <- rf.data.val[-index,]

trcontrol = trainControl(method='cv', number=10, savePredictions = T,
                         classProbs = TRUE,summaryFunction = twoClassSummary,returnResamp="all")

model = train(y ~ . , data=training, method = "rf", trControl = trcontrol,metric="ROC") 

conMat <- confusionMatrix(predict(model,testing),testing$y)

save(model, file=paste0(gridFile, "validation.RData"))
save(conMat, file=paste0(gridFile, "confusion_matrix.RData"))


pdf(paste0(gridFile, "Trees_no_clim_gridded.pdf"), width=25, height=15)

full_tree <- rpart(y~., method = "class", control = rpart.control(cp = 0, minsplit = 2), data = rf.data)
plot(full_tree)
text(full_tree, use.n = T)

dev.off()


# Coeficients and importance ----------------------------------------------

# PLOT VARIABLE IMPORTANCE
pdf(paste0(gridFile, "var_imp_no_clim_gridded.pdf"), width=25, height=15)

varImpPlot(rf.final)

dev.off()


## scaled var imp
p <- as.matrix(rf.final$importance)   
ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 

## save importance
save(p, file = paste0(gridFile, "var_imp_scaled_no_clim_gridded.RData"))

pdf(paste0(gridFile, "var_imp_scaled_no_clim_gridded.pdf"), width=25, height=15)
dotchart(p[ord,1], main="Scaled Variable Importance", pch=19)
dev.off()

## partial dependence plots
pdf(paste0(gridFile, "PartialPlots_6_no_clim_gridded.pdf"), width=8, height=8)
p <- as.matrix(rf.final$importance)    
ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]])  
dotchart(p[ord,1], main="Scaled Variable Importance", pch=19)
plot(rf.final, main="Bootstrap Error Convergence")

for (i in 1:length(names(rf.data[,2:ncol(rf.data)])) ) {
  p <- partialPlot(rf.final, rf.data[,2:ncol(rf.data)], 
                   names(rf.data[,2:ncol(rf.data)])[i], which.class="1", plot=FALSE)   
  p$y <- (p$y - min(p$y)) / (max(p$y) - min(p$y)) 
  plot( y=lowess(y=p$y, x=p$x, f=sp)$y, x=p$x, type="l", ylab="p",  
        xlab=names(rf.data[,2:ncol(rf.data)])[i],
        main=paste("PARTIAL PLOT", names(rf.data[,2:ncol(rf.data)])[i], sep=" - ")) 
}
dev.off() 

} # end loop

# Tuning ------------------------------------------------------------------
## check tuning here, if error can be improved through mtry, rerun models 

##  check error rates

rf.final$err.rate[,1]

oob.error.data <- data.frame(
  Trees = rep(1:nrow(rf.final$err.rate), times = 3),
  Type = rep(c("OOB", "0", "1"), each = nrow(rf.final$err.rate)),
  Error = c(rf.final$err.rate[,"OOB"],
            rf.final$err.rate[, "0"],
            rf.final$err.rate[,"1"])
)


t1 <- ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
# t1
file.name2 <- paste0(gridFile, "oob_error_gridded.jpg")
ggsave(t1, filename=file.name2, dpi=300, height=5, width=6)

## check split

oob.values <- vector(length=10)
# oob.values

for(i in 1:10) {
  temp.model <- randomForest(y~., data=rf.data, mtry = i, ntree=b)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
  
}

# oob.values
# rf.final$mtry
# mean(rf.final$err.rate[,1])


# Response curves/relationships ------------------------------------------------

## change in delta

## test change in delta curves with absolute values
## may need to be run with actual values, no delta

## change to absolute
rf.data.ch <- abs(rf.data[, -1]) %>%
  mutate(Y = rf.data$Y)
# names(rf.data.ch)

## define ffm 

ffm <- names(rf.data.ch)[11:16]
# ffm

## observation data with comids
all_data_obs <- na.omit(all_data_obs)
length(all_data_obs$COMID)

# dim(rf.data)
# names(rf.data)

## get means of all variables

rf.data.mean <- rf.data %>%
  pivot_longer(AvgClay:SP_Mag, names_to = "Variable", values_to="Value") %>%
  group_by(Variable) %>%
  summarise(MeanVal = mean(Value))

## make data long - raw values of ffm

rf.data.long <- rf.data %>%
  pivot_longer(AvgClay:SP_Mag, names_to = "Variable", values_to="Value")

# rf.data.long

full_data <- NULL

for(m in 1:length(ffm)) {
  
  metric <- ffm[m]
  # metric
  
  metricVals <- rf.data %>%
    pivot_longer(AvgClay:SP_Mag, names_to = "Variable", values_to="Value") %>%
    filter(Variable %in% metric)
  
  # head(metricVals)
  
  ## get sequence to predict on
  incr <- seq(min(metricVals$Value), max(metricVals$Value), (max(metricVals$Value)/20))
  incr <- rev(incr)
  
  datax <- data.frame(incr)
  # datax
  
  pred_df_incrx <- NULL
  
  ## replace mean of ffm with increment and predi using rf model
  
  for(i in 1: length(incr)) {
    
    
    ## replace metric values with increment
    data <- rf.data %>%
      select(-paste(metric)) %>%
      mutate(increment = incr[i])
    # names(data)
    
    ## change name for prediction
    colnames(data)[17] <- metric
    
    ## predict on increments one at a time
    pred_df_incr <- as.data.frame(predict(rf.final, data, type="prob",  index=2, 
                                          na.rm=TRUE, overwrite=TRUE, progress="window"))
    
    # pred_df_incr
    pred_df_incr[,3] <- incr[i]
    pred_df_incr[,4] <- i
    
    pred_df_incrx <- bind_rows(pred_df_incrx, pred_df_incr)
    
    
    
  }
  
  
  
  pred_df_incrx$COMID <- all_data_obs$COMID
  pred_df_incrx$FFM <- metric
  
  ## change names and combine
  colnames(pred_df_incrx)[1:4] <- c("ProbAbs", "ProbPres", "IncrementValue", "IncrementNumber")
  
  full_data <- bind_rows(full_data, pred_df_incrx)
  
}


# head(full_data)
# unique(full_data$FFM)

coms <- unique(full_data$COMID)[c(1,4,46,87,245)]

full_data_sub <- full_data %>%
  filter(COMID %in% coms)


## plot

p1 <- ggplot(full_data_sub, aes(y= ProbPres, x = IncrementValue)) +
  geom_path() +
  facet_grid(rows = vars(COMID), cols = vars(FFM), scales = "free_x")

p1

file.name1 <- "Figures/01_incremental_preds_sep_coms.jpg"
ggsave(p1, filename=file.name1, dpi=300, height=5, width=6)

