##########################################################################
# PROGRAM: RasterUpSample.R
# USE: UP SAMPLES A RASTER USING ROBUST REGRESSION 
# REQUIRES: RGDAL FORMAT COMPATABLE RASTERS
#           PACKAGES: MASS, sp, raster, rgdal 
#
# ARGUMENTS: 
#  x                X (HIGHER RESOULTION) INDEPENDENT VARIABLE RASTER 
#  y                Y (LOWER RESOULTION) DEPENDENT VARIABLE RASTER    
#  p                PERCENT SUBSAMPLE
#  sample.type      TYPE OF SAMPLE (random OR systematic); DEFAULT IS random
#  file             IF SPECIFIED, A RASTER SURFACE WILL BE WRITTEN TO DISK.
#                     THE FILE EXTENTION WILL DICTATE THE RASTER FORMAT.
#  ...              ADDITIONAL ARGUMENTS PASSED TO predict
#
# EXAMPLES: 
#    setwd("C:/ANALYSIS/TEST/RRR")
#    x <- paste(getwd(), paste("elev", "img", sep="."), sep="/")
#    y <- paste(getwd(), paste("precip90", "img", sep="."), sep="/")
#    RasterUpSample(x=x, y=y, p=0.01, sample.type="random", filename="RPREDICT.img")
#      praster <- raster( paste(getwd(), "RPREDICT.img", sep="/"))
#      Y <- raster(paste(getwd(), paste("precip90", "img", sep="."), sep="/"))
#     op <- par(mfrow = c(1, 2))
#        plot(Y)
#        plot(praster) 
#     par(op)
#
# CONTACT: 
#     Jeffrey S. Evans
#     Senior Landscape Ecologist  
#     The Nature Conservancy
#     Central Science/DbyD
#     Laramie, WY 82070 
#     jeffrey_evans@tnc.org
#     (970) 672-6766
##########################################################################
RasterUpSample <- function(x, y, p, sample.type="random", filename=FALSE, ...) {
   if (!require(MASS)) stop("MASS PACKAGE MISSING")
     if (!require(sp)) stop("sp PACKAGE MISSING")
     if (!require(raster)) stop("raster PACKAGE MISSING")
   if (!require(rgdal)) stop("rgdal PACKAGE MISSING")
	  X <- stack(x)
	  Y <- raster(y) 
     if(sample.type == "random") { 
	   print("SAMPLE TYPE RANDOM")
	    SubSamps <- sampleRandom(X, ((nrow(X)*ncol(X))*p), sp=TRUE)
		} 
	 if(sample.type == "systematic") {
       print("SAMPLE TYPE SYSTEMATIC")
      SubSamps <- sampleRegular(X, ((nrow(X)*ncol(X))*p), asRaster=TRUE)	 
      SubSamps <- as(SubSamps, 'SpatialPointsDataFrame') 
		}  	 
	  Yvalues <- extract(Y, SubSamps)
    SubSamps@data <- data.frame(SubSamps@data, Y=Yvalues) 
   ( rrr <- rlm(as.formula(paste(names(SubSamps@data)[2], ".", sep=" ~ ")), 
                data=SubSamps@data, scale.est="Huber", psi=psi.hampel, 
                init="lts") )
  if (filename != FALSE) {
  	predict(X, rrr, filename=filename, na.rm=TRUE, progress='window', 
	        overwrite=TRUE, ...)
     print(paste("RASTER WRITTEN TO", filename, sep=": "))			
	}
     print(paste("MEAN RESIDUAL ERROR", round(mean(rrr$residuals), digits=5), sep=": "))
     print(paste("AIC", round(AIC(rrr), digits=5), sep=": "))   
  return(rrr)		
}

###################################################################
# PROGRAM: MultiColinear.R                
# PURPOSE: IDENTIFY MULTI-COLINEAR VARIABLES USING QR MATRIX DECOMPOSITION                   
#
# ARGUMENTS: 
#       X   A DATAFRAME 
#       p   MULTI-COLINEARITY THRESHOLD (DEFAULT 1e-07)
#
# VALUE:
#       TEST STATISTIC MESSAGE
#       CHARACTER VECTOR OF POTENTIAL MULTI-COLINEAR VARIABLES
#
# NOTES:
#       COLINEARITY THRESHOLDS MUST BE ADJUSTED BASED ON NUMBER OF 
#        X-VARIABLES. FOR SMALL NUMBER OF VARIABLES (<20) USE 1e-07 
#        FOR LARGE NUMBER (>20) USE 0.05 
#
# EXAMPLES: 
#   # DUMMY DATA
#   test = data.frame(v1=seq(0.1, 5, length=100), v2=seq(0.1, 5, length=100), 
#                     v3=dnorm(runif(100)), v4=dnorm(runif(100)) ) 
#
#   # TEST FOR MULTICOLINEAR VARABLE(s)
#   MultiColinear(test[,c(1,3)])
#   cl <- MultiColinear(test)
#
#   # PCA BIPLOT OF VARIABLES 
#    pca.test <- prcomp(test[,1:ncol(test)], scale=TRUE)
#    biplot(pca.test, arrow.len=0.1, xlabs=rep(".", length(pca.test$x[,1])))        
#
#   # REMOVE IDENTIFIED VARIABLE(S)
#   test <- test[, -which(names(test)==cl)]
#
# REFERENCES:
#  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. 
#     Wadsworth & Brooks/Cole. 
#
#  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978) 
#     LINPACK Users Guide. Philadelphia: SIAM Publications. 
#
##########################################################################
MultiColinear <- function(x, p=1e-07) {
 if (!inherits(x, "data.frame")) stop("X MUST BE A data.frame")
   if ( (dim(x)[2] < 2) == TRUE) stop("NOT ENOUGH VARIABLES TO TEST")
     xtest <- x
     x.names <- names(xtest)
  qrx <- qr(xtest, tol=p)
    if (length(names(xtest)[qrx$pivot[1:qrx$rank]]) != length(xtest) )  
      {  
        keep <- names(xtest)[qrx$pivot[1:qrx$rank]]
         warning("MULTI-COLINEAR VARIABLES: ", paste(setdiff(x.names, keep), collapse = ","))
      return(paste(setdiff(x.names, keep)))
    } else { print(" NO MULTICOLINEAR VARIABLES IDENTIFIED")
  } 
}

##########################################################################
# PROGRAM: rf.modelSel (FOR CLASSIFICATION OR REGRESSION)
# USE: RANDOM FOREST MODEL SELECTION USING SCALED IMPORTANCE VALUES
# REQUIRES: >= R 2.14.0, randomForest 4.6-7 
#           
# ARGUMENTS: 
#       ydata      Y Data for model 
#       xdata      X Data for model
#       imp.scale  Type of scaling for importance values (mir or se), default is mir
#       r          Vector of importance percentiles to test i.e., c(0.1, 0.2, 0.5, 0.7, 0.9)
#       final      Run final model with selected variables (TRUE/FALSE)
#       plot.imp   Plot variable importance (TRUE/FALSE)
#       parsimony  THRESHOLD FOR ASSESSING COMPETING MODEL 0-1 SCALE (IF NULL NOT RUN)
#       ...        Arguments to pass to randomForest (e.g., ntree=1000, replace=TRUE, proximity=TRUE)
#
# VALUE:
#     A LIST WITH THE FOLLOWING OBJECTS
#         rf.final - FINAL RF MODEL (LIST OBJECT) USING SELECTED VARIABLES (IF final=TRUE)
#           SELVARS - LIST OF FINAL SELECTED VARIABLES
#           TEST - VALIDATION USED IN MODEL SELECTION
#           IMPORTANCE - IMPORTANCE VALUES FROM SELECTED MODEL
#           PARAMETERS - VARIABLES USED IN EACH TESTED MODEL
#
# NOTES: 
#        IF YOU WANT TO RUN CLASSIFICATION MAKE SURE Y IS A FACTOR
#        OTHERWISE RF RUNS IN REGRESSION MODE
#
#        The mir scale option perfroms a row standardization and the se option performs
#        normalization using The “standard errors” of the permutation-based importance measure.
#        Both options result in a 0-1 range but se summs to 1.  
#                  mir = i/max(i)
#                  se = (i / se) / ( sum(i) / se) 
#
#  IMPORTANCE CANNONT BE FALSE AND IS SET IN THE FUNCTION, SO DO NOT USE IMPORTANCE FLAG
#        
#        For regression the model selection criteria is; largest %variation 
#        explained, smallest MSE, and fewest parameters. 
#         
#        For classification; Smallest OOB error, smallest maximum within 
#        class error, and fewest parameters. 
#
# REFERENCES:
#    Evans, J.S. and S.A. Cushman (2009) Gradient Modeling of Conifer Species 
#      Using Random Forest. Landscape Ecology 5:673-683.
#
#    Murphy M.A., J.S. Evans, and A.S. Storfer (2010) Quantify Bufo boreas 
#      connectivity in Yellowstone National Park with landscape genetics. 
#      Ecology 91:252-261
#
#    Evans J.S., M.A. Murphy, Z.A. Holden, S.A. Cushman (2011). Modeling species 
#      distribution and change using Random Forests CH.8 in Predictive Modeling in 
#      Landscape Ecology eds Drew, CA, Huettmann F, Wiersma Y. Springer 
# 
# EXAMPLES: 
# # CLASSIFICATION
#     data(iris)
#     iris$Species <- as.factor(iris$Species) 
#	 ( rf.class <- rf.modelSel(iris[,1:4], iris[,"Species"], imp.scale="mir") )
#     ( rf.class <- rf.modelSel(iris[,1:4], iris[,"Species"], imp.scale="mir", parsimony=0.03) ) 
#       vars <- rf.class$PARAMETERS[[3]]
#     ( rf.fit <- randomForest(x=iris[,vars], y=iris[,"Species"]) )                           
# # REGRESSION
#     data(airquality)
#     airquality <- na.omit(airquality)
#     ( rf.regress <- rf.modelSel(airquality[,2:6], airquality[,1], imp.scale="se") )
#	 ( rf.regress <- rf.modelSel(airquality[,2:6], airquality[,1], imp.scale="se", parsimony=0.03) )
#       vars <- rf.regress$PARAMETERS[[3]]
#         ( rf.fit <- randomForest(x=airquality[,vars], y=airquality[,1]) )
# # REGRESSION - NOT RUN	 
#    require(sp)	 
#    data(meuse)
#    meuse <- na.omit(meuse)
#   ( rf.regress <- rf.modelSel(meuse[,4:14], meuse[,3], imp.scale="se",
#                               r=c(0.10,0.25,0.50,0.75,0.90)) )	 
#    ( rf.regress <- rf.modelSel(meuse[,4:14], meuse[,3], imp.scale="se", parsimony=0.03,
#                                 r=c(0.10,0.25,0.50,0.75,0.90)) )	 
#    	                               
# CONTACT: 
#     Jeffrey S. Evans 
#     Senior Landscape Ecologist 
#     The Nature Conservancy - Central Science
#     Adjunct Faculty
#     University of Wyoming
#     Laramie, WY
#     (970)672-6766
#     jeffrey_evans@tnc.org
##########################################################################
rf.modelSel <- function(xdata, ydata, imp.scale="mir", r=c(0.25, 0.50, 0.75),  
                        final=FALSE, plot.imp=TRUE, parsimony=NULL, ...) 
  {
 if (!require (randomForest)) stop("randomForest PACKAGE MISSING")
 rf.ImpScale <- function (x, scale="mir") { 
  if (!inherits(x, "randomForest")) 
       stop(deparse(substitute(x)), " Must be a randomForest object")
  if (x$type == "regression") {
   if (is.null(x$importanceSD) == TRUE | "%IncMSE" %in% 
       names(as.data.frame(x$importance)) == FALSE)
        stop("OBJECT DOES NOT CONTAIN PERMUTATED IMPORTANCE, PLEASE RUN 
              randomForest WITH importance=TRUE")   
	rf.imp <- x$importance[,"%IncMSE"]
    rf.impSD <- x$importanceSD
       rf.impSD[rf.impSD == 0] <- 0.000000001	
    if (scale == "mir") {
      i <- rf.imp / max(rf.imp) 
		}	  
    if (scale == "se") {
	  i <- ( rf.imp / rf.impSD ) / sum(rf.imp / rf.impSD, na.rm=TRUE)			 
        }
	 }
  if (x$type == "classification" | x$type == "unsupervised") {
   if (is.null(x$importanceSD) == TRUE | "MeanDecreaseAccuracy" %in% 
       names(as.data.frame(x$importance)) == FALSE)
        stop("OBJECT DOES NOT CONTAIN PERMUTATED IMPORTANCE, PLEASE RUN 
       randomForest WITH importance=TRUE") 
	rf.imp <- x$importance[,"MeanDecreaseAccuracy"]
    rf.impSD <- x$importanceSD[,"MeanDecreaseAccuracy"]
       rf.impSD[rf.impSD == 0] <- 0.000000001	
    if (scale == "mir") {
      i <- rf.imp / max(rf.imp) 
		}	  
    if (scale == "se") {
	  i <- ( rf.imp / rf.impSD ) / sum(rf.imp / rf.impSD, na.rm=TRUE)			 
        }
	 }
 	i <- as.data.frame(i)
	  names(i) <- "importance" 
        row.names(i) <- names(rf.imp)	
   return( i )            
 }
 
RFtype <- is.factor(ydata) #TEST FOR FACTOR IN Y 
##CLASSIFICATION##
if (RFtype == "TRUE") {
    model.vars <- list()
    ln <- 0
    rf.all <- randomForest(x=xdata, y=ydata, importance=TRUE, ...) 
      model.vars[[ln <- ln + 1]] <- rownames(rf.all$importance)  
       class.errors <- as.data.frame(rf.all$err.rate)
        class.errors <- na.omit(class.errors)  
         class.errors[class.errors == NaN] <- 0
          class.errors[class.errors == Inf] <- 1         
        i <- vector()
	      for ( l in 2:nlevels(as.factor(names(class.errors))) ) {              
            i <- append(i, median(class.errors[,l]))
          }        
        max.error = max(i) 
	imp <- rf.ImpScale(rf.all, scale=imp.scale) 
    results <- as.data.frame(array(0, dim=c( 0, 4 )))
      x <- c(0, (median(rf.all$err.rate[,"OOB"]) * 100), max.error * 100, dim(xdata)[2] )
    results <- rbind(results, x) 	 	 
     for (p in 1:length(r) ) {
		 t = quantile(imp[,1], probs=r[p])
         sel.imp <- subset(imp, importance >= t)
           sel.vars <- rownames(sel.imp)
     if (length( sel.vars ) > 1) {                             
         xdata.sub <- xdata[,sel.vars]       
      rf.model <- randomForest(x=xdata.sub, y=ydata, importance=TRUE)          
           class.errors <- as.data.frame(rf.model$err.rate)
            class.errors <- na.omit(class.errors)  
             class.errors[class.errors == NaN] <- 0
              class.errors[class.errors == Inf] <- 1      
        i <- as.vector(array(0, dim=c((0),(1))))
       for ( l in 2:nlevels(as.factor(names(class.errors))) )
          {
          x.bar <- mean(class.errors[,l])              
            i <- as.vector(append(i, x.bar, after=length(i) ))
            }        
         max.error = max(i[2:length(i)] )     
         x <- c(t, median(rf.model$err.rate[,1]) * 100, max.error * 100, length(sel.vars) )
         results <- rbind(results, x)
		  model.vars[[ln <- ln + 1]] <- rownames(rf.model$importance)
         }
        }
  names(results) <- c("THRESHOLD", "OOBERROR", "CLASS.ERROR", "NPARAMETERS")
  results <- results[order(results$CLASS.ERROR, results$OOBERROR, results$NPARAMETERS),]
    if (is.null(parsimony) == FALSE) { 
	  if(parsimony < 0.00000001 | parsimony > 0.9) stop( "parsomony MUST RANGE 0-1")
        oob <- "TRUE"
        for(i in 2:nrow(results)) {
          if( abs((results[i,][2] - results[1,][2] ) / results[1,][2]) <= parsimony  &
              abs( (results[i,][3] - results[1,][3] ) / results[1,][3] ) <= parsimony ) {
            oob <- append(oob, "TRUE")
        	  } else {
        	oob <- append(oob, "FALSE")
            }
            final <- results[which( oob == "TRUE" ),]
        	  final <- final[final$NPARAMETERS == min(final$NPARAMETERS) ,]$THRESHOLD
        }
          } else {		
            final <- as.vector(results[,"THRESHOLD"])[1]
        }	
      sel.imp <- subset(imp, importance >= final)    
        sel.vars <- rownames(sel.imp)
          sel.post=which( results$NPARAMETERS == length(sel.vars) ) 
      results <- rbind(results[sel.post,],results[-sel.post,]) 	
  } # END OF CLASSIFICATION
  
##REGRESSION## 
if (RFtype == "FALSE") {
    model.vars <- list()
      ln <- 0      
    rf.all <- randomForest(x=xdata, y=ydata, importance=TRUE, ...) 
	  model.vars[[ln <- ln + 1]] <- rownames(rf.all$importance)  
	   imp <- rf.ImpScale(rf.all, scale=imp.scale) 
      results <- as.data.frame(array(0, dim=c( 0, 4 )))
     x <- c(0, (median(rf.all$rsq)), mean(rf.all$mse), dim(xdata)[2] )
     results <- rbind(results, x)     
   for (p in 1:length(r) ) {
        t = quantile(imp[,1], probs=r[p])		 
        sel.vars <- rownames(subset(imp, importance >= t))  
     if (length( sel.vars ) > 1) {                             
      xdata.sub <- as.data.frame(xdata[,sel.vars]) 
      rf.model <- randomForest(x=xdata.sub, y=ydata, importance=TRUE, ...)          
      x <- c(t, (median(rf.model$rsq)), mean(rf.model$mse), length(sel.vars) )
      results <- rbind(results, x)
	     model.vars[[ln <- ln + 1]] <- rownames(rf.model$importance)
      }
    }
   names(results) <- c("THRESHOLD", "VAREXP", "MSE", "NPARAMETERS")
     results <- results[order(-results$VAREXP, results$MSE, results$NPARAMETERS),]  
    if (!is.null(parsimony)) {
      if(parsimony < 0.00000001 | parsimony > 0.9) stop( "parsomony MUST RANGE 0-1")	
        oob <- "TRUE"
        for(i in 2:nrow(results)) {
          if( abs((results[i,][2] - results[1,][2] ) / results[1,][2]) <= parsimony  &
              abs( (results[i,][3] - results[1,][3] ) / results[1,][3] ) <= parsimony ) {
            oob <- append(oob, "TRUE")
        	  } else {
        	oob <- append(oob, "FALSE")
            }
            final <- results[which( oob == "TRUE" ),]
        	  final <- final[final$NPARAMETERS == min(final$NPARAMETERS) ,]$THRESHOLD
        }
          } else {		
            final <- as.vector(results[,"THRESHOLD"])[1]
        }	
      sel.imp <- subset(imp, importance >= final)    
        sel.vars <- rownames(sel.imp)
        sel.post=which( results$NPARAMETERS == length(sel.vars) ) 
      results <- rbind(results[sel.post,],results[-sel.post,]) 			
   } # END OF REGRESSION 
   
    if (plot.imp == TRUE) {
        if (imp.scale=="mir") {lable="Row Standardization Variable Importance"} 	
		  if (imp.scale=="se") {lable="Standardized Error Variable Importance"}
	  p <- as.matrix(subset(imp, importance >= final))    
       ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]])  
       dotchart(p[ord,1], main=lable, pch=19)
    }
	
    if (final == TRUE) {
       sub.xdata <- xdata[,sel.vars]  #SUBSET VARIABLES
        rf.final <- randomForest(x=sub.xdata, y=ydata, importance=TRUE, ...)           
      ( list(MODEL=rf.final, SELVARS=sel.vars, TEST=results, IMPORTANCE=sel.imp, PARAMETERS=model.vars) )      
         } else {
      ( list(SELVARS=sel.vars, TEST=results, IMPORTANCE=sel.imp, PARAMETERS=model.vars) ) 
    }     
 }

######################################################################################
# PROGRAM: rf.ClassBalance (FOR CLASSIFICATION)
# USE: CREATES A BALANCED SAMPLE IN A RANDOM FORESTS CLASSIFICATION MODEL 
# REQUIRES: R 2.15.0, randomForest 4.6-5 (CURRENT TESTED VERSIONS) 
#                                                                                                                                                                                                     
# ARGUMENTS:  
#       ydata      RESPONSE VARIABLE  (i.e., [,2] or [,"SPP"] )                        
#       xdata      INDEPENDENT VARIABLES(S)  (i.e., [,3:14] or [3:ncol(data)] )
#       p          P-VALUE OF COVARIANCE CONVERGENCE TEST (DO NOT RECCOMEND CHANGING)
#       cbf        FACTOR USED TO TEST IF MODEL IS IMBALANCED, DEFAULT IS SIZE OF 
#                    MINORITY CLASS * 3                                              
#       ...        ADDITIONAL ARGUMENTS TO PASS TO RANDOM FOREST                          
# 
# VALUE:  
#      A LIST OBJECT WITH RF MODEL OBJECT, CUMMLIATIVE OOB ERROR,        
#        CUMMLIATIVE CONFUSION MATRIX, AND PERCENT CORRECT CLASSIFIED  
#
# NOTES:
#     RUNS MODEL WITH RANDOM SUBSET OF MAJORITY CLASS UNTIL                   
#       COVARIANCE CONVERGES TO FULL DATA. PREDECTION IS BASED ON                 
#       COMBINING SUBSET MODELS TO CREATE FINAL ENSEMBLE
#
# REFERENCES:
#    Evans, J.S. and S.A. Cushman (2009) Gradient Modeling of Conifer Species 
#      Using Random Forest. Landscape Ecology 5:673-683.
#
#    Evans J.S., M.A. Murphy, Z.A. Holden, S.A. Cushman (2011). Modeling species 
#      distribution and change using Random Forests CH.8 in Predictive Modeling in 
#      Landscape Ecology eds Drew, CA, Huettmann F, Wiersma Y. Springer 
#                                                                       
# EXAMPLES: 
#      rfClassBalance(ydata=data[,1], xdata=data[,3:ncol(data)], ntree=100)       
#                                                                                       
# CONTACT:
#     Jeffrey S. Evans
#     Senior Landscape Ecologist  
#     The Nature Conservancy
#     Central Science/DbyD
#     Affiliate Assistant Professor
#     Environment and Natural Resources
#     University of Wyoming
#     Laramie, WY 82070 
#     jeffrey_evans@tnc.org
#     (970) 672-6766
######################################################################################
rfClassBalance <- function (ydata, xdata, p=0.005, cbf=3,...) 
 {
 if (!require (randomForest)) stop("randomForest PACKAGE MISSING")
  if (  class(ydata) != "factor" ) { ydata <- as.factor(ydata) }
  CompCov <- function(m1, m2, pVal=p) {
       k = 2
        p = 2
         n1 = dim(m1)[1]
          n2 = dim(m2)[1] 
           n = n1 + n2
            s1 <- crossprod(m1[1:dim(m1)[1]])
             s2 <- crossprod(m2[1:dim(m2)[1]])
              c1 = (1/(n1-1))*s1
              c2 = (1/(n2-1))*s2
             c = (s1+s2)/(n-k)
            d = det(c)
            d1 = det(c1)
           d2 = det(c2) 
          m = ( (n-k)*log(d) ) - ( (n1-1)*log(d1) + (n2-1)*log(d2) )
         h = 1-((2*p*p+3*p-1) / (6*(p+1)*(k-1)) * (1 / (n1-1)+1 / (n2-1)+1 / (n-k)))
        chi = round(abs(m*h),digits=6)
       df = p*(p+1)*(k-1)/2
       print( paste("EQUIVALENCE p", chi, sep=": ") )
          if ( (chi <= pVal ) == TRUE & (i > 2) |  (i > 20)  == TRUE ) { 
               ( "TRUE" )
                  } else {
               ( "FALSE" ) 
             }
         }  		 
    y <- ydata
    x <- xdata  		 		 
    class.ct <- as.numeric()		 
      for(i in 1:nlevels(y)) {
        class.ct <- append(class.ct, length(which(y==levels(y)[i])), 
    	                   after=length(class.ct) )
        }
        maj.post <- which.max(class.ct)
    	  maj.class <- levels(ydata) [maj.post]
        min.post <- which.min(class.ct)
    	  min.class <- levels(ydata) [min.post]
    		
    if ( ( class.ct[maj.post] <= class.ct[min.post] * cbf ) == TRUE) 
            stop("CLASSES ARE BALANCED!")      
    tmp.data <- data.frame(y, x)
	   majority <- tmp.data[tmp.data[,"y"] == maj.class ,]       
       minority <- tmp.data[tmp.data[,"y"] == min.class ,]    
	   all.cov <- cov(majority[,names(x)])     
	  test <- as.data.frame(array(0, dim=c( 0, dim(tmp.data)[2] )))
        names(test) <- names(majority) 
     if ( !is.na(match("rf.model",ls()))) rm(rf.model)
        n <- dim(minority)[1]*2                 
  i=0  
    converge = c("FALSE")  
     while (converge != "TRUE" )
       {
       i=i+1
        ns <- sample(1:nrow(majority), n) 
        class.sample <- majority[ns, ]
          mdata <- rbind(minority, class.sample)   
        if (  class(mdata[,1]) != "factor" ) 
                   { mdata[,1] <- as.factor(mdata[,1]) }
        if ( !is.na(match("rf.model",ls()))) {               
            rf.fit <- randomForest(x=mdata[,2:ncol(mdata)], y=mdata[,1])                           
            rf.model <- combine(rf.fit, rf.model)           
               OOB <- ( OOB + median(rf.fit$err.rate[,1]) ) 
               CM <- (CM + rf.fit$confusion)                 
               confusion <- confusion + rf.fit$confusion 
               } else {
            rf.model <- randomForest(x=mdata[,2:ncol(mdata)], y=mdata[,1])  
               OOB <- median(rf.model$err.rate[,1]) 
               CM <- rf.model$confusion           
               confusion <- rf.model$confusion                          
         }
      test <- rbind(test, class.sample)    
         test.cov <- cov( test[,names(x)] )
         converge <- CompCov(all.cov, test.cov)  
  }
    OOB <- OOB / i
    CM[,3] <- CM[,3] / i
    PCC <- ( sum(diag(CM))/sum(CM) ) * 100
   list( MODEL=rf.model, OOB_ERROR=OOB, CONFUSION=CM, PCT_CC=PCC )
}

##########################################################################
# PROGRAM: rf.Permutation (FOR REGRESSION)
# USE: RANDOM PERMUTATION TO TEST SIGNIFICANCE OF randomForest REGRESSION MODEL
# REQUIRES: >= R 2.15.0, randomForest 4.6-7 
#           
# ARGUMENTS: 
#      x         DEPENDENT VARABLE(S) TO USE IN MODEL
#      y         DEPENDENT VARIABLE TO USE IN MODEL
#      p         p VALUE TO TEST FOR MODEL SIGNIFICANCE
#      q         QUANTILE THRESHOLD FOR PLOT
#      nperm     NUMBER OF PERMUTATIONS
#      plot      SHOULD THE RESULTS BE PLOTTED (DOTTED LINE REPRESENT TEST p-VALUE)
#      ...       ADDITIONAL randomForest ARGUMENTS 
#
# VALUE:
#    A LIST WITH THE FOLLOWING OBJECTS
#       RandRsquare      VECTOR OF RANDOM R-SQUARE VALUES
#       Rsquare          R SQUARE OF TRUE MODEL  
#       Accept           IS THE MODEL SIGNIFICANT AT SPECIFIED p VALUE
#       TestQuantile     QUANTILE THRESHOLD USED IN SIGNIFICANCE PLOT
#       pValueThreshold  SIGNIFICANCE VALUE
#       pValue           P VALUE OF RANDOMIZATIONS
#       nPerm            NUMBER OF PERMUTATIONS          
#
# REFERENCES:
#    Murphy M.A., J.S. Evans, and A.S. Storfer (2010) Quantify Bufo boreas 
#      connectivity in Yellowstone National Park with landscape genetics. 
#      Ecology 91:252-261
#
#    Evans J.S., M.A. Murphy, Z.A. Holden, S.A. Cushman (2011). Modeling species 
#      distribution and change using Random Forests CH.8 in Predictive Modeling in 
#      Landscape Ecology eds Drew, CA, Huettmann F, Wiersma Y. Springer 
# 
# EXAMPLES: 
#    require(randomForest)
#    data(airquality)
#    airquality <- na.omit(airquality)
#      ( rf.test <- rf.Permutation(x=airquality[,2:6], y=airquality[,1], nperm=999) )
#                               
# CONTACT: 
#     Jeffrey S. Evans 
#     Senior Landscape Ecologist 
#     The Nature Conservancy - Central Science
#     Adjunct Faculty
#     University of Wyoming
#     Laramie, WY
#     (970)672-6766
#     jeffrey_evans@tnc.org
##########################################################################
rf.Permutation <- function(x, y, q=0.99, p=0.05, nperm=999, plot=TRUE, ...) {
  if (!require (randomForest)) stop("randomForest PACKAGE MISSING")
    if (is.factor(y)) stop("y CANNOT BE A FACTOR") 
      rf.test <- randomForest(x=x, y=y, ...)
	    test.rsq <- median(rf.test$rsq)
	rand.dist <- vector() 
      for( i in 1:nperm) {	
        rand.y <- sample(y, length(y)) 
          rf.test <- randomForest(x=x, y=rand.y, ...)
            rand.dist <- append(rand.dist, median(rf.test$rsq)) 
        }	
       Pval <- function(x, test, nperm) { 
         if ( length( x[x >= test] ) < 1 ) { 
	           error = 1 
	         } else { 
	   	    error = length( x[x >= test] ) + 1
	   	   }	
          return( error / nperm )
        }				
	  if( plot == TRUE) { 
	    den=density(rand.dist)
          den$y <- den$y/max(den$y)		
	        plot(den, type="n", xlim=c(min(rand.dist), 1), xlab="R-square", ylab="",  
	  	         main="Distribution of randomized models")
                   polygon(den, col="blue")
                     abline(v=test.rsq, col="black", lwd=1.5, lty=2)
					 abline(v=quantile(rand.dist,p=q),lwd=1.5, lty=2, col="red") 
              legend("topright", c("model", "null"), bg="white",  
		             col=c("black","red"), lty=c(2,2), lwd=c(1.5,1.5) )				   
	    }
	  pValue=round(Pval(x=rand.dist, test=test.rsq, nperm=nperm), digits=6)	
	if( pValue <= p ) accept=TRUE else accept=FALSE 
      if (accept == TRUE) accept <- paste("MODEL SIGNIFICANT AT p=", pValue, sep="" ) 
	    if (accept == FALSE) accept <- paste("MODEL NOT SIGNIFICANT AT p= ", pValue, sep="" )
    print(accept)		
  return( list(RandRsquare=rand.dist, Rsquare=test.rsq, Accept=accept, TestQuantile=q, 
               pValueThreshold=p, pValue=pValue, nPerm=nperm) )
} 

##########################################################################
# PROGRAM: PlotROC
# USE: PLOTS A ROC FOR A BINARY PROBLEM AND REPORTS VALIDATION STATS
# REQUIRES: TWO ORDERED VECTORS OF OBSERVED AND PREDICTED VALUES
#               PACKAGE "ROCR"
#
# ARGS: predictedValues      Predicted values
#       actualValues         Observed values
#       cutoff               Method to calculate cutoff
#       cutoffValue          Critical cutoff value
#       colorize             Plot in color
#       title                Plot title
#       summaryFile          Object contaning validation statistics
#       bg                   Background plot color
#
# EXAMPLE: PlotROC(model[,"PRED"], model[,"OBS"], cutoff="automatic", .title="ROC PLOT" )
#              
# CONTACT: 
#     Jeffrey S. Evans, Ph.D.
#     Senior Landscape Ecologist  
#     The Nature Conservancy
#     Central Science/DbyD
#     Affiliate Assistant Professor
#     Environment and Natural Resources
#     University of Wyoming
#     Laramie, WY 82070 
#     jeffrey_evans@tnc.org
#     (970) 672-6766
##########################################################################
PlotROC <- function(predictedValues, actualValues, cutoff="automatic", cutoffValue=NULL, 
                    colorize=TRUE, .title=NULL, summaryFile=NULL, bg="white")
{
    require(ROCR)
    pred <- prediction(predictedValues, actualValues)
    
    # Create the ROC performance object. 
    perf <- performance(pred, "tpr", "fpr")
    
    # Calculate model summary statistics.
    nLabel <- min(actualValues)                   
    pLabel <- max(actualValues)                   
    
    auc <- performance(pred, "auc")@y.values[[1]]
    
    if ((nLabel == 0 || nlabel == 1) && (pLabel == 0 || pLabel == 1))
        mxe <- performance(pred, "mxe")@y.values[[1]]
    else
        mxe <- NA
        
    prbe <- unlist(performance(pred, "prbe")@y.values)
    if (length(prbe) > 0)
        prbe <- prbe[length(prbe)]
    else
        prbe <- NA
        
    if (is.numeric(nLabel))
        rmse <- performance(pred, "rmse")@y.values[[1]]
    else
        rmse <- NA

    messages = vector(mode="character")

    messages = append(messages, "Model summary statistics:")
    messages = append(messages, "")
    messages = append(messages, sprintf("Area under the ROC curve (auc)           = %f", auc))
    if (!is.na(mxe))
        messages = append(messages, sprintf("Mean cross-entropy (mxe)                 = %f", mxe))
    messages = append(messages, sprintf("Precision-recall break-even point (prbe) = %f", prbe))
    messages = append(messages, sprintf("Root-mean square error (rmse)            = %f", rmse))
    
    # If asked us to calculate the cutoff, calculate it.
    if (cutoff == "automatic")
    {
        distFromPerfect <- sqrt(unlist(perf@x.values)^2 + (1 - unlist(perf@y.values))^2)
        cutoffValue <- unlist(perf@alpha.values)[which.min(distFromPerfect)]
    }
    
    # If there is a cutoff, calculate the contingency table. 
    if (cutoff != "none")
    {
        tn = length(which((predictedValues < cutoffValue) & (actualValues == nLabel)))
        fn = length(which((predictedValues < cutoffValue) & (actualValues != nLabel)))
        tp = length(which((predictedValues >= cutoffValue) & (actualValues == pLabel)))
        fp = length(which((predictedValues >= cutoffValue) & (actualValues != pLabel)))
        
        messages = append(messages, "")
        messages = append(messages, sprintf("Contingency table for cutoff = %f:", cutoffValue))
        messages = append(messages, "")
        messages = append(messages, "             Actual P  Actual N     Total")
        messages = append(messages, sprintf("Predicted P %9i %9i %9i", tp, fp, tp+fp))
        messages = append(messages, sprintf("Predicted N %9i %9i %9i", fn, tn, tn+fn))
        messages = append(messages, sprintf("      Total %9i %9i %9i", tp+fn, fp+tn, tp+fn+fp+tn))
        messages = append(messages, "")

        tn = as.double(tn)
        fn = as.double(fn)
        tp = as.double(tp)
        fp = as.double(fp)
        acc = (tp+tn)/(tp+fp+tn+fn)

        messages = append(messages, sprintf("Accuracy (acc)                                = %f", acc))
        messages = append(messages, sprintf("Error rate (err)                              = %f", (fp+fn)/(tp+fp+tn+fn)))
        messages = append(messages, sprintf("Rate of positive predictions (rpp)            = %f", (tp+fp)/(tp+fp+tn+fn)))
        messages = append(messages, sprintf("Rate of negative predictions (rnp)            = %f", (tn+fn)/(tp+fp+tn+fn)))
        messages = append(messages, "")
        messages = append(messages, sprintf("True positive rate (tpr, or sensitivity)      = %f", tp/(tp+fn)))
        messages = append(messages, sprintf("False positive rate (fpr, or fallout)         = %f", fp/(fp+tn)))
        messages = append(messages, sprintf("True negative rate (tnr, or specificity)      = %f", tn/(fp+tn)))
        messages = append(messages, sprintf("False negative rate (fnr, or miss)            = %f", fn/(tp+fn)))
        messages = append(messages, "")
        messages = append(messages, sprintf("Positive prediction value (ppv, or precision) = %f", tp/(tp+fp)))
        messages = append(messages, sprintf("Negative prediction value (npv)               = %f", tn/(tn+fn)))
        messages = append(messages, sprintf("Prediction-conditioned fallout (pcfall)       = %f", fp/(tp+fp)))
        messages = append(messages, sprintf("Prediction-conditioned miss (pcmiss)          = %f", fn/(tn+fn)))
        messages = append(messages, "")
        messages = append(messages, sprintf("Matthews correlation coefficient (mcc)        = %f", (tp*tn - fp*fn)/sqrt((tp+fn)*(fp+tn)*(tp+fp)*(fn+tn))))
        messages = append(messages, sprintf("Odds ratio (odds)                             = %f", (tp*tn)/(fn*fp)))
        messages = append(messages, sprintf("SAR                                           = %f", (acc + auc + rmse)/3))
    }
    else
    {
        cutoffValue = NA
        tp = NA
        fp = NA
        tn = NA
        fn = NA
    }
    
    # Output the messages, optionally to the summaryFile.
    writeLines("")
    writeLines(messages)
    writeLines("")
    
    if (!is.null(summaryFile))
        writeLines(messages, summaryFile)
    
    # Create the ROC plot.
    plot(perf, colorize=colorize, lwd=5, main=.title)
    abline(0, 1, lty=2)
    if (cutoff != "none")
    {
        tpr = tp/(tp+fn)
        fpr = fp/(fp+tn)
        if (colorize)
            points(x=fpr, y=tpr, cex=1.5, lwd=2)
        else
            points(x=fpr, y=tpr, pch=21, cex=1.5, lwd=2, bg=bg)
        text(x=fpr+0.01, y=tpr-0.01, labels=sprintf("Cutoff = %f", cutoffValue), adj=c(0,1))
    }
    
    # Return successfully.
    return(c(cutoffValue, tp, fp, fn, tn, auc, mxe, prbe, rmse))
}

##########################################################################
# PROGRAM: ClassificationValidation.R
# USE: CALCULATES VALIDATION STATISTICS ON CLASSIFICATION MODELS
# REQUIRES: A CONFUSION MATRIX OR VECTOR(s) OF OBSERVED AND PREDICTED-CLASS 1...n
#
# Default method for obs/pred data
# confusionMatrix(data, reference, positive = NULL, dnn = c("Prediction", "Reference"), 
#                prevalence = NULL, ...)
#
# method for class 'table':
# confusionMatrix(data, positive = NULL, prevalence = NULL, ...)
#
# ARGUMENTS: 
# data - a factor of predicted classes (for the default method) or an object of class table.
# reference - a factor of classes to be used as the true results
# positive - an optional character string for the factor level that corresponds to a "positive" 
#            result (if that makes sense for your data). If there are only two factor levels, 
#            the first level will be used as the "positive" result.
# dnn - a character vector of dimnames for the table
# prevalence - a numeric value or matrix for the rate of the "positive" class of the data. 
#              When data has two levels, prevalence should be a single numeric value. Otherwise, 
#              it should be a vector of numeric values with elements for each class. The vector 
#              should have names corresponding to the classes.
#
# DETAILS
#   For two class problems, the sensitivity, specificity, positive predictive value and negative 
#   predictive value is calculated using the positive argument. Also, the prevalence of the "event" 
#   is computed from the data (unless passed in as an argument), the detection rate (the rate of true 
#   events also predicted to be events) and the detection prevalence (the prevalence of predicted events).
#
# 2x2 table with notation	
#  Predicted 	  Pres   Abs
#         Pres 	  A 	  B
#         Abs     C 	  D
#
# Formulas:
#   Sensitivity = A/(A+C)
#   Specificity = D/(B+D)
#   Prevalence = (A+C)/(A+B+C+D)
#   PPV = (sensitivity * Prevalence)/((sensitivity*Prevalence) + ((1-specificity)*(1-Prevalence)))
#   NPV = (specificity * (1-Prevalence))/(((1-sensitivity)*Prevalence) + ((specificity)*(1-Prevalence)))
#   Detection Rate = A/(A+B+C+D)
#   Detection Prevalence = (A+B)/(A+B+C+D)
#
#   The overall accuracy and unweighted Kappa statistic are calculated. The overall accuracy rate is 
#   computed along with a 95 percent confidence interval for this rate (using binom.test) and a one-sided 
#   test to see if the accuracy is better than the "no information rate," which is taken to be the largest 
#   class percentage in the data. For more than two classes, these results are calculated comparing each 
#   factor level to the remaining levels (i.e. a "one versus all" approach).  
#
# EXAMPLES:
#  Default method using two columns
#   confusionMatrix(data[,"OBSERVED"], data[,"PREDICTED"])
#
#  Class "table" for use with defined confusion matrix 
#    confusion = as.table(rbind(c(505, 122),c(6, 23)))
#      rownames(confusion) <- c("ABS","PRES")
#      colnames(confusion) <- c("ABS","PRES")
#    confusionMatrix(confusion)
#      
#  Using two values resulting from Boolean statment
#    agree=25000
#    disagree=5000
#      confusion = as.table(rbind(c(agree, disagree),c(disagree, agree)))
#        rownames(confusion) <- c("AGREE","DISAGREE")
#        colnames(confusion) <- c("AGREE","DISAGREE")
#    confusionMatrix(confusion)
#
# CONTACT: 
#     Jeffrey S. Evans, Ph.D.
#     Senior Landscape Ecologist  
#     The Nature Conservancy
#     Central Science/DbyD
#     Affiliate Assistant Professor
#     Environment and Natural Resources
#     University of Wyoming
#     Laramie, WY 82070 
#     jeffrey_evans@tnc.org
#     (970) 672-6766
##########################################################################
posPredValue <- function(data, ...){
    UseMethod("posPredValue")
  }
"posPredValue.default" <- function(data, reference, positive = levels(reference)[1], prevalence = NULL, ...)
{
  if(!is.factor(reference) | !is.factor(data)) 
    stop("inputs must be factors")
  
  if(length(unique(c(levels(reference), levels(data)))) != 2)
    stop("input data must have the same two levels")
  
  lvls <- levels(data) 
  if(is.null(prevalence)) prevalence <- mean(reference == positive)
  sens <- sensitivity(data, reference, positive)
  spec <- specificity(data, reference, lvls[lvls != positive])
  (sens * prevalence)/((sens*prevalence) + ((1-spec)*(1-prevalence)))

}

"posPredValue.table" <- function(data, positive = rownames(data)[1], prevalence = NULL, ...)
{
  ## "truth" in columns, predictions in rows
  if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")
  if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)
      colnames(data) <- rownames(data) <- c("pos", "neg")
      posCol <- which(colnames(tmp) %in% positive)
      negCol <- which(!(colnames(tmp) %in% positive))
      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])      
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      positive <- "pos"
      rm(tmp)
    }
  negative <- colnames(data)[colnames(data) != positive]
  if(is.null(prevalence)) prevalence <- sum(data[, positive])/sum(data)
  
  sens <- sensitivity(data, positive)
  spec <- specificity(data, negative)
    (sens * prevalence)/((sens*prevalence) + ((1-spec)*(1-prevalence)))
}

"posPredValue.matrix" <- function(data, positive = rownames(data)[1], prevalence = NULL, ...)
{
  data <- as.table(data)
  posPredValue.table(data, prevalence = prevalence)
}

negPredValue <- function(data, ...){
    UseMethod("negPredValue")
  }

"negPredValue.default" <- function(data, reference, negative = levels(reference)[2], prevalence = NULL, ...)
{
   if(!is.factor(reference) | !is.factor(data)) 
      stop("input data must be a factor")
   
   if(length(unique(c(levels(reference), levels(data)))) != 2)
      stop("input data must have the same two levels")   

   lvls <- levels(data) 
   if(is.null(prevalence)) prevalence <- mean(reference == lvls[lvls != negative])
   sens <- sensitivity(data, reference, lvls[lvls != negative])
   spec <- specificity(data, reference, negative)
   (spec * (1-prevalence))/(((1-sens)*prevalence) + ((spec)*(1-prevalence)))
}

"negPredValue.table" <- function(data, negative = rownames(data)[-1], prevalence = NULL, ...)
{
  ## "truth" in columns, predictions in rows
  if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")

  if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)
      
     colnames(data) <- rownames(data) <- c("pos", "neg")
      negCol <- which(colnames(tmp) %in% negative)
      posCol <- which(!(colnames(tmp) %in% negative))
      
      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])      
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      negative <- "neg"
      rm(tmp)
    }

  positive <- colnames(data)[colnames(data) != negative]
  if(is.null(prevalence)) prevalence <- sum(data[, positive])/sum(data)
  
  sens <- sensitivity(data, positive)
  spec <- specificity(data, negative)
  (spec * (1-prevalence))/(((1-sens)*prevalence) + ((spec)*(1-prevalence)))

}

"negPredValue.matrix" <- function(data, negative = rownames(data)[-1], prevalence = NULL, ...)
{
  data <- as.table(data)
  negPredValue.table(data, prevalence = prevalence)
}

sensitivity <- function(data, ...){
    UseMethod("sensitivity")
  }

"sensitivity.default" <- function(data, reference, positive = levels(reference)[1], ...)
{
  if(!is.factor(reference) | !is.factor(data)) 
    stop("inputs must be factors")

  ## todo: relax the =2 constraint and let ngative length be > 2
  if(length(unique(c(levels(reference), levels(data)))) != 2)
    stop("input data must have the same two levels")
  
  numer <- sum(data %in% positive & reference %in% positive)
  denom <- sum(reference %in% positive)
  sens <- ifelse(denom > 0, numer / denom, NA)
  sens
}

"sensitivity.table" <- function(data, positive = rownames(data)[1], ...)
{
  ## "truth" in columns, predictions in rows
  if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")

  if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)
      
      colnames(data) <- rownames(data) <- c("pos", "neg")
      posCol <- which(colnames(tmp) %in% positive)
      negCol <- which(!(colnames(tmp) %in% positive))
      
      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])      
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      positive <- "pos"
      rm(tmp)
    }

  numer <- sum(data[positive, positive])
  denom <- sum(data[, positive])
  sens <- ifelse(denom > 0, numer / denom, NA)
  sens
}

"sensitivity.matrix" <- function(data, positive = rownames(data)[1], ...)
{
  data <- as.table(data)
  sensitivity.table(data)
}

specificity <- function(data, ...){
    UseMethod("specificity")
  }

"specificity.default" <- function(data, reference, negative = levels(reference)[-1], ...)
{
   if(!is.factor(reference) | !is.factor(data)) 
      stop("input data must be a factor")

   ## todo: relax the =2 constraint and let ngative length be > 2
   if(length(unique(c(levels(reference), levels(data)))) != 2)
      stop("input data must have the same two levels")   
   
   numer <- sum(data %in% negative & reference %in% negative)
   denom <- sum(reference %in% negative)
   spec <- ifelse(denom > 0, numer / denom, NA)  
   spec
}

"specificity.table" <- function(data, negative = rownames(data)[-1], ...)
{
  ## "truth" in columns, predictions in rows
  if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")

  if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)
      
      colnames(data) <- rownames(data) <- c("pos", "neg")
      negCol <- which(colnames(tmp) %in% negative)
      posCol <- which(!(colnames(tmp) %in% negative))
      
      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])      
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      negative <- "neg"
      rm(tmp)
    }
  
  numer <- sum(data[negative, negative])
  denom <- sum(data[, negative])
  spec <- ifelse(denom > 0, numer / denom, NA)  
  spec
}

"specificity.matrix" <- function(data, negative = rownames(data)[-1], ...)
{
  data <- as.table(data)
  specificity.table(data)
}

confusionMatrix <- function(data, ...){
   UseMethod("confusionMatrix")
  }
confusionMatrix.default <- function(data, reference, positive = NULL,
                                    dnn = c("Prediction", "Reference"),
                                    prevalence = NULL, ...)
{
  #library(e1071)
  if(!is.factor(data)) data <- factor(data)
  if(!is.factor(reference)) reference <- factor(reference)
  if(!is.character(positive) & !is.null(positive)) stop("positive argument must be character")

  if(length(levels(data)) != length(levels(reference)))
    stop("the data and reference factors must have the same number of levels")
  
  if(any(levels(data) != levels(reference)))
    stop("the data and reference values must have exactly the same levels")
  
  classLevels <- levels(data)
  numLevels <- length(classLevels)
  if(numLevels < 2) 
    stop("there must be at least 2 factors levels in the data")
  
  if(numLevels == 2 & is.null(positive))  positive <- levels(reference)[1]
  
  classTable <- table(data, reference, dnn = dnn, ...)
  
  confusionMatrix(classTable, positive, prevalence = prevalence)
}

print.confusionMatrix <- function(x, digits = max(3, getOption("digits") - 3), printStats = TRUE, ...)
{
   cat("Confusion Matrix and Statistics\n\n") 
   print(x$table, ...)
   if(printStats)
   {
      overall <- signif(x$overall, digits = digits)
      accCI <- paste("(", paste(overall[ c("AccuracyLower", "AccuracyUpper")],
                     collapse = ", "), ")", sep = "")      
      overallText <- c(paste(overall["Accuracy"]), accCI,
                       paste(overall[c("AccuracyNull", "AccuracyPValue")]),
                       "", paste(overall["Kappa"]))
      overallNames <- c("Accuracy", "95% CI",
                        "No Information Rate",
                        "P-Value [Acc > NIR]",
                        "",
                        "Kappa")
                        
      if(dim(x$table)[1] > 2)
      {
         cat("\nOverall Statistics\n")
         overallNames <- ifelse(overallNames == "", "", paste(overallNames, ":"))
         out <- cbind(format(overallNames, justify = "right"), overallText)
         colnames(out) <- rep("", ncol(out))
         rownames(out) <- rep("", nrow(out))
         
         print(out, quote = FALSE)
         
         cat("\nStatistics by Class:\n\n")
         print(t(x$byClass), digits = digits)
         
      } else {

         overallText <- c(overallText, "", paste(signif(x$byClass, digits = digits)))
         overallNames <- c(overallNames, "", names(x$byClass))
         overallNames <- ifelse(overallNames == "", "", paste(overallNames, ":"))
         overallNames <- c(overallNames, "", "'Positive' Class :")
         overallText <- c(overallText, "", x$positive)
         out <- cbind(format(overallNames, justify = "right"),overallText)
         colnames(out) <- rep("", ncol(out))
         rownames(out) <- rep("", nrow(out))
         out <- rbind(out, rep("", 2))
         print(out, quote = FALSE)
      }        
   }
   invisible(x)   
}

classAgreement <- function (tab, match.names=FALSE) 
{
    n <- sum(tab)
    ni <- apply(tab, 1, sum)
    nj <- apply(tab, 2, sum)

    ## patch for matching factors
    if (match.names && !is.null(dimnames(tab))) {
      lev <- intersect (colnames (tab), rownames(tab))
      p0 <- sum(diag(tab[lev,lev]))/n
      pc <- sum(ni[lev] * nj[lev])/n^2
    } else { # cutoff larger dimension
      m <- min(length(ni), length(nj))
      p0 <- sum(diag(tab[1:m, 1:m]))/n
      pc <- sum(ni[1:m] * nj[1:m])/n^2
    }
    n2 <- choose(n, 2)
    rand <- 1 + (sum(tab^2) - (sum(ni^2) + sum(nj^2))/2)/n2
    nis2 <- sum(choose(ni[ni > 1], 2))
    njs2 <- sum(choose(nj[nj > 1], 2))
    crand <- (sum(choose(tab[tab > 1], 2)) - (nis2 * njs2)/n2)/((nis2 + 
        njs2)/2 - (nis2 * njs2)/n2)
    list(diag = p0, kappa = (p0 - pc)/(1 - pc), rand = rand, 
        crand = crand)
}

matchClasses <- function(tab, method = "rowmax", iter=1, maxexact=9,
                         verbose=TRUE){

    methods <- c("rowmax", "greedy", "exact")
    method <- pmatch(method, methods)
    
    rmax <- apply(tab,1,which.max)
    myseq <- 1:ncol(tab)
    cn <- colnames(tab)
    rn <- rownames(tab)
    if(is.null(cn)){
        cn <- myseq
    }
    if(is.null(rn)){
        rn <- myseq
    }
    
    if(method==1){
        retval <- rmax
    }
    if(method==2 | method==3){
        if(ncol(tab)!=nrow(tab)){
            stop("Unique matching only for square tables.")
        }
        dimnames(tab) <- list(myseq, myseq)
        cmax <- apply(tab,2,which.max)
        retval <- rep(NA, ncol(tab))
        names(retval) <- colnames(tab)

        baseok <- cmax[rmax]==myseq
        for(k in myseq[baseok]){
            therow <- (tab[k,])[-rmax[k]]
            thecol <- (tab[, rmax[k]])[-k]            
            if(max(outer(therow, thecol, "+")) < tab[k, rmax[k]]){
                retval[k] <- rmax[k]
            }
            else{
                baseok[k] <- FALSE
            }
        }
        
        if(verbose){
            cat("Direct agreement:", sum(baseok),
                "of", ncol(tab), "pairs\n")
        }
        if(!all(baseok)){
            if(method==3){
                if(sum(!baseok)>maxexact){
                    method <- 2
                    warning(paste("Would need permutation of", sum(!baseok),
                                  "numbers, resetting to greedy search\n"))
                }
                else{
                    iter <- gamma(ncol(tab)-sum(baseok)+1)
                    if(verbose){
                        cat("Iterations for permutation matching:", iter, "\n")
                    }
                    perm <- permutations(ncol(tab)-sum(baseok))
                }
            }
            
            ## rest for permute matching
            if(any(baseok)){
                rest <- myseq[-retval[baseok]]
            }
            else{
                rest <- myseq
            }
            
            for(l in 1:iter){
                newretval <- retval
                if(method == 2){
                    ok <- baseok
                    while(sum(!ok)>1){
                        rest <- myseq[!ok]
                        k <- sample(rest, 1)
                        if(any(ok)){
                            rmax <- tab[k, -newretval[ok]]
                        }
                        else{
                            rmax <- tab[k,]
                        }
                        newretval[k] <- as.numeric(names(rmax)[which.max(rmax)])
                        ok[k] <- TRUE
                    }
                    newretval[!ok] <- myseq[-newretval[ok]]
                }
                else{
                    newretval[!baseok] <- rest[perm[l,]]
                }
                
                if(l>1){
                    agree <- sum(diag(tab[,newretval]))/sum(tab)
                    if(agree>oldagree){
                        retval <- newretval
                        oldagree <- agree
                    }
                }
                else{
                    retval <- newretval
                    agree <- oldagree <- sum(diag(tab[,newretval]))/sum(tab)
                }
            }
        }
    }

    if(verbose){
        cat("Cases in matched pairs:",
            round(100*sum(diag(tab[,retval]))/sum(tab), 2), "%\n")
    }
            
    if(any(as.character(myseq)!=cn)){
        retval <- cn[retval]
    }
    names(retval) <- rn
    
    retval
}

compareMatchedClasses <- function(x, y,
                                  method="rowmax", iter=1, maxexact=9,
                                  verbose=FALSE)
{
    if(missing(y)){
        retval <- list(diag=matrix(NA, nrow=ncol(x), ncol=ncol(x)),
                       kappa=matrix(NA, nrow=ncol(x), ncol=ncol(x)),
                       rand=matrix(NA, nrow=ncol(x), ncol=ncol(x)),
                       crand=matrix(NA, nrow=ncol(x), ncol=ncol(x)))
        for(k in 1:(ncol(x)-1)){
            for(l in (k+1):ncol(x)){
                tab <- table(x[,k], x[,l])
                m <- matchClasses(tab, method=method, iter=iter,
                                  verbose=verbose, maxexact=maxexact)
                a <- classAgreement(tab[,m])
                retval$diag[k,l] <- a$diag
                retval$kappa[k,l] <- a$kappa
                retval$rand[k,l] <- a$rand
                retval$crand[k,l] <- a$crand
            }
        }
    }
    else{
        x <- as.matrix(x)
        y <- as.matrix(y)
        retval <- list(diag=matrix(NA, nrow=ncol(x), ncol=ncol(y)),
                       kappa=matrix(NA, nrow=ncol(x), ncol=ncol(y)),
                       rand=matrix(NA, nrow=ncol(x), ncol=ncol(y)),
                       crand=matrix(NA, nrow=ncol(x), ncol=ncol(y)))
        for(k in 1:ncol(x)){
            for(l in 1:ncol(y)){
                tab <- table(x[,k], y[,l])
                m <- matchClasses(tab, method=method, iter=iter,
                                  verbose=verbose, maxexact=maxexact)
                a <- classAgreement(tab[,m])
                retval$diag[k,l] <- a$diag
                retval$kappa[k,l] <- a$kappa
                retval$rand[k,l] <- a$rand
                retval$crand[k,l] <- a$crand
            }
        }
    }
    retval
}

permutations <- function(n) {
    if(n ==1)
        return(matrix(1))
    else if(n<2)
        stop("n must be a positive integer")
    
    z <- matrix(1)
    for (i in 2:n) { 
        x <- cbind(z, i)
        a <- c(1:i, 1:(i - 1))
        z <- matrix(0, ncol=ncol(x), nrow=i*nrow(x))
        z[1:nrow(x),] <- x 
        for (j in 2:i-1) { 
            z[j*nrow(x)+1:nrow(x),] <- x[, a[1:i+j]] 
        } 
    } 
    dimnames(z) <- NULL
    z
} 

confusionMatrix.table <- function(data, positive = NULL, prevalence = NULL, ...)
{
  #library(e1071)
  if(length(dim(data)) != 2) stop("the table must have two dimensions")
  if(!all.equal(nrow(data), ncol(data))) stop("the table must nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same classes in the same order")
  if(!is.character(positive) & !is.null(positive)) stop("positive argument must be character")
  
  classLevels <- rownames(data)
  numLevels <- length(classLevels)
  if(numLevels < 2) 
    stop("there must be at least 2 factors levels in the data")
  
  if(numLevels == 2 & is.null(positive))  positive <- rownames(data)[1]

  if(numLevels == 2 & !is.null(prevalence) && length(prevalence) != 1)
    stop("with two levels, one prevalence probability must be specified")

  if(numLevels > 2 & !is.null(prevalence) && length(prevalence) != numLevels)
    stop("the number of prevalence probability must be the same as the number of levels")

  if(numLevels > 2 & !is.null(prevalence) && is.null(names(prevalence)))
    stop("with >2 classes, the prevalence vector must have names")
  
  propCI <- function(x) { binom.test(sum(diag(x)), sum(x))$conf.int }

  propTest <- function(x) {out <- binom.test(sum(diag(x)), sum(x),
                           p = max(apply(x, 2, sum)/sum(x)),
                           alternative = "greater")
      unlist(out[c("null.value", "p.value")])
    }
  
  overall <- c(unlist(classAgreement(data))[c("diag", "kappa")],
               propCI(data), propTest(data))
  
  names(overall) <- c("Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper", "AccuracyNull", 
                      "AccuracyPValue")  
  if(numLevels == 2)
    {
      if(is.null(prevalence)) prevalence <- sum(data[, positive])/sum(data)
      negative <- classLevels[!(classLevels %in% positive)]
      tableStats <- c(sensitivity.table(data, positive),
                      specificity.table(data, negative),
                      posPredValue.table(data, positive, prevalence = prevalence),
                      negPredValue.table(data, negative, prevalence = prevalence),
                      prevalence,
                      sum(data[positive, positive])/sum(data),
                      sum(data[positive, ])/sum(data))
      names(tableStats) <- c("Sensitivity", "Specificity",
                             "Pos Pred Value", "Neg Pred Value",
                             "Prevalence", "Detection Rate",
                                "Detection Prevalence")       
    } else {

      tableStats <- matrix(NA, nrow = length(classLevels), ncol = 7)
      
      for(i in seq(along = classLevels))
        {
          pos <- classLevels[i]
          neg <- classLevels[!(classLevels %in% classLevels[i])]
          prev <- if(is.null(prevalence)) sum(data[, pos])/sum(data) else prevalence[pos]
          tableStats[i,] <- c(sensitivity.table(data, pos),
                              specificity.table(data, neg),
                              posPredValue.table(data, pos, prevalence = prev),
                              negPredValue.table(data, neg, prevalence = prev),
                              prev,
                              sum(data[pos, pos])/sum(data),
                              sum(data[pos, ])/sum(data))          

        }
      rownames(tableStats) <- paste("Class:", classLevels)
      colnames(tableStats) <- c("Sensitivity", "Specificity",
                                "Pos Pred Value", "Neg Pred Value",
                                "Prevalence", "Detection Rate",
                                "Detection Prevalence")  
    }

  structure(list(positive = positive,
                 table = data, 
                 overall = overall, 
                 byClass = tableStats,
                 dots = list(...)), 
            class = "confusionMatrix")
}

as.matrix.confusionMatrix <- function(x, what="xtabs", ...)
{
  if(!(what %in% c("xtabs", "overall", "classes")))
    stop("what must be either xtabs, overall or classes")
  out <- switch(what, xtabs = matrix(as.vector(x$table),
                  nrow = length(colnames(x$table)),
                  dimnames = list(rownames(x$table), colnames(x$table))),
                overall = as.matrix(x$overall),
                classes = as.matrix(x$byClass))
  if(what == "classes")
    {
      if(length(colnames(x$table)) > 2)
        {
          out <- t(out)
          colnames(out) <- gsub("Class: ", "", colnames(out), fixed = TRUE)
        }
    }
  out
}

as.table.confusionMatrix <- function(x, ...)  x$table

