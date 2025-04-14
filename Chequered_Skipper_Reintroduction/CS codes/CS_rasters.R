
# Fit SDMs to CS Scotland data

# Packages
library(data.table)
library(biomod2)
library(ggplot2)
library(rgdal)
library(raster)
library(ncdf4)
library(tidyverse)
library(ecospat)

# Read in CS data (already prepped)
csdata <- fread("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/BNM_records_scotland_cs_area500.csv")
csdata <- csdata[easting >= 90000]

ggplot(csdata, aes(easting, northing, color=Presence))+geom_point()


# Read in weather data
climate <- stack("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/haduk_1km_av_climate_2000_2017.tif")
names(climate) <- c("WinterT","SpringT","SummerT","AutumnT","Rainfall")

#plot(climate)

cor(as.matrix(climate), use = "complete.obs")

# Read in land cover data
lcm <- stack("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/LCM2015_GB_1km_percent_cover_aggregate_class.tif")
lcnames <- c("broadleaf.woodland",
             "coniferous.woodland",
             "arable",
             "improved.grassland",
             "semi.natural.grassland",
             "mountain.heath.bog",
             "saltwater","freshwater",
             "coastal",
             "urban")
names(lcm) <- lcnames

lcm <- subset(lcm, lcnames[!lcnames %in% c("saltwater","freshwater")])

lcmScot <- stack(crop(lcm, extent(90000,233000,621000,817000)))


elev <-  raster("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/elevation_wscot_1km_raster.tif")
elev <- crop(elev, extent(lcmScot))
crs(lcmScot) <- crs(elev)

# Clip landcover to area of interest using elevation raster
lcmScot <- mask(lcmScot, elev)

#plot(lcmScot)

#plot(elev)

# Add a layer to the lcm raster stack for the total lc cover
temp <- stackApply(lcmScot, indices = c(rep(1,10)), fun=sum, na.rm=TRUE)

# Clip climate
crs(climate) <- crs(elev)
climateScot <- mask(climate, elev)

#plot(climate)
#plot(climateScot)

# Remove 1km with less than 25% landcover classified
lcmScot[temp < 25] <- NA
elev[temp < 25] <- NA
climateScot[temp < 25] <- NA

covs <- stack(lcmScot, elev, climateScot)

cov_cors <- cor(as.matrix(covs), use = "complete.obs")

apply(cov_cors, 2, function(x){x[x<1 & x > .7]})

csdata_sp <- csdata[, .(easting, northing, Presence)]
coordinates(csdata_sp) <- ~ easting + northing
crs(csdata_sp) <- crs(elev)

# subset to less weather variables due to high correlation
covsfit <- subset(covs, c(lcnames,"SummerT","elevation_wscot_1km_raster"))


#writeRaster(covsfit, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/Environment.tif", overwrite=TRUE)


# Format data for biomod
biomdata <- BIOMOD_FormatingData(resp.var = csdata_sp,
                                 expl.var = covsfit,
                                 resp.name="Chequered Skipper")
plot(biomdata)

# Check data re there as plot(biomdata) doesn't show all points
#temp <- data.frame(x = biomdata@coord[,1],
#                   y = biomdata@coord[,2],
#                   status = biomdata@data.species)
#ggplot(temp, aes(x,y, color=status))+geom_point()

# Define model options using default options
myBiomodOption <- BIOMOD_ModelingOptions()
biomoutput <- BIOMOD_Modeling(biomdata,
                              models = c("GLM","GBM","GAM","RF","MARS","CTA","ANN"),
                              model.options = myBiomodOption,
                              VarImport = 5,
                              NbRunEval = 5,
                              DataSplit = 70)


# Model summary
biomoutput
# Model evaluations
biomeval <- get_evaluations(biomoutput)

biomeval["ROC","Testing.data",,,]


biomeval["ROC","Testing.data",,,]

biomevald <- as.data.frame.table(biomeval)
setDT(biomevald)
biomevald <- biomevald[Var1 == "ROC"]
biomevald[Var2 == "Testing.data"]$Var2 <- "AUC"
biomevald$Var2 <- factor(biomevald$Var2, levels=c("AUC","Cutoff","Sensitivity","Specificity"))

ggplot(biomevald[Var4 != "Full"], aes(Var3, Freq))+
  geom_boxplot()+facet_wrap(~Var2, scales="free_y")+
  xlab("Model")+ylab("Value")



vi <- get_variables_importance(biomoutput)

vid <- as.data.frame.table(vi)
setDT(vid)

ggplot(vid[Var4 != "Full"], aes(Var1, Freq))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(vid[Var4 != "Full"], aes(Var2, Freq))+
  geom_boxplot()+facet_wrap(~Var1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# First model VI ----
apply(vi[,,1:5,], 1, mean)

vi_prop <- apply(vi[,,1:5,], 1, mean)#/sum(apply(vi[,,1:10,], 1, mean))

minvi <- vi_prop[vi_prop < 0.05]


# Second model fit ----
biomdata2 <- BIOMOD_FormatingData(resp.var = csdata_sp,
                                  expl.var =  subset(covsfit, 
                                                     names(covsfit)[!names(covsfit) %in% names(minvi)]),
                                  resp.name="Chequered Skipper")


biomoutput2 <- BIOMOD_Modeling(biomdata2,
                               models = c("GLM","GBM","GAM","RF","MARS","CTA","ANN"),
                               model.options = myBiomodOption,
                               VarImport = 5,
                               NbRunEval = 5,
                               DataSplit = 75)


# Second model summary ----
biomeval2 <- get_evaluations(biomoutput2)

biomeval2["ROC","Testing.data",,,]

biomeval2d <- as.data.frame.table(biomeval2)
setDT(biomeval2d)
biomeval2d <- biomeval2d[Var1 == "ROC"]
biomeval2d[Var2 == "Testing.data"]$Var2 <- "AUC"
biomeval2d$Var2 <- factor(biomeval2d$Var2, levels=c("AUC","Cutoff","Sensitivity","Specificity"))

ggplot(biomeval2d[Var4 != "Full"], aes(Var3, Freq))+
  geom_boxplot()+facet_wrap(~Var2, scales="free_y")+
  xlab("Model")+ylab("Value")

#removed CTA and ANN as they have the lowest AUC

modellist <- grep("GLM|GBM|GAM|RF|MARS", get_built_models(biomoutput2), value=TRUE)


# Ensemble modelling
biomEM <- BIOMOD_EnsembleModeling(biomoutput2,
                                  eval.metric="ROC",
                                  chosen.models = modellist,
                                  prob.mean = FALSE,
                                  #eval.metric.quality.threshold = .7,
                                  models.eval.meth=c("TSS","ROC"),
                                  prob.mean.weight = TRUE)


biomEM2 <- BIOMOD_EnsembleModeling(biomoutput2,
                                  eval.metric="ROC",
                                  chosen.models = modellist,
                                  prob.mean = TRUE,
                                  #eval.metric.quality.threshold = .7,
                                  models.eval.meth=c("TSS","ROC"),
                                  prob.mean.weight = TRUE)
get_evaluations(biomEM2)




# Model projection
biomproj <- BIOMOD_Projection(biomoutput2, 
                              subset(covsfit, 
                                     names(covsfit)[!names(covsfit) %in% names(minvi)]),
                              proj.name="Model1")

#plot(biomproj)
#plot(biomproj, str.grep="RUN1")

all_proj <- get_projected_models(biomproj)
mod_proj <- get_predictions(biomproj)



# Ensemble forecasting
biomEM_f <- BIOMOD_EnsembleForecasting(EM.output = biomEM,
                                       projection.output = biomproj,
                                       selected.models = 
                                         "Chequered.Skipper_EMwmeanByROC_mergedAlgo_Full_AllData")


biomEM_f2 <- BIOMOD_EnsembleForecasting(EM.output = biomEM2,
                                       projection.output = biomproj,
                                       selected.models = 
                                         "Chequered.Skipper_EMwmeanByROC_mergedAlgo_Full_AllData")


plot(biomEM_f)

plot(biomEM_f2)




biomEM_fpred <- get_predictions(biomEM_f)


biomEM_fpred2 <- get_predictions(biomEM_f2)


plot(biomEM_fpred)

plot(biomEM_fpred2)


threshold500 <- FilteringTransformation(biomEM_fpred2, 500)

plot(threshold500)

summary(threshold500)

threshold.mean <- FilteringTransformation(biomEM_fpred2, 268.2489)

plot(threshold.mean)


threshold.25 <- FilteringTransformation(biomEM_fpred2, 132)


plot(threshold.25)




class <- matrix(c(0,0.132,1,0.132,0.999,2,0.999,1,3),nrow=3,ncol=3,byrow=TRUE)
Categorise <- reclassify(threshold.25, class, filename='suitability classes',overwrite=TRUE)
plot(Categorise,col=c("#f4a460","#adff2f","#008000"),legend=FALSE, xlab="Easting", ylab="Northing")
legend('bottomleft',inset=0.01,c(3,2,1),c("Present","Potential","Unknown"),pch=c(15,15,15),
       col = c("#008000","#adff2f","#f4a460"),box.lty=0,bg=FALSE)


classmean <- matrix(c(0,0.268,1,0.268,0.999,2,0.999,1,3),nrow=3,ncol=3,byrow=TRUE)
Categorisemean <- reclassify(threshold.mean, classmean, filename='suitability classes',overwrite=TRUE)
plot(Categorisemean,col=c("#f4a460","#adff2f","#008000"),legend=FALSE, xlab="Easting", ylab="Northing")
legend('bottomleft',inset=0.01,c(3,2,1),c("Present","Potential","Unknown"),pch=c(15,15,15),
       col = c("#008000","#adff2f","#f4a460"),box.lty=0,bg=FALSE)


class50 <- matrix(c(0,0.5,1,0.5,0.999,2,0.999,1,3),nrow=3,ncol=3,byrow=TRUE)
Categorise50 <- reclassify(threshold500, classmean, filename='suitability classes',overwrite=TRUE)
plot(Categorise50,col=c("#f4a460","#adff2f","#008000"),legend=FALSE, xlab="Easting", ylab="Northing")
legend('bottomleft',inset=0.01,c(3,2,1),c("Present","Potential","Unknown"),pch=c(15,15,15),
       col = c("#008000","#adff2f","#f4a460"),box.lty=0,bg=FALSE)


cellStats(biomEM_fpred2, stat='mean')
quantile(biomEM_fpred2, probs = c(0.25, 0.5, 0.75))


csdata_p <- csdata_sp%>% subset(Presence !="0")

buf <- create.buffer(Categorise50, csdata_p, radius=1.5)

writeRaster(biomEM_fpred, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/CS_ensemble_test5.tif", overwrite=TRUE)



# SHould landcover and elevation be standardised or does biomod do this? 
# Should some data be reserved for validating the ensemble?
# How to get measures of uncertainty?  - do an SD map of the models
# Explore model options e.g. to include quadratics/interations and testing/training etc

