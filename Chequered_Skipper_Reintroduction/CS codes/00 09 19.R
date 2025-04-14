
# Fit SDMs to CS Scotland data

# Packages
library(data.table)
library(biomod2)
library(ggplot2)
library(rgdal)
library(raster)
library(ncdf4)

# Read in CS data (already prepped)
csdata2000 <- fread("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/BNM_2000.csv")
csdata2000 <- csdata2000[easting >= 90000]

ggplot(csdata2000, aes(easting, northing, color=Presence))+geom_point()


# Read in weather data
climate <- stack("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/haduk_1km_av_climate_2000_2017.tif")
names(climate) <- c("WinterT","SpringT","SummerT","AutumnT","Rainfall")

plot(climate)

climateScot <- stack(crop(climate, extent(150000,230000,700000,800000)))

plot(climateScot)

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

lcmScot <- stack(crop(lcm, extent(150000,230000,700000,800000)))

plot(lcmScot)

elev <-  raster("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/elevation_wscot_1km_raster.tif")
elev <- crop(elev, extent(lcmScot))
crs(lcmScot) <- crs(elev)

# Clip landcover to area of interest using elevation raster
lcmScot <- mask(elev, lcmScot)

plot(lcmScot)

plot(elev)

# Add a layer to the lcm raster stack for the total lc cover
temp <- stackApply(lcmScot, indices = c(rep(1,10)), fun=sum, na.rm=TRUE)


plot(climateScot)

# Remove 1km with less than 25% landcover classified
lcmScot[temp < 25] <- NA
elev[temp < 25] <- NA
climateScot[temp < 25] <- NA

covs <- stack(lcmScot, elev, climateScot)

cov_cors <- cor(as.matrix(covs), use = "complete.obs")

apply(cov_cors, 2, function(x){x[x<1 & x > .7]})

csdata_sp00 <- csdata2000[, .(easting, northing, Presence)]
coordinates(csdata_sp00) <- ~ easting + northing
crs(csdata_sp00) <- crs(elev)

# subset to less weather variables due to high correlation
covsfit <- subset(covs, c(lcnames,"SummerT","elevation_wscot_1km_raster"))


#writeRaster(covsfit, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/Environment.tif", overwrite=TRUE)


# Format data for biomod
biomdata00 <- BIOMOD_FormatingData(resp.var = csdata_sp00,
                                 expl.var = covsfit,
                                 resp.name="Chequered Skipper 2000")
plot(biomdata00)



csdata2009 <- fread("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/BNM_2009.csv")
csdata2009 <- csdata2009[easting >= 90000]

csdata2009 <- subset( csdata2009, select = -V1 )


ggplot(csdata2009, aes(easting, northing, color=Presence))+geom_point()


# Read in weather data
climate09 <- stack("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/haduk_1km_av_climate_2000_2017.tif")
names(climate09) <- c("WinterT","SpringT","SummerT","AutumnT","Rainfall")

climateScot09 <- stack(crop(climate09, extent(150000,230000,700000,800000)))


cor(as.matrix(climate09), use = "complete.obs")

# Read in land cover data
lcm09 <- stack("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/LCM2015_GB_1km_percent_cover_aggregate_class.tif")
lcnames09 <- c("broadleaf.woodland",
             "coniferous.woodland",
             "arable",
             "improved.grassland",
             "semi.natural.grassland",
             "mountain.heath.bog",
             "saltwater","freshwater",
             "coastal",
             "urban")
names(lcm09) <- lcnames09

lcm09 <- subset(lcm09, lcnames09[!lcnames09 %in% c("saltwater","freshwater")])

lcmScot09 <- stack(crop(lcm09, extent(150000,230000,700000,800000)))


elev09 <-  raster("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/elevation_wscot_1km_raster.tif")
elev09 <- crop(elev09, extent(lcmScot09))
crs(lcmScot09) <- crs(elev09)

# Clip landcover to area of interest using elevation raster
lcmScot09 <- mask(lcmScot09, elev09)

#plot(lcmScot09)

#plot(elev09)

# Add a layer to the lcm raster stack for the total lc cover
temp09 <- stackApply(lcmScot09, indices = c(rep(1,10)), fun=sum, na.rm=TRUE)

# Clip climate
crs(climate09) <- crs(elev09)

# Remove 1km with less than 25% landcover classified
lcmScot09[temp09 < 25] <- NA
elev09[temp09 < 25] <- NA
climateScot09[temp09 < 25] <- NA

covs09 <- stack(lcmScot09, elev09, climateScot09)

cov_cors09 <- cor(as.matrix(covs09), use = "complete.obs")

apply(cov_cors09, 2, function(x){x[x<1 & x > .7]})

csdata_sp09 <- csdata2009[, .(easting, northing, Presence)]
coordinates(csdata_sp09) <- ~ easting + northing
crs(csdata_sp09) <- crs(elev09)

# subset to less weather variables due to high correlation
covsfit09 <- subset(covs09, c(lcnames09,"SummerT","elevation_wscot_1km_raster"))


#writeRaster(covsfit, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/Environment.tif", overwrite=TRUE)


# Format data for biomod
biomdata09 <- BIOMOD_FormatingData(resp.var = csdata_sp09,
                                   expl.var = covsfit09,
                                   resp.name="Chequered Skipper 2009")
plot(biomdata09)

csdata2019 <- fread("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/BNM_2019.csv")
csdata2019 <- csdata2019[easting >= 90000]

csdata2019 <- subset( csdata2019, select = -V1 )


ggplot(csdata2019, aes(easting, northing, color=Presence))+geom_point()


# Read in weather data
climate19 <- stack("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/haduk_1km_av_climate_2000_2017.tif")
names(climate19) <- c("WinterT","SpringT","SummerT","AutumnT","Rainfall")


climateScot19 <- stack(crop(climate19, extent(150000,230000,700000,800000)))


cor(as.matrix(climate19), use = "complete.obs")

# Read in land cover data
lcm19 <- stack("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/LCM2015_GB_1km_percent_cover_aggregate_class.tif")
lcnames19 <- c("broadleaf.woodland",
               "coniferous.woodland",
               "arable",
               "improved.grassland",
               "semi.natural.grassland",
               "mountain.heath.bog",
               "saltwater","freshwater",
               "coastal",
               "urban")
names(lcm19) <- lcnames19

lcm19 <- subset(lcm19, lcnames19[!lcnames19 %in% c("saltwater","freshwater")])

lcmScot19 <- stack(crop(lcm19, extent(150000,230000,700000,800000)))


elev19 <-  raster("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/elevation_wscot_1km_raster.tif")
elev19 <- crop(elev19, extent(lcmScot19))
crs(lcmScot19) <- crs(elev19)

# Clip landcover to area of interest using elevation raster
lcmScot19 <- mask(lcmScot19, elev19)

#plot(lcmScot19)

#plot(elev19)

# Add a layer to the lcm raster stack for the total lc cover
temp19 <- stackApply(lcmScot19, indices = c(rep(1,10)), fun=sum, na.rm=TRUE)

# Clip climate
crs(climate19) <- crs(elev19)


# Remove 1km with less than 25% landcover classified
lcmScot19[temp19 < 25] <- NA
elev19[temp19 < 25] <- NA
climateScot19[temp19 < 25] <- NA

covs19 <- stack(lcmScot19, elev19, climateScot19)

cov_cors19 <- cor(as.matrix(covs19), use = "complete.obs")

apply(cov_cors19, 2, function(x){x[x<1 & x > .7]})

csdata_sp19 <- csdata2019[, .(easting, northing, Presence)]
coordinates(csdata_sp19) <- ~ easting + northing
crs(csdata_sp19) <- crs(elev19)

# subset to less weather variables due to high correlation
covsfit19 <- subset(covs19, c(lcnames19,"SummerT","elevation_wscot_1km_raster"))


#writeRaster(covsfit, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/Environment.tif", overwrite=TRUE)


# Format data for biomod
biomdata19 <- BIOMOD_FormatingData(resp.var = csdata_sp19,
                                   expl.var = covsfit19,
                                   resp.name="Chequered Skipper 2019")
plot(biomdata19)

attach(mtcars)
par(mfrow=c(1,3))
plot(biomdata00)
plot(biomdata09)
plot(biomdata19)





(hist <- ggplot(bnm_avg, aes(x = Abundance)) +
    geom_histogram(binwidth=50) +
    theme_classic())

shapiro.test(bnm_avg$Abundance)#W = 0.96103, p-value = 0.5646


(hist <- ggplot(bnm_avg, aes(x = logpop)) +
    geom_histogram() +
    theme_classic())

shapiro.test(bnm_avg$logpop)#W = 0.96038, p-value = 0.5515


(hist <- ggplot(bnm_avg, aes(x = avg)) +
    geom_histogram() +
    theme_classic())

shapiro.test(bnm_avg$avg)#W = 0.94011, p-value = 0.2409


bnm_avg<-mutate(bnm_avg,logavg=log(avg))

(hist <- ggplot(bnm_avg, aes(x = logavg)) +
    geom_histogram(binwidth=0.2) +
    theme_classic())

shapiro.test(bnm_avg$logavg)#W = 0.98391, p-value = 0.9741


