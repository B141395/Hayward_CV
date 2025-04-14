# Weather data prep - downloaded from CEDA - HadUK gridded climate obs at 1km

# Packages
library(raster)
library(ncdf4)

# Average temperature ----

# Read in average temperature data
seasmeantemp <- list()
for(k in 2000:2019){
  seasmeantemp[[paste0("year",k)]] <- stack(paste0("./Data/HadUK/meantemp/tas_hadukgrid_uk_1km_seas_",k,"01-",k,"12.nc"))
}

# Stack list of years to single raster
seasmeantemp <- stack(seasmeantemp)
# Take average across years for each season
seasmeantempav <- stackApply(seasmeantemp, indices = 1:4, fun=mean)
# Set NA values to sea etc
seasmeantempav[seasmeantempav > 9e36] <- NA
# Set names to seasons
names(seasmeantempav) <- c("Winter","Spring","Summer","Autumn")

plot(seasmeantempav)

# Crop to W Scotland
seasmeantempav <- stack(crop(seasmeantempav, extent(90000,233000,621000,817000)))

# Save raster
writeRaster(seasmeantempav, "./Data/HadUK/Processed/haduk_1km_seas_av_temp_2000_2017.tif")

# Rain ----

# Read in annual total rainfall
annrainfall <- list()
for(k in 2000:2017){
  annrainfall[[paste0("year",k)]] <- stack(paste0("./Data/HadUK/rainfall/rainfall_hadukgrid_uk_1km_ann_",k,"01-",k,"12.nc"))
}


# Stack list of years to single raster
annrainfall <- stack(annrainfall)
# Take average across years for each season
annrainfallav <- stackApply(annrainfall, indices = 1, fun=mean)
# Set NA values to sea etc
annrainfallav[annrainfallav > 9e36] <- NA

plot(annrainfallav)



# Crop to W Scotland
annrainfallav <- stack(crop(annrainfallav, extent(90000,233000,621000,817000)))
names(annrainfallav) <- "Rainfall"
writeRaster(annrainfallav, "./Data/HadUK/Processed/haduk_1km_ann_av_rainfall_2000_2017.tif")



climate <- stack(seasmeantempav, annrainfallav)

cor(as.matrix(climate), use = "complete.obs")

writeRaster(climate, "./Data/HadUK/Processed/haduk_1km_av_climate_2000_2017.tif", 
            overwrite=TRUE)

