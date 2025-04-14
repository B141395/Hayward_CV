# Convert elevation tif data to data frame

library(data.table)
library(rgdal)
library(raster)
library(ggplot2)

# Read in tiff file (see README in elevation file - data source from OS Terrain 50)
elev <- stack("./Data/Elevation/WScot_elevation_OSTerrain50.tif")

# Need to remove the sea
scot <- readOGR("D://Scotland.shp")
crs(elev) <- crs(scot)

elev <- mask(elev, scot)


elev_df <- setDT(as.data.frame(as(elev, "SpatialGridDataFrame")))
colnames(elev_df) <- c("elev","easting50m","northing50m")

# Limit extent
elev_df <- elev_df[easting50m >= 81000 & easting50m <= 233000 & 
                     northing50m >= 621000 & northing50m <= 817000]

# Add a column corresponding to the 1km
elev_df[, easting := floor(easting50m/1000)*1000]
elev_df[, northing := floor(northing50m/1000)*1000]

# Should ideally be 400 50m squares to 1 1km square
summary(elev_df[, .N, by=.(easting,northing)])

# Calculate min, max, average elevation per 1km but ignoring the sea
elev_summ <- 
  elev_df[, .(n50m = length(elev), mean = mean(elev), min = min(elev), max = max(elev)), by=.(easting, northing)]


# Save
write.csv(elev_summ, "./Data/Elevation/elevation_wscot_1km.csv", row.names=FALSE)

# Convert and save as a raster
elev_summ$easting <- elev_summ$easting+500
elev_summ$northing <- elev_summ$northing+500
elev_summ_sp <- elev_summ[, c("easting","northing","mean")]
coordinates(elev_summ_sp) <- ~ easting + northing
crs(elev_summ_sp) <- crs(elev)
gridded(elev_summ_sp) <- TRUE
elev_r <- raster(elev_summ_sp)
plot(elev_r)
writeRaster(elev_r, "./Data/Elevation/elevation_wscot_1km_raster.tif")

ggplot(elev_summ, aes(easting, northing, color=mean))+geom_point()

