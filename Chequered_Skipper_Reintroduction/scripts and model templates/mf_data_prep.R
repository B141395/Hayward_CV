# Prepare MF data
# Combine presences from Scotland site dossier with absences from BNM

# Packages ----
library(data.table)
library(rnrfa)
library(ggplot2)
library(OpenStreetMap)
library(rgdal)

# MF data from site dossier ----
# Presence records from site dossier (in QGIS project)
mf <- read.csv("./Data/MF_1km_Records_Scotland_Dossier.csv", stringsAsFactors = FALSE)

# Limit to recent records
mf <- mf[mf$LastYear >= 2000,]

# Add easting and northing
mf <- cbind(mf, osg_parse(mf$GridRef))

# Save formatted mf data ----
write.csv(mf, "./Data/Formatted/MF Dossier_records_with_EN.csv")


# Quick plot of MF Scotland record locations
ggplot(mf, aes(easting, northing))+geom_point()+coord_fixed()

# Produce a map with background
# get easting and northing as lat/long
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"
# Create spatial dataframe
mf_sp <- SpatialPoints(mf[,c("easting","northing")],
                                proj4string = CRS(ukgrid))
# Convert to lat/long
mf_sp_ll <- spTransform(mf_sp, CRS(latlong))
mf_df_ll <- data.frame(mf_sp_ll)
colnames(mf_df_ll) <- c("Long","Lat")
# Get bounding box values for mf points
bbox <- mf_sp_ll@bbox
# Define area for base map
map <- openmap(upperLeft = c(bbox[2,2]+1, bbox[1,1]-1), 
               lowerRight =  c(bbox[2,1]-1,bbox[1,2]+1),
               type = "osm", minNumTiles = 20)
# Set CRS for basemap
map.latlon <- openproj(map, projection =  mf_sp@proj4string)

autoplot(map.latlon)+
  geom_point(data = mf, aes(easting, northing))+
  theme(panel.background = element_rect(colour = NA),
         plot.background = element_rect(colour = NA))+
  xlab("Easting")+ylab("Northing")+
  xlim(min(mf$easting)-20000, max(mf$easting)+20000)+
  ylim(min(mf$northing)-20000, max(mf$northing)+20000)

  
# Define extent ----
# Define spatial extent to consider (20km buffer around min/max from mf records)
minE  <- min(mf$easting)-20000
maxE <- max(mf$easting)+20000
minN <- min(mf$northing)-20000
maxN <- max(mf$northing)+20000




# BNM data ----
# (combined data up to 2016 with 2017)
bnm <- readRDS("../../Data/BNM/2017/bnm_to_2017.rds")

# Reduce to columns of interest and adults only
bnm <- bnm[adults != "" & adults != 0, .(id, recorder, site, county, gridref, km_sq, record_date, species, adults, rec_year, rec_month, rec_day)]

# Limit to 2000-2017 & at least 1km scale & remove grid refs starting with W,Z as osg_parse doesn't like them
bnm <- bnm[rec_year >= 2000 & km_sq != "" & !(substr(gridref, 1, 1) %in% c("W","Z"))]

# Add easting and northing ----
bnm_en <- osg_parse(bnm$gridref)
bnm$easting_full <- bnm_en$easting
bnm$northing_full <- bnm_en$northing


# Limit spatial extent ----
bnm_scot <- bnm[easting_full >= minE & 
           easting_full <= maxE & 
           northing_full >= minN & 
           northing_full <= maxN]

# Add 1km easting/northing
bnm_scot$easting <- floor(bnm_scot$easting/1000)*1000
bnm_scot$northing <- floor(bnm_scot$northing/1000)*1000


autoplot(map.latlon)+
  geom_point(data = unique(bnm_scot[,c("easting","northing")]), 
             aes(easting, northing))+
  geom_point(data = mf, aes(easting, northing), color="red")+
  theme(panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA))+
  xlab("Easting")+ylab("Northing")+
  xlim(min(mf$easting)-20000,max(mf$easting)+20000)+
  ylim(min(mf$northing)-20000,max(mf$northing)+20000)

# Look at mf data in bnm dataset? ----
bnm_mf <- bnm_scot[ species == "Marsh Fritillary"]
# Produce a df identifying whether recorder in one or both sources
mf$km_sq <- mf$GridRef

# First and last years from bnm data
bnm_mf_FL <- bnm_mf[, .(FY=min(rec_year),LY=max(rec_year)), by=.(km_sq, easting, northing)]
# Add data source identifiers
bnm_mf_FL$BNM <- TRUE
mf$SD <- TRUE
bnm_mf_both <- merge(bnm_mf_FL,
                     mf, by=c("km_sq","easting","northing"),
                     all.x=TRUE, all.y=TRUE)
bnm_mf_both[is.na(SD)]$SD <- FALSE
bnm_mf_both[is.na(BNM)]$BNM <- FALSE
bnm_mf_both$SOURCE <- paste(bnm_mf_both$SD, bnm_mf_both$BNM,sep="_")

# Show points and whether from SD or BNM or both
autoplot(map.latlon)+
  geom_point(data = bnm_mf_both, 
             aes(easting, northing, color=SOURCE))+
  theme(panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA))+
  xlab("Easting")+ylab("Northing")+
  xlim(min(mf$easting)-20000,max(mf$easting)+20000)+
  ylim(min(mf$northing)-20000,max(mf$northing)+20000)


# Limit in timing of records according to MF season? ----
# What months mf recorded?
table(bnm_mf$rec_month)
# Proportion in May - July
nrow(bnm_mf[rec_month %in% 5:7])/nrow(bnm_mf)

# Limit to squares with at least X records/species? ----
# How many records/species do squares typically have?
bnm_scot_n <- bnm_scot[rec_month %in% 5:7 & species != "Marsh Fritillary", 
                       .(nR = uniqueN(record_date), 
                         nSpp = uniqueN(species),
                         minyear = min(rec_year),
                         maxyear = max(rec_year)), 
                       by = .(km_sq, easting, northing)]
table(bnm_scot_n$nR)
table(bnm_scot_n$nSpp)

# How many squares have at least 3 records?
nrow(bnm_scot_n[nR >= 3])

# Save formatted datasets ----
write.csv(bnm_scot_n, "./Data/Formatted/BNM_records_scotland_mf_area.csv")
write.csv(bnm_scot_n[nR >= 3], "./Data/Formatted/BNM_records_scotland_mf_area_min3rec.csv")


# Save combined output ----
bnm_scot_n$Presence <- 0
mf$Presence <- 1

alldata <- rbind(bnm_scot_n[nR>=3,c("km_sq","easting","northing","Presence")], 
                 mf[,c("km_sq","easting","northing","Presence")])

write.csv(alldata, "./Data/Formatted/mf_PA_records_scotland_mf_area.csv")

alldata5 <- alldata
alldata5[, V1 := NULL]
alldata5[, easting := easting + 500]
alldata5[, northing := northing + 500]


write.csv(alldata5,"./Data/Formatted/mf_PA_records_scotland_mf_area500.csv")



# Check with a quick map
autoplot(map.latlon)+
  geom_point(data = alldata, 
             aes(easting, northing, color=factor(Presence)))+
  theme(panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA))+
  xlab("Easting")+ylab("Northing")+
  xlim(min(mf$easting)-20000,max(mf$easting)+20000)+
  ylim(min(mf$northing)-20000,max(mf$northing)+20000)

