library(tidyverse)
library(data.table)
library(rnrfa)
library(ggplot2)
library(OpenStreetMap)
library(rgdal)
library(dplyr)

cs_bnm <-read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/Chequered Skipper Scotland.csv", stringsAsFactors = FALSE)


cs_bnm <- cs_bnm[cs_bnm$rec_year >= 2000,]

cs_bnm<- cs_bnm %>% 
  mutate(adults = parse_number(adults))

cs_bnm[is.na(cs_bnm)] = 0

cs_bnm<- cs_bnm %>% 
  filter(is.finite(adults))

cs<- cs_bnm %>% 
  mutate(abundance = adults, easting = x, northing = y)
  
cs<- cs %>% 
  select(gridref,km_sq,abundance,rec_month,rec_year,easting,northing)


write.csv(cs, ".Chequered_Skipper_records_with_EN_2.csv")

ggplot(cs, aes(easting, northing))+geom_point()+coord_fixed()

# Produce a map with background
# get easting and northing as lat/long
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"
# Create spatial dataframe
cs_sp <- SpatialPoints(cs[,c("easting","northing")],
                       proj4string = CRS(ukgrid))

# Convert to lat/long
cs_sp_ll <- spTransform(cs_sp, CRS(latlong))
cs_df_ll <- data.frame(cs_sp_ll)
colnames(cs_df_ll) <- c("Long","Lat")
# Get bounding box values for mf points
bbox <- cs_sp_ll@bbox
# Define area for base map
map <- openmap(upperLeft = c(bbox[2,2]+1, bbox[1,1]-1), 
               lowerRight =  c(bbox[2,1]-1,bbox[1,2]+1),
               type = "osm", minNumTiles = 20)
# Set CRS for basemap
map.latlon <- openproj(map, projection =  cs_sp@proj4string)

OpenStreetMap::autoplot.OpenStreetMap(map.latlon)+
  geom_point(data = cs, aes(easting, northing))+
  theme(panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA))+
  xlab("Easting")+ylab("Northing")+
  xlim(min(cs$easting)-20000, max(cs$easting)+20000)+
  ylim(min(cs$northing)-20000, max(cs$northing)+20000)

minE  <- min(cs$easting)-20000
maxE <- max(cs$easting)+20000
minN <- min(cs$northing)-20000
maxN <- max(cs$northing)+20000




bnm <- subset(cs_bnm, select = -c(X_uid_))

bnm_scot <- osg_parse(bnm$gridref)
bnm$easting <- bnm_scot$easting
bnm$northing <- bnm_scot$northing

bnm$easting <- floor(bnm$easting/1000)*1000
bnm$northing <- floor(bnm$northing/1000)*1000

OpenStreetMap::autoplot.OpenStreetMap(map.latlon)+
  geom_point(data = unique(bnm[,c("easting","northing")]), 
             aes(easting, northing))+
  theme(panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA))+
  xlab("Easting")+ylab("Northing")+
  xlim(min(bnm$easting)-20000,max(bnm$easting)+20000)+
  ylim(min(bnm$northing)-20000,max(bnm$northing)+20000)


bnm<- bnm %>% 
  select(gridref,km_sq,record_dat,adults,rec_year,rec_month,rec_day,easting,northing)

table(bnm$rec_month)
table(bnm$adults)

# Proportion in May - July
nrow(bnm[bnm$rec_month,5:7])/nrow(bnm)

# Limit to squares with at least X records/species? ----
# How many records do squares typically have?


bnm <- subset(bnm, rec_month != 0)

bnm <- bnm %>% subset(km_sq !="")

bnm2 <- bnm %>% group_by(km_sq) %>%
  summarise(Presence = sum(adults))

bnm2<- bnm2 %>% mutate_if(is.numeric, ~1 * (. > 0))

bnm <- bnm %>% group_by(km_sq) %>% mutate(nR = n_distinct(record_dat))

bnm<- bnm %>% select(km_sq,easting,northing,nR,rec_year)

bnm<- bnm %>% group_by(km_sq) %>% mutate (minyear = min(rec_year), maxyear = max(rec_year))
  
bnm_n <- bnm %>% select(km_sq,easting,northing,nR,minyear,maxyear)


bnm_n <- bnm_n[!duplicated(bnm_n$km_sq), ]

bnm_3 <- merge(bnm_n,bnm2,by="km_sq")

bnm_4 <- bnm_3 %>% select(km_sq, easting, northing,Presence)

absence <- read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/mf_PA_records_scotland_mf_area.csv", stringsAsFactors = FALSE)

absence <- absence %>% subset(Presence != 1)

absence <- subset(absence, select = -c(X))

bnm_5 <- rbind(bnm_4,absence)

bnm_5 <- bnm_5 %>% mutate(Presence = Presence.x+Presence.y)

bnm_6 <- bnm_5 %>% group_by(km_sq) %>%
  summarise(Presence = sum(Presence))

bnm_7 <- bnm_5[!duplicated(bnm_5$km_sq), ]

bnm_7 <- bnm_7 %>% select(km_sq,easting,northing)

bnm_output <- merge(bnm_6,bnm_7,by="km_sq")


# Save formatted datasets ----
write.csv(bnm_output, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/BNM_records_scotland_cs_area.csv")

bnm_500 <- read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/BNM_records_scotland_cs_area.csv")

bnm_500 <- subset(bnm_500, select = -c(X))

bnm_500 <- bnm_500 %>% mutate (easting = easting+500, northing = northing+500)

write.csv(bnm_500, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/BNM_records_scotland_cs_area500.csv")
