library(raster)
library(rgeos)
library(ggplot2)
library(ggthemes)

library(countrycode)
library(rmapshaper)
library(tidyverse)
library(data.table)
library(rnrfa)
library(ggplot2)
library(OpenStreetMap)
library(rgdal)
library(dplyr)
library(grid)
library(gridExtra) 
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
library(ggspatial)
library(sf)




cs_bnm <-read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/Chequered Skipper Scotland.csv", stringsAsFactors = FALSE)

cs_bnm <- cs_bnm[cs_bnm$rec_year >= 2000,]

cs_bnm<- cs_bnm %>% 
  mutate(adults = parse_number(adults))

cs_bnm[is.na(cs_bnm)] = 0

cs_bnm<- cs_bnm %>% 
  filter(is.finite(adults))

cs<- cs_bnm %>% 
  mutate(abundance = adults, easting = x, northing = y)



bnm <- cs %>% group_by(rec_year) %>% mutate(nR = n_distinct(record_dat))

bnm <- bnm %>% 
  select(abundance,nR, rec_year)

bnm2 <- bnm %>% group_by(rec_year) %>%
  summarise(abundance = sum(abundance))

bnm_3 <- bnm[!duplicated(bnm$rec_year), ]

bnm3 <- bnm_3 %>% 
  select(nR, rec_year)

bnm_output <- merge(bnm2,bnm3,by="rec_year")

ggplot(bnm_output, aes(x=rec_year,y=abundance))+ geom_point()


cs1 <-read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/BNM_records_scotland_cs_area500.csv", stringsAsFactors = FALSE)


cs1 <- subset(cs1, select = -c(X))


a <- cs1 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble()

a2 <- cbind(cs1, a)

#write.csv(a2, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/CSMAP.csv")

a3 <-read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/CSMAP.csv", stringsAsFactors = FALSE)


ggplot(a3, # Creating plot and naming it Fig_2
       aes(x = X, y = Y, colour = Presence)) +  # set x and y input as longitude and latitude 
  borders("world", colour = "gray40", fill = "gray75", size = 0.3) +  # specifying format of the map
  coord_cartesian(xlim = c(-7.5, -2.5), ylim = c(55,57.5)) + # specifying coordinates of the graph
  theme_map() + #displaying map
  geom_point(size = 1) + # set size of data points
  theme(legend.position = "right") + # add legend to the map
  labs(title = "UK CS Population map")+ # add title to the map
  theme(plot.title = element_text(size = 15, hjust = 0.5)) 

world <- getMap(resolution = "low")


clipper_scot <- as(extent(-8, 0, 55, 59), "SpatialPolygons")

proj4string(clipper_scot) <- CRS(proj4string(world))

world_clip <- raster::intersect(world, clipper_scot)

world_clip_f <- fortify(world_clip)

ggplot() + 
    geom_polygon(data = world_clip_f, 
                 aes(x = long, y = lat, group = group),
                 fill = "gray75", colour = "black") + 
    geom_point(data = a3, alpha = 1,size = 5,shape = 21,colour = "black",
               aes(x = X, y = Y,shape = Presence,fill = Presence)) +
  scale_colour_manual(values = c("dark blue","light blue"))+ 
  scale_fill_manual(values = c("dark blue","light blue"))+
  theme_bw() +
    xlab("Longitude (°)") +
    ylab("Latitude (°)") + 
    coord_quickmap()+ 
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=16))+
  annotation_north_arrow(location = "tr", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering, height = unit(1,"in"), width = unit(1,"in"))+  
  scalebar(data = world_clip_f, location = "bottomleft", dist = 100,
             dist_unit = "km", transform = TRUE,  model = "WGS84")
















cs_2000 <- cs_bnm[cs_bnm$rec_year == 2016,]

cs_2000<- cs_2000 %>% 
  mutate(adults = parse_number(adults))

cs_bnm[is.na(cs_bnm)] = 0

cs_2000<- cs_2000 %>% 
  filter(is.finite(adults))

cs<- cs_2000 %>% 
  mutate(abundance = adults, easting = x, northing = y)

cs<- cs %>% 
  select(gridref,km_sq,abundance,rec_month,rec_year,easting,northing)



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






cs <-read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/Chequered Skipper Scotland 2.csv", stringsAsFactors = FALSE)

cs <- subset(cs, rec_month != 0)

cs <- cs[cs$rec_year >= 2000,]

cs <- cs[!cs$km_sq=="",]

cs<- cs %>% 
  mutate(adults = parse_number(adults))

cs[is.na(cs)] = 0

cs <- subset(cs, adults != 0)

cs2 <- cs %>% select(km_sq,adults,rec_year,x,y)


cs2 <- cs2 %>% group_by(km_sq) %>%
  summarise(Presence = sum(adults))


bnm <- cs %>% group_by(km_sq) %>% mutate(nR = n_distinct(record_dat))


bnm<- bnm %>% select(km_sq,nR,x,y)

bnm_n <- bnm[!duplicated(bnm$km_sq), ]

cs4 <- merge(bnm_n,cs3,by="km_sq")


#write.csv(cs4, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/CSpresence.csv")


csp <-read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/CSpresence.csv", stringsAsFactors = FALSE)

bnm1 <- bnm_n %>% select(km_sq, x, y)

csp <- merge(csp,bnm1,by="km_sq")

csp <- subset(csp, select = -c(X))

csp1 <- csp %>%
  st_as_sf(coords = c("x", "y"), crs = 27700) %>% st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble()

csp <- cbind(csp, csp1)

csp <- csp %>% select(km_sq,nR,Presence,X,Y)


ggplot(csp, # Creating plot and naming it Fig_2
       aes(x = X, y = Y, colour = nR)) +  # set x and y input as longitude and latitude 
  borders("world", colour = "gray40", fill = "gray75", size = 0.3) +  # specifying format of the map
  coord_cartesian(xlim = c(-7.5, -2.5), ylim = c(55,57.5)) + # specifying coordinates of the graph
  theme_map() + #displaying map
  geom_point(size = 1) + # set size of data points
  theme(legend.position = "right") + # add legend to the map
  labs(title = "UK CS Population map")+ # add title to the map
  theme(plot.title = element_text(size = 15, hjust = 0.5)) 

world <- getMap(resolution = "low")


clipper_scot <- as(extent(-8, 0, 55, 59), "SpatialPolygons")

proj4string(clipper_scot) <- CRS(proj4string(world))

world_clip <- raster::intersect(world, clipper_scot)

world_clip_f <- fortify(world_clip)

ggplot() + 
  geom_polygon(data = world_clip_f, 
               aes(x = long, y = lat, group = group),
               fill = "gray75", colour = "black") + 
  geom_point(data = csp, alpha = 1,size = 2.5,shape = 21,colour = "black",
             aes(x = X, y = Y,shape = nR,fill = nR)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") + 
  coord_quickmap()+ 
  labs(title = "Scotland Carterocephalus palaemon Presence Records")+ # add title to the map
  theme(plot.title = element_text(size = 15, hjust = 0.5))+
  annotation_north_arrow(location = "tr", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering, height = unit(1,"in"), width = unit(1,"in"))+  
  scalebar(data = world_clip_f, location = "bottomleft", dist = 100,
           dist_unit = "km", transform = TRUE,  model = "WGS84")
