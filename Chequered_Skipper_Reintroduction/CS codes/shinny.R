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

cs_bnm <- cs_bnm %>% mutate(ID = X_uid_)


cs<- cs_bnm %>% 
  mutate(abundance = adults, easting = x, northing = y)


cs <- cs %>% select(ID,species,x,y,adults)


library(sf)


a <- cs %>%
  st_as_sf(coords = c("x", "y"), crs = 27700) %>% st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble()


a2 <- cbind(cs, a)

a3 <- a2 %>% select(ID, X, Y)

a4 <- a2 %>% select(ID,species, adults)

a5 <- merge(a3,a4,by="ID")

#write.csv(a5, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/CSshinny.csv")
