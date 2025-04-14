library(data.table)
library(rnrfa)
library(ggplot2)
library(OpenStreetMap)
library(rgdal)
library(tidyverse)
library(dplyr)
library(ggspatial)
library(ggthemes)

install.packages("ggspatial")


cs_bnm <-read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/Chequered Skipper Scotland.csv", stringsAsFactors = FALSE)

table(cs_bnm$rec_year)

cs_2009 <- cs_bnm[cs_bnm$rec_year == 2009,]

cs_2009 <- cs_2009 %>% 
  mutate(adults = parse_number(adults))

cs_2009[is.na(cs_2009)] = 0

bnm2 <- cs_2009 %>% group_by(km_sq) %>%
  summarise(Presence = sum(adults))

bnm2<- bnm2 %>% mutate_if(is.numeric, ~1 * (. > 0))

cs_2009 <- cs_2009[!duplicated(cs_2009$km_sq), ]

cs_2009 <- merge(cs_2009,bnm2,by="km_sq")

cs<- cs_2009 %>% 
  mutate(easting = x, northing = y)

cs<- cs %>% 
  select(km_sq,easting,northing,Presence)

cs<- subset(cs, Presence != 0)

write.csv(cs, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/BNM_2009.csv")



