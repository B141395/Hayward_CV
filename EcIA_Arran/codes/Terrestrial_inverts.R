#Libraries----
library(ggplot2)
library(tidyverse)
library(stringr)

#Style----
style <- function(){ #setting the style for my plots
  font <- "Helvetica"
  theme(plot.title = element_text(family = font, size = 14, face = "bold", color = "#222222", hjust = 0.5), 
        plot.subtitle = element_text(family = font, size = 12, margin = margin(9, 0, 9, 0)), 
        plot.caption = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text.align = 0, 
        legend.position = "bottom",
        legend.title = element_text(family = font, size = 9, face = "bold", color = "#222222",  hjust = 0.5), 
        legend.key = element_blank(), 
        legend.text = element_text(family = font, size = 9, color = "#222222"),
        axis.text = element_text(family = font, size = 9, color = "#222222"), 
        axis.text.x = element_text(margin = margin(5, b = 10)), 
        axis.title = element_text(family = font, size = 12, face = "bold", color = "#222222"), 
        axis.ticks = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 12, hjust = 0))
}

#Importing raw data----
tinv<-read.csv("occurrence_dat/terrestrial_inverts_occurrence.csv")#loading the terretrial inverts data
#1. Terrestrial inverts data preparation----
tinv <-tinv %>% 
  select(eventID,occurrenceStatus,individualCount,scientificName,class,order,family,genus,species) %>% #keeping the needed columns only
  mutate(Plot = case_when( #creating a new column called plot
    grepl("^N", eventID, ignore.case = TRUE) ~ "Northern Plot", #classifying eventID with "N" as Northern Plot
    grepl("^S", eventID, ignore.case = TRUE) ~ "Southern Plot", #classifying eventID with "S" as Southern Plot
    TRUE ~ "Unknown"
  )) %>% 
  mutate(method = case_when( #creating a new column for sampling methods
    grepl("P", eventID) ~ "pitfall", #eventID with "P" for pitfall
    TRUE ~ "sweepnet"#eventID without "P" is for sweepnet
  ))%>% 
  mutate(family = ifelse(family == "", "Unidentified", family)) %>% #changing the empty cells into unidentified
  mutate(order = ifelse(order == "", "Unidentified", order)) %>% 
  mutate(richness = ifelse(occurrenceStatus == "present", 1, 0))#creating a new column for species richness

pitfall<-tinv %>% #Keeping only pitfall
  filter(method == "pitfall")

snet<-tinv %>%
  filter(method == "sweepnet") %>% #Keeping only sweepnet
  filter(occurrenceStatus == "present") %>% #Keeping present data only
  mutate(Trans = case_when( #creating a column for transect
    grepl("L", eventID, ignore.case = TRUE) ~ "Low",
    grepl("M", eventID, ignore.case = TRUE) ~ "Mid",    
    grepl("H", eventID, ignore.case = TRUE) ~ "High",
    TRUE ~ "Unknown")) #sorting the samples into different transects by their eventID 
 
snet$Trans<-factor(snet$Trans, levels = c("Low", "Mid", "High"))#ordering the factor levels

#2. Pitfall
#2.1. Presence and Absence of Pitfall----
p_pres <- pitfall %>% 
  distinct(eventID,occurrenceStatus,Plot)%>%#keeping distinct eventIDs and their occurrence and plot
  mutate(ab_pr=1) #changing all of them to 1 for tallying later


(pres_p<-ggplot(p_pres, aes(y=ab_pr,x=occurrenceStatus))+ #plotting the number of pifalls that are empty or not
  geom_bar(stat = "identity",orientation = "x")+
  labs(x = "Invertebrates absent or present", y = "Number of Pitfalls", 
        title = "Invertebrates presence in pitfalls\nin Northern and Southern Plots")+
  facet_wrap(~Plot)+ #seperating the Northerna and Southern Plots for comparision
  style())

ggsave(filename = "figures/Terrestrial_inverts/Pitfall_pres.png", plot = pres_p, width = 7.5, height = 5) #exporting the graph as png


#2.2. Abundance of Inverts in pitfalls----
p_abun <- pitfall %>% 
  filter(occurrenceStatus == "present") #keeping pitfalls with inverts present only

(abun_p<-ggplot(p_abun, aes(fill=order, y=individualCount,x=Plot))+ #plotting the inverts abundance
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    labs(x = "Plots", y = "Number of Individuals", 
         title = "Number of Invertebrates sampled by pitfalls\nin Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Terrestrial_inverts/Pitfall_abun.png", plot = abun_p, width = 7.5, height = 5) #exporting the graph as png

#2.3. Family richness of Inverts in pitfalss----
p_rich <- p_abun %>% 
  distinct(order,family,Plot,richness) #keeping dinstinct families per plot

(rich_p<-ggplot(p_rich, aes(fill=order, y=richness,x=Plot))+ #plotting family richness
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    labs(x = "Plots", y = "Family Richness", 
         title = "Invertebrate family richness sampled by pitfalls\nin Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Terrestrial_inverts/Pitfall_rich.png", plot = rich_p, width = 7.5, height = 5) #exporting the graph as png

#3.Sweepnets
#3.1. Abundance of Inverts in Sweepnets----
(abun_s<-ggplot(snet, aes(fill=order, y=individualCount,x=Plot))+ #plotting the abundance of inverts
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    labs(x = "Plots", y = "Number of Individuals", 
         title = "Number of Invertebrates sampled by sweep-netting\nin Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Terrestrial_inverts/Snet_abun.png", plot = abun_s, width = 7.5, height = 5) #exporting the graph as png

#base on transect
(abun_s_t<-ggplot(snet, aes(fill=order, y=individualCount,x=Trans))+ #plotting the abundanc if inverts base on transects
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    facet_wrap("Plot")+ #comparing the northern and souther plots
    labs(x = "transects", y = "Number of Individuals", 
         title = "Number of Invertebrates sampled by sweep-netting\nin Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Terrestrial_inverts/Snet_Trans_abun.png", plot = abun_s_t, width = 7.5, height = 5) #exporting the graph as png

#3.2. Family richness of Inverts in Sweepnets----
s_rich <- snet %>% 
  distinct(order,family,Plot,richness) #keeping dinstinct families per plot

(rich_s<-ggplot(s_rich, aes(fill=order, y=richness,x=Plot))+ #plotting the number of pifalls that are empty or not
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    labs(x = "Plots", y = "Family Richness", 
         title = "Invertebrate family richness sampled by sweep-netting\nin Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Terrestrial_inverts/Snet_rich.png", plot = rich_s, width = 7.5, height = 5) #exporting the graph as png


#base on transect
s_rich_2 <- snet %>% 
  distinct(order,family,Plot,richness,Trans) #keeping dinstinct families per transect

(rich_s_t<-ggplot(s_rich_2, aes(fill=order, y=richness,x=Trans))+ #plotting the number of pifalls that are empty or not
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    facet_wrap("Plot")+ #comparing the northern and souther plots
    labs(x = "transects", y = "Family Richness", 
         title = "Invertebrate family richness sampled by sweep-netting\nin Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Terrestrial_inverts/Snet_Trans_rich.png", plot = rich_s_t, width = 7.5, height = 5) #exporting the graph as png

#4. Total----
total <- tinv %>% 
  filter(occurrenceStatus == "present")

(abun_tot<-ggplot(total, aes(fill=order, y=individualCount,x=Plot))+ #plotting the abundance of inverts
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    labs(x = "Plots", y = "Number of Individuals", 
         title = "Number of Invertebrates sampled by pitfall and sweep-netting\nin Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Terrestrial_inverts/abun_tot.png", plot = abun_tot, width = 7.5, height = 5) #exporting the graph as png


(rich_tot<-ggplot(total, aes(fill=order, y=richness,x=Plot))+ #plotting the number of pifalls that are empty or not
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    labs(x = "Plots", y = "Family Richness", 
         title = "Invertebrate family richness sampled by pitfall and sweep-netting\nin Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Terrestrial_inverts/rich_tot.png", plot = rich_tot, width = 7.5, height = 5) #exporting the graph as png

