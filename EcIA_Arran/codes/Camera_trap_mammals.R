#Libraries----
library(ggplot2)
library(tidyverse)
library(stringr)

#Style----
style <- function(){ #setting the style for my plots
  font <- "Helvetica"
  theme(plot.title = element_text(family = font, size = 18, face = "bold", color = "#222222", hjust = 0.5), 
        plot.subtitle = element_text(family = font, size = 18, margin = margin(9, 0, 9, 0)), 
        plot.caption = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text.align = 0, 
        legend.position = "bottom",
        legend.title = element_text(family = font, size = 9, face = "bold", color = "#222222",  hjust = 0.5), 
        legend.key = element_blank(), 
        legend.text = element_text(family = font, size = 9, color = "#222222"),
        axis.text = element_text(family = font, size = 15, color = "#222222"), 
        axis.text.x = element_text(margin = margin(5, b = 10)), 
        axis.title = element_text(family = font, size = 16, face = "bold", color = "#222222"), 
        axis.ticks = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 15, hjust = 0))
}

#Importing raw data----
camera_trap<-read.csv("occurrence_dat/camera_trap_occurrence.csv")#loading the camera trap data
#Mammals----
mammals<-camera_trap %>% 
  filter(class=="Mammalia") %>% 
  select(eventID,individualCount,scientificName,genus,species) %>% #keeping the needed columns only
  mutate(Plot = case_when( #creating a new column called plot
    grepl("^n", eventID, ignore.case = TRUE) ~ "Northern Plot", #classifying eventID with "n" as Northern Plot
    grepl("^s", eventID, ignore.case = TRUE) ~ "Southern Plot", #classifying eventID with "s" as Southern Plot
    TRUE ~ "Unknown"
  )) 

(Mabun <- ggplot(mammals, aes(fill=scientificName, y=individualCount,x=Plot))+ #plotting the mammal abundance
    geom_bar(stat = "identity",position = "stack",orientation = "x")+ 
    labs(x = "Plots", y = "Individuals Observed", 
         fill = "Species",
         title = "Mammal abundance sampled by camera traps\nfor Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Camera_Trap/Mammal_abun.png", plot = Mabun,width = 10, height = 6) #exporting the graph as png


mammals_rich<-mammals %>% 
  distinct(scientificName,Plot) %>% 
  mutate(richness=1)

(Mrich <- ggplot(mammals_rich, aes(fill=scientificName, y=richness,x=Plot))+ #plotting the mammal richness
    geom_bar(stat = "identity",position = "stack",orientation = "x")+ 
    labs(x = "Plots", y = "Species Richness", 
         fill = "Species",
         title = "Mammal species richness sampled by camera traps\nfor Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Camera_Trap/Mammal_rich.png", plot = Mrich,width = 10, height = 6) #exporting the graph as png
