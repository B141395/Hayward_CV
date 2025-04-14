#Libraries----
library(ggplot2)
library(tidyverse)


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
bird_t <-read.csv("occurrence_dat/bird_transect_occurrence.csv") #import the bird transect data
bird_p <-read.csv("occurrence_dat/bird_point_occurrence.csv")

#1.transect data----
bird_t <-bird_t %>% 
  select(eventID,individualCount,scientificName,family,genus,species) %>% #keeping the needed columns only
  mutate(Plot = case_when( #creating a new column called plot
    grepl("^n", eventID, ignore.case = TRUE) ~ "Northern Plot", #classifying eventID with "n" as Northern Plot
    grepl("^s", eventID, ignore.case = TRUE) ~ "Southern Plot", #classifying eventID with "s" as Southern Plot
    TRUE ~ "Unknown"
  ))
  

(Bird_transect <- ggplot(bird_t, aes(fill=species, y=individualCount,x=family))+ #plotting the individual birds observed per plot
  geom_bar(stat = "identity",position = "stack",orientation = "x")+ 
  facet_wrap(~Plot)+ #seperating the Northerna and Southern Plots for comparision
  labs(x = "Bird Families", y = "Individuals Observed", 
       title = "Birds sampled in transects for Northern and Southern Plots")+
  style()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(filename = "figures/Birds/Birds_trans.png", plot = Bird_transect,width = 10, height = 6) #exporting the graph as png


(Bird_transect_tot <- ggplot(bird_t, aes(fill=species, y=individualCount,x=Plot))+ #plotting the individual birds observed per plot
    geom_bar(stat = "identity",position = "stack",orientation = "x")+ 
    labs(x = "Bird Families", y = "Individuals Observed", 
         title = "Birds abundance sampled by transects walks \n for Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Birds/Birds_trans_tot_abun.png", plot = Bird_transect_tot,width = 9, height = 7) #exporting the graph as png


bird_t <- bird_t %>% 
  mutate(richness=1) %>% #creating a column for species richness
  mutate(Trans = case_when( #creating a column for transect
    grepl("1", eventID, ignore.case = TRUE) ~ "Low",
    grepl("2", eventID, ignore.case = TRUE) ~ "Mid",    
    grepl("3", eventID, ignore.case = TRUE) ~ "High",
    TRUE ~ "Unknown"))#sorting the samples into different transects by their eventID 

bird_t$Trans <- factor(bird_t$Trans, levels = c("Low", "Mid", "High"))#ordering the factor levels


(Bird_transect_abun <- ggplot(bird_t, aes(fill=species, y=individualCount,x=Trans))+ #plotting the individual birds observed per plot
    geom_bar(stat = "identity",position = "stack",orientation = "x")+ 
    facet_wrap(~Plot)+ #seperating the Northerna and Southern Plots for comparision
    labs(x = "Bird Families", y = "Individuals Observed", 
         title = "Birds abundance sampled by transects walks\nfor Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Birds/Birds_trans_abun.png", plot = Bird_transect_abun,width = 10, height = 6) #exporting the graph as png


bird_t_rich <- bird_t%>% 
  distinct(scientificName,Plot) %>% 
  mutate(richness=1)

(Bird_t_rich <- ggplot(bird_t, aes(fill=species, y=richness, x=Trans))+ #Plotting the species richness by transect with species composition
  geom_bar(stat = "identity", postion = "stack",orientation = "x")+
    labs(x = "transects", y = "Species Richness", 
         title = "Bird species richness in each transect")+
    facet_wrap(~Plot)+#seperating the Northerna and Southern Plots for comparision
    style())
    

ggsave(filename = "figures/Birds/Birds_trans_rich.png", plot = Bird_t_rich,width = 10, height = 6) #exporting the graph as png


(Bird_t_rich_2 <- ggplot(bird_t_rich, aes(y=richness, x=Plot))+#Plotting the species richness by transect
    geom_bar(stat = "identity",orientation = "x")+
    labs(x = "Plots", y = "Species Richness", 
         title = "Bird species richness sampled by transect walks\nin Northern and Southern Plots")+
    style())


ggsave(filename = "figures/Birds/Birds_trans_rich_2.png", plot = Bird_t_rich_2,width = 7, height = 5) 


#2.Point Count Data----

bird_p <-bird_p %>% 
  select(eventID,individualCount,scientificName,family,genus,species) %>% #keeping the needed columns only
  mutate(Plot = case_when( #creating a new column called plot
    grepl("pointN", eventID, ignore.case = TRUE) ~ "Northern Plot", #classifying eventID with "n" as Northern Plot
    grepl("pointS", eventID, ignore.case = TRUE) ~ "Southern Plot", #classifying eventID with "s" as Southern Plot
    TRUE ~ "Unknown"
  ))

(Bird_Point <- ggplot(bird_p, aes(fill=species, y=individualCount,x=family))+ #plotting the individual birds observed per plot
    geom_bar(stat = "identity",position = "stack",orientation = "x")+ 
    facet_wrap(~Plot)+ #seperating the Northerna and Southern Plots for comparision
   labs(x = "Bird Families", y = "Individuals Observed", 
         title = "Birds sampled by point count in Northern and Southern Plots")+
    style()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))


ggsave(filename = "figures/Birds/Birds_point_abun.png", plot = Bird_Point,width = 10, height = 6) #exporting the graph as png

bird_p <- bird_p %>% #adding a column for species richness
  mutate(richness=1)

(Bird_p_rich <- ggplot(bird_p, aes(y=richness, x=Plot))+#Plotting the species richness by point count
    geom_bar(stat = "identity",orientation = "x")+
    labs(x = "Point Counts", y = "Species Richness", 
         title = "Birds species richness sampled by point count\nin Northern and Southern Plots")+
    style())


ggsave(filename = "figures/Birds/Birds_point_rich.png", plot = Bird_p_rich,width = 7.5, height = 6) #exporting the graph as png


(Bird_p_rich_2 <- ggplot(bird_p, aes(fill=species, y=richness, x=Plot))+ #Plotting the species richness by point count showing species composition
    geom_bar(stat = "identity", postion = "stack",orientation = "x")+ 
    labs(x = "Point Counts", y = "Species Richness", 
         title = "Birds species richness sampled by point count\nin Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Birds/Birds_point_rich_2.png", plot = Bird_p_rich_2,width = 7.5, height = 6) #exporting the graph as png

#3. Total----
bird_total <- bird_t %>% 
  select(-Trans)

bird_total <- rbind(bird_total,bird_p) %>% 
  select(-eventID)

(Bird_Total <- ggplot(bird_total, aes(fill=species, y=individualCount,x=family))+ #plotting the individual birds observed per plot
    geom_bar(stat = "identity",position = "stack",orientation = "x")+ 
    facet_wrap(~Plot)+ #seperating the Northerna and Southern Plots for comparision
    labs(x = "Bird Families", y = "Individuals Observed", 
         title = "Total Birds sampled by point count and transects\nin Northern and Southern Plots")+
    style()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(filename = "figures/Birds/Birds_tot_abun.png", plot = Bird_Total,width = 9, height = 7) #exporting the graph as png

bird_total_rich <- bird_total %>% 
  distinct(species,Plot) %>% 
  mutate(richness=1)

(Bird_tot_rich <- ggplot(bird_total_rich, aes(y=richness, x=Plot, fill=species))+#Plotting the species richness by point count
    geom_bar(stat = "identity",orientation = "x")+
    labs(x = "Plots", y = "Species Richness", 
         title = "Total Birds species richness sampled by point count and transects\nin Northern and Southern Plots")+
    style())
ggsave(filename = "figures/Birds/Birds_tot_rich.png", plot = Bird_tot_rich,width = 9, height = 7.5) #exporting the graph as png

