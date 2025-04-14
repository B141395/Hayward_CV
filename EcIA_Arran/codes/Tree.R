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
tree<-read.csv("occurrence_dat/tree_occurrence.csv")#loading the tree data

#Tree data----
tree<-tree%>% 
  select(eventID,individualCount,scientificName,family,genus,species) %>% #keeping the needed columns only
  mutate(Plot = case_when( #creating a new column called plot
    grepl("^n", eventID, ignore.case = TRUE) ~ "Northern Plot", #classifying eventID with "n" as Northern Plot
    grepl("^s", eventID, ignore.case = TRUE) ~ "Southern Plot", #classifying eventID with "s" as Southern Plot
    TRUE ~ "Unknown"))%>% 
    mutate(richness=1)%>%  #creating a column for species richness
    mutate(Trans = case_when( #creating a column for transect
      grepl("1", eventID, ignore.case = TRUE) ~ "Low",
      grepl("2", eventID, ignore.case = TRUE) ~ "Mid",    
      grepl("3", eventID, ignore.case = TRUE) ~ "High",
      TRUE ~ "Unknown"))#sorting the samples into different transects by their eventID 

tree$Trans <- factor(tree$Trans, levels = c("Low", "Mid", "High"))#ordering the factor levels

#Plotting Graphs----
#Abundance----
(t_abun<-ggplot(tree, aes(fill=species, y=individualCount,x=Plot))+ #plotting the abundance of inverts
   geom_bar(stat = "identity",position = "stack",orientation = "x")+
   labs(x = "Plots", y = "Number of Trees", 
        title = "Tree abundance in Northern and Southern Plots")+
   style())

ggsave(filename = "figures/Trees/tree_abun.png", plot = t_abun, width = 7.5, height = 5) #exporting the graph as png

#base on transect
(t_abun2<-ggplot(tree, aes(fill=species, y=individualCount,x=Trans))+ #plotting the abundanc if inverts base on transects
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    facet_wrap("Plot")+ #comparing the northern and souther plots
    labs(x = "transects", y = "Number of Trees", 
         title = "Tree abundance in Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Trees/tree_abun_trans.png", plot = t_abun2, width = 7.5, height = 5) #exporting the graph as png

#Species richness----
t_rich <- tree %>% 
  distinct(species,Plot,richness) #keeping dinstinct species per plot

(rich_t<-ggplot(t_rich, aes(fill=species, y=richness,x=Plot))+ #plotting the number of pifalls that are empty or not
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    labs(x = "Plots", y = "Species Richness", 
         title = "Tree species richness in Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Trees/tree_rich.png", plot = rich_t, width = 7.5, height = 5) #exporting the graph as png


#base on transect
t_rich_2 <- tree %>% 
  distinct(species,Plot,richness,Trans) 
(rich_t2<-ggplot(t_rich_2, aes(fill=species, y=richness,x=Trans))+ #plotting the number of pifalls that are empty or not
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    facet_wrap("Plot")+ #comparing the northern and souther plots
    labs(x = "transects", y = "Species Richness", 
         title = "Tree species richness in Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Trees/tree_rich_trans.png", plot = rich_t2, width = 7.5, height = 5) #exporting the graph as png
