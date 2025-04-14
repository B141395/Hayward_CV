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
bat<-read.csv("occurrence_dat/bat_occurrence.csv")#loading the bat data
#Bat data----
bat<-bat %>% 
  select(eventID,occurrenceStatus,scientificName)

bat.pa <- bat %>%
  pivot_wider(names_from=scientificName,values_from=c(occurrenceStatus))

list0 <- as.list(rep(0,ncol(bat.pa))) ##values toreplace the NAs
names(list0) <- names(bat.pa)
bat.pa <- as.data.frame(bat.pa %>%replace_na(list0)) ##replace the NAs by 0’s


bat.long<- bat.pa %>% 
  pivot_longer(cols = -"eventID", names_to = "Species", values_to = "occurrenceStatus")


bat.long <- bat.long %>% 
  mutate(Plot = case_when( #creating a new column called plot
    grepl("North", eventID, ignore.case = TRUE) ~ "Northern", #classifying eventID with "n" as Northern Plot
    grepl("South", eventID, ignore.case = TRUE) ~ "Southern", #classifying eventID with "s" as Southern Plot
    TRUE ~ "Unknown")) %>% 
  mutate(Habitat = case_when( #creating a new column called habitat
    grepl("1", eventID, ignore.case = TRUE) ~ "Woodland", #classifying eventID with "n" as Northern Plot
    grepl("2", eventID, ignore.case = TRUE) ~ "Aquatic", #classifying eventID with "s" as Southern Plot
    grepl("3", eventID, ignore.case = TRUE) ~ "Aquatic", #classifying eventID with "n" as Northern Plot
    grepl("4", eventID, ignore.case = TRUE) ~ "Woodland", #classifying eventID with "s" as Southern Plot
    TRUE ~ "Unknown"))

bat.long <- bat.long %>% 
  mutate(Site = paste(Plot,Habitat))

(bat_plot <- ggplot(bat.long, aes(x = Species, y = Site, fill = occurrenceStatus)) +
  geom_tile(color = "Black", size = 0.5,
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(low = "White",
                       mid = "grey",
                       high = "#FF0000",
                       midpoint = 0.5) +
  geom_text(aes(label = occurrenceStatus), color = "Black", fontface = "bold", size = 3)+
    labs(x = "Bat Species", y = "Site", 
         title = "Probability of Bat presence at each sampled site",
         fill = "Probability of Bat presence      ") +
    guides(fill = guide_colourbar(barwidth = 10,
                                  barheight = 0.5))+
    theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
          axis.text = element_text(size = 11),
          plot.title = element_text(size = 18, hjust = 0.5),
          axis.title = element_text(size = 16), 
          legend.position = "bottom",  # Set legend position to bottom
          legend.box = "horizontal")+   # Arrange legend items horizontally
    coord_fixed(ratio = 1))

ggsave(filename = "figures/Bats/bat_prob.png", plot = bat_plot, width = 10, height = 5) #exporting the graph as png


bat.long$Presence <- ifelse(bat.long$occurrenceStatus >= 0.5, 1, 0)
  
(bat_0.5 <- ggplot(bat.long, aes(x = Species, y = Site, fill = as.factor(Presence))) +
  geom_tile(color = "Black",size = 0.5,
            lwd = 1.5,
            linetype = 1) +
  scale_fill_manual(values = c("0" = "white", "1" = "#FF0000"),
                    labels = c("0" = "Absent", "1" = "Present")) +
    labs(x = "Bat species", y = "Sites", 
         title = "Bat presence with a 0.5 threshold at each sampled site",
         fill = "Bat presence",
         position = "bottom" ) + 
    theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
          axis.text = element_text(size = 11),
          plot.title = element_text(size = 18, hjust = 0.5),
          axis.title = element_text(size = 16), 
          legend.title = element_text(size = 13, face = "bold"), 
          legend.text = element_text(size = 11),
          legend.position = "bottom")+  # Set legend position to bottom
    coord_fixed(ratio = 1))
  
ggsave(filename = "figures/Bats/bat0.5.png", plot = bat_0.5, width = 10, height = 5) #exporting the graph as png

  