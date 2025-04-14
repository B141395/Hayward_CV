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
        axis.text = element_text(family = font, size = 10, color = "#222222"), 
        axis.text.x = element_text(margin = margin(5, b = 10)), 
        axis.title = element_text(family = font, size = 16, face = "bold", color = "#222222"), 
        axis.ticks = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 15, hjust = 0))
}

#Importing raw data----
ainv<-read.csv("occurrence_dat/aquatic_inverts_occurrence.csv")#loading the aquatic inverts data
#Aquatic inverts----

ainv <-ainv %>% 
  select(eventID,individualCount,phylum,class,order,family,genus) %>% #keeping the needed columns only
  mutate(Plot = case_when( #creating a new column called plot
    grepl("^N", eventID, ignore.case = TRUE) ~ "Northern Plot", #classifying eventID with "N" as Northern Plot
    grepl("^S", eventID, ignore.case = TRUE) ~ "Southern Plot", #classifying eventID with "S" as Southern Plot
    TRUE ~ "Unknown"
  )) 


Stream<-ainv %>%
  filter(str_detect(eventID, "Stream"))%>%
  mutate(family = ifelse(family == "", "unidentified", family))

Marsh<-ainv %>%
  filter(str_detect(eventID, "Marsh"))%>%
  mutate(family = ifelse(family == "", "unidentified", family))

(Stream_ab <- ggplot(Stream, aes(fill=family,y=individualCount,x=order))+ #plotting the individual birds observed per plot
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    facet_wrap(~Plot)+ #seperating the Northerna and Southern Plots for comparision
    labs(x = "Invertebrate Orders", y = "Individuals Observed", 
         title = "Aquatic Invertebrates sampled in streams")+
    style()+
    scale_fill_manual(values = c("Baetidae" = "#f8766d", 
                                 "Heptageniidae (synonym: Ecdyonuridae)" = "#00bc51",
                                 "Philopotamidae" = "#a58aff",
                                 "Simuliidae" = "#fb61d7",
                                 "unidentified" = "#6a6a6a",
                                 "Dixidae" = "#89ac00",
                                 "Leptoceridae" = "#29a3ff",
                                 "Capniidae" = "#d09400",
                                 "Hydrophilidae" = "#00c0b2"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(filename = "figures/Aquatic_inverts/stream_abun.png", plot = Stream_ab, width = 8, height = 5) #exporting the graph as png



(Marsh_ab <- ggplot(Marsh, aes(y=individualCount,x=order))+ #plotting the individual birds observed per plot
    geom_bar(stat = "identity",orientation = "x")+
    facet_wrap(~Plot)+ #seperating the Northerna and Southern Plots for comparision
    labs(x = "Invertebrate Orders", y = "Individuals Observed", 
         title = "Aquatic Invertebrates sampled in marshes")+
    style()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(filename = "figures/Aquatic_inverts/marsh_abun.png", plot = Marsh_ab, width = 8, height = 5) #exporting the graph as png


ainv_total_rich <- ainv %>% 
  distinct(order,Plot) %>% 
  mutate(richness=1)

(Ainv_total_rich <- ggplot(ainv_total_rich, aes(y=richness, x=Plot))+#Plotting the species richness by point count
    geom_bar(stat = "identity",orientation = "x")+
    labs(x = "Plots", y = "Order Richness", 
         title = "Total Aquatic Order richness sampled\nin Northern and Southern Plots")+
    style())

ggsave(filename = "figures/Aquatic_inverts/tot_rich.png", plot = Ainv_total_rich, width = 7, height = 4.5) #exporting the graph as png


BMWP<-ainv %>% 
  select(eventID,order,family,Plot) %>% #keeping the needed columns only
  mutate(BMWP=case_when( #creating a new column called BMWP
    grepl("Baetidae", family, ignore.case = TRUE) ~ "4", #giving different family or order their BMWP scores
    grepl("Simuliidae", family, ignore.case = TRUE) ~ "5", 
    grepl("Hydrophilidae", family, ignore.case = TRUE) ~ "5", 
    grepl("Heptageniidae (synonym: Ecdyonuridae)", family, ignore.case = TRUE) ~ "10", 
    grepl("Leptoceridae", family, ignore.case = TRUE) ~ "10", 
    grepl("Capniidae", family, ignore.case = TRUE) ~ "10", 
    grepl("Oligochaete", order, ignore.case = TRUE) ~ "1", 
    grepl("Gordioidea", order, ignore.case = TRUE) ~ "1", 
    TRUE ~ NA
  ))

BMWP <- BMWP[!is.na(BMWP$BMWP), ] #remove sample without a BMWP score

BMWP[BMWP == ""] <- NA #changing empty cells to NA


BMWP <- BMWP%>% 
  mutate(Habitat = case_when( #creating a new column called habitat
    grepl("Stream", eventID, ignore.case = TRUE) ~ "Stream", 
    grepl("Marsh", eventID, ignore.case = TRUE) ~ "Marsh", 
    TRUE ~ "Unknown"
  )) %>% 
 mutate(Habitat = paste(Plot,Habitat))%>%
  mutate(family = coalesce(family, order)) # for sample that wasnt identified to family level, take the order ID as family for easier calculation



BMWP <- BMWP[!duplicated(BMWP[c("family", "Habitat")]), ]#removing duplicated sample in each habitat

BMWP$BMWP <- as.numeric(as.character(BMWP$BMWP))#changing the BMWP scores to numeric

TotBMWP <- BMWP %>% 
  group_by(Habitat) %>%
  summarize(BMWP_Score= sum(BMWP, na.rm = TRUE))#summing the BMWP scores per sample for the Total BMWP score for the habitat

ASPT <- BMWP %>% 
  group_by(Habitat) %>%
  summarize(ASPT_Score= mean(BMWP, na.rm = TRUE))#finding the mean for the ASPT score

BMWP_wide <- merge(TotBMWP,ASPT, by="Habitat")

BMWP_long <- BMWP_wide %>%
  pivot_longer(cols = c("BMWP_Score","ASPT_Score"), names_to = "ScoreType", values_to = "Score")

(Scores<- ggplot(BMWP_long, aes(x = Habitat, y = Score, fill = ScoreType)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(y = "Score", x = "Habitat", fill = "Score Type",
         title = "Water Quality Scores for each habitat sampled")+
    scale_fill_discrete(name = "Score Type", labels = c("ASPT Score", "BMWP Score")) +
    style())

ggsave(filename = "figures/Aquatic_inverts/Scores.png", plot = Scores, width = 6.5, height = 4.5) #exporting the graph as png
