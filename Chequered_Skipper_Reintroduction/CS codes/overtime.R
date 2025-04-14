library(tidyverse)
library(data.table)
library(rnrfa)
library(ggplot2)
library(OpenStreetMap)
library(rgdal)
library(dplyr)
library(ggpmisc)

style <- function(){
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
        panel.grid.minor = element_line(color = "#cbcbcb"), 
        panel.grid.major.y = element_line(color = "#cbcbcb"), 
        panel.grid.major.x = element_line(color = "#cbcbcb"), 
        panel.background = element_blank(), 
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(size = 12, hjust = 0))
}



cs_bnm <-read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/Chequered Skipper Scotland.csv", stringsAsFactors = FALSE)
cs_bnm <- cs_bnm %>% subset(km_sq !="")
cs_bnm <- cs_bnm[cs_bnm$rec_year >= 2000,]
cs_bnm <- subset(cs_bnm, rec_month != 0)
cs_bnm<- cs_bnm %>%
  mutate(adults = parse_number(adults))
cs_bnm[is.na(cs_bnm)] = 0
cs_bnm<- cs_bnm %>%
  filter(is.finite(adults))
cs_bnm <- cs_bnm %>% mutate(ID = X_uid_)
cs_bnm<- cs_bnm %>%  subset(adults !="0")
bnm_sum <- cs_bnm %>% group_by(rec_year) %>%
  summarise(Abundance = sum(adults))
bnm_year <- cs_bnm %>% group_by(rec_year) %>% mutate(nR = n_distinct(record_dat))
bnm_year <- bnm_year[!duplicated(bnm_year$rec_year), ]
bnm_year <- bnm_year %>% select(rec_year, nR)
bnm_total <- merge(bnm_sum,bnm_year, by="rec_year")
bnm_total <- bnm_total %>% mutate (year.scaled = (rec_year - 1999))

ggplot(data=bnm_total, # Creating plot and naming it Fig_1
       aes(x=rec_year, y = Abundance))+ # graph populations of individual species over years
  geom_smooth(method='lm',size=0.5)+ # add trend lines to the plot
  geom_point(size=0.5)+ # add data points to the plot
  style()+
  xlab("Year")+ # add x-axis title
  ylab("Recorded Abundance") +  # add y-axis title
  theme(plot.title = element_text(size = 15, hjust = 0.5))

ggplot(data=bnm_total, # Creating plot and naming it Fig_1
       aes(x=rec_year, y = nR))+ # graph populations of individual species over years
  geom_smooth(method='lm',size=0.5)+ # add trend lines to the plot
  geom_point(size=0.5)+ # add data points to the plot
  style()+
  xlab("Year")+ # add x-axis title
  ylab("Presence Records") +  # add y-axis title
  theme(plot.title = element_text(size = 15, hjust = 0.5))

ggplot(data=bnm_total, # Creating plot and naming it Fig_1
       aes(x=rec_year, y = Abundance))+ # graph populations of individual species over years
  geom_bar(stat="identity")+ # add data points to the plot
  style()+
  xlab("Year")+ # add x-axis title
  ylab("Presence Records")

bnm_log <- bnm_total %>% mutate(logpop=log(Abundance))
my.formula = y~x

ggplot(data=bnm_log, # Creating plot and naming it Fig_1
       aes(x=rec_year, y = logpop))+ # graph populations of individual species over years
  geom_smooth(method='lm',size=0.5)+ # add trend lines to the plot
  geom_point(size=0.5)+ # add data points to the plot
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  xlab("Year")+ # add x-axis title
  ylab("log(Abundance)") +  # add y-axis title
  theme(plot.title = element_text(size = 15, hjust = 0.5))

ggplot(data=bnm_log, # Creating plot and naming it Fig_1
       aes(x=rec_year, y = Abundance))+ # graph populations of individual species over years
  geom_smooth(method='lm',size=0.5)+ # add trend lines to the plot
  geom_point(size=0.5)+ # add data points to the plot
  style()+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  xlab("Year")+ # add x-axis title
  ylab("Abundance") +  # add y-axis title
  theme(plot.title = element_text(size = 15, hjust = 0.5))
bnm_avg <- bnm_log %>% mutate(avg=Abundance/nR)

ggplot(data=bnm_avg, # Creating plot and naming it Fig_1
       aes(x=rec_year, y = avg))+ # graph populations of individual species over years
  geom_smooth(method='lm',size=0.5)+ # add trend lines to the plot
  geom_point(size=0.5)+ # add data points to the plot
  style()+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  xlab("Year")+ # add x-axis title
  ylab("Average Abundance per Presence record") +  # add y-axis title
  theme(plot.title = element_text(size = 15, hjust = 0.5))

ggplot(data=bnm_total, # Creating plot and naming it Fig_1
       aes(x=rec_year, y = nR))+ # graph populations of individual species over years
  geom_smooth(method='lm',size=0.5)+ # add trend lines to the plot
  geom_point(size=0.5)+ # add data points to the plot
  style()+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  xlab("Year")+ # add x-axis title
  ylab("Presence Records") +  # add y-axis title
  theme(plot.title = element_text(size = 15, hjust = 0.5))

bm1<- lm(logpop ~ year.scaled,
         data = bnm_log)
summary(bm1)#F-statistic: 5.342 on 1 and 18 DF,  p-value: 0.03286

bm2<- lm(Abundance ~ year.scaled,
         data = bnm_log)
summary(bm2)#F-statistic: 5.693 on 1 and 18 DF,  p-value: 0.02822


bm3 <- lm(Abundance ~ rec_year,
          data = bnm_log)
summary(bm3) #F-statistic: 5.693 on 1 and 18 DF,  p-value: 0.02822
anova(bm1)


bm4 <- lm(avg ~ rec_year,
          data = bnm_avg)

plot(bm4)

summary(bm4)#F-statistic: 5.723 on 1 and 18 DF,  p-value: 0.02787

