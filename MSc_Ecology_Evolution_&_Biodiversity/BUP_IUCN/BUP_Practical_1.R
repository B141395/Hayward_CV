landuse <- read.csv("C:/Users/heiwu/Downloads/landuse/assessments.csv")
overexploitation <- read.csv("C:/Users/heiwu/Downloads/overexploitation/assessments.csv")
invasion <- read.csv("C:/Users/heiwu/Downloads/biologicalinvasions/assessments.csv")
pollution <- read.csv("C:/Users/heiwu/Downloads/pollution/assessments.csv")
climate <- read.csv("C:/Users/heiwu/Downloads/climatechange/assessments.csv")

lapply(pollution, class)
str(pollution)

climate$scientificName

climate$landuse <- 0
climate$overexploitation <- 0
climate$invasion <- 0
climate$pollution <- 0
climate$climate <- 1

landuse$landuse <- 1
landuse$overexploitation <- 0
landuse$invasion <- 0
landuse$pollution <- 0
landuse$climate <- 0

overexploitation$landuse <- 0
overexploitation$overexploitation <- 1
overexploitation$invasion <- 0
overexploitation$pollution <- 0
overexploitation$climate <- 0

invasion$landuse <- 0
invasion$overexploitation <- 0
invasion$invasion <- 1
invasion$pollution <- 0
invasion$climate <- 0

pollution$landuse <- 0
pollution$overexploitation <- 0
pollution$invasion <- 0
pollution$pollution <- 1
pollution$climate <- 0

threats.dat <- rbind(landuse, overexploitation, invasion, pollution, climate)

length(unique(threats.dat$scientificName))

dim(threats.dat)

threats.dup <- which(duplicated(threats.dat$scientificName))

threats.dat[threats.dup,1:23]

library(dplyr)

threats.redundant<-distinct(threats.dat[threats.dup,1:23])

for(i in 1:nrow(threats.redundant)){}

row.number <- which(threats.dat$scientificName==threats.redundant$scientificName[i])
#

row.selected <- threats.dat[row.number,]

new.row <-
data.frame(c(row.selected[1,1:23],colSums(row.selected[,24:28])))  
threats.dat <- threats.dat[-row.number,]
threats.dat <- rbind(threats.dat,new.row)
#

library(ggplot2)

threats.var <- c("landuse","overexploitation","invasion","pollution","climate")
threats.bar<-threats.dat[threats.var]

threats.bar <- colSums(threats.bar)

barplot(threats.bar)

colnames(threats.dat)

if(!require(devtools)) install.packages("devtools")
library(devtools)
devtools::install_github("gaospecial/ggVennDiagram")
library("ggVennDiagram")

x <-list()
for(i in 1:5){
  x[[i]]<-
    threats.dat$scientificName[which(threats.dat[23+i]==1)]
}
names(x)<-names(threats.dat[24:28])

ggVennDiagram(x,label_alpha = 0)
