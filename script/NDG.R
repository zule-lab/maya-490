packages <- c("tidyverse")
library(tidyverse)

NDG <- read.csv("input/Metadata_GS_CU .csv")
NDG
NDG$Cultivar.Code <- NDG$Species.Code
NDG$Genus.Code <- substr(NDG$Species.Code, 1,2)
NDG$Site.Type <- substr(NDG$Site.Code, 1,1)
NDG$Site.number <- substr(NDG$Site.Code, 2,4)
NDG$Site.number <- as.numeric(NDG$Site.number)

#unique sites (so as not to overlap with dawson)
NDG$Site.number <- NDG$Site.number + 1000
NDG$Site.number
#View(NDG)

NDG$Plot <- "Loyola" 
NDG$Plot.code <- "2"
NDG <- NDG %>%
  mutate(Species.Code=str_remove(Species.Code, "[XX]")) %>%
  mutate(Species.Code = str_remove(Species.Code, "[X]")) %>%
  mutate(Species.Code = substring(Species.Code, 1,4))  %>%
  mutate(Species.Code=dplyr::recode(Species.Code, LOY="LOXY")) %>% 
  mutate(Species.Code=dplyr::recode(Species.Code, 	SOA="SOXA"))
NDG$Site.Code <- paste(NDG$Site.Type, NDG$Site.number, sep="")
#View(NDG)
length(unique(NDG$Cultivar.Code))
#155 cultivar species
length(unique(NDG$Species.Code))
#tot species =149

str(NDG)

NDG_meta <- NDG[, c(20:21, 14, 13, 18, 3, 17, 4, 16, 5, 9)]
str(NDG_meta)
write.csv(NDG_meta, "output/meta_NDG.csv")

#masterlist

Master_NDG <- read.csv("input/TreeSpeciesList.csv")
str(Master_NDG)
Master_NDG <- Master_NDG[,c(1:7, 9, 11)]
str(Master_NDG)
Master_Dawson <- read.csv("output/MASTER.DAWSON.csv")
str(Master_Dawson)

Master_NDG_DAWSON <- rbind(Master_NDG, Master_Dawson)
Master_NDG_DAWSON <- distinct(Master_NDG_DAWSON, Species.Code, .keep_all = TRUE)
Master_NDG_DAWSON <- with(Master_NDG_DAWSON, Master_NDG_DAWSON[order(Species.Code),])
View(Master_NDG_DAWSON)
write.csv(Master_NDG_DAWSON, "output/Master.key.csv")


#assigning nativity 
NDG_trees <- read.csv("output/meta_NDG.csv")
Master_key <- read.csv("output/Master.key.csv")
str(Master_key)
NDG_Native <- dplyr::left_join(NDG_trees, Master_key, by= c("Genus.Code", "Species.Code"))
View(NDG_Native)
NDG_species <- dplyr::distinct(NDG_Native, Species.Code, .keep_all = TRUE)
View(NDG_species)
str(NDG_species)
NDG_missing <- dplyr::filter(NDG_species, is.na(Native_SLL))
View(NDG_missing)
write.csv(NDG_Native, "output/NDG_Origin.csv")

#joining Dawson and NDG 

NDG_Origin <- read.csv("output/NDG_Origin_good.csv")
str(NDG_Origin)

NDG_Origin <- NDG_Origin[,c(3:13, )]

Dawson_Trees <- read.csv("output/Dawson.Trees.With.Origin.csv")
str(Dawson_Trees)
Dawson_Trees <- Dawson_Trees[, c(2:17)]

setdiff(NDG_Origin, Dawson_Trees)

NDG_Dawson_With_Origin <- rbind(Dawson_Trees, NDG_Origin)

write.csv(NDG_Dawson_With_Origin, "output/NDG_DAWSON_ORIGIN.csv")

####Questions to Ask Carly

#should separate Dawson with neighbourhood analysis or complete all together? 
#for UQAM, no location points, how to create sites? 
#for westmount, still waiting, yet to be contacted next steps? 
#figure out how many 
#draft report due for December 1st 








