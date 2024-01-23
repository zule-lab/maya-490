
library(tidyverse)
library(dplyr)
library(magrittr)

meta.data.dawson <- read.csv("input/Metadata_Dawson.csv", na.strings="")
#View(meta.data)
str(meta.data.dawson)

#get rid of NA and recode 
meta.data.dawson <- meta.data.dawson %>%
  drop_na(Site.Code) %>%
  distinct() %>%
  mutate(Species.Code=recode(Species.Code, LOPER="LOPE")) %>%
  mutate(Species.Code=recode(Species.Code, EU="LOPE")) %>%
  mutate(Species.Code=recode(Species.Code, HOLONICERALONICERAEU="LOPE")) %>%
  mutate(Species.Code=recode(Species.Code, ST="MGST")) %>%
  mutate(Species.Code=recode(Species.Code, WE="WEFL")) %>%
  mutate(Species_Common=recode(Species_Common, Wygeria ="Weigela Florida"))
  
meta.data.dawson$Cultivar.Code <- meta.data.dawson$Species.Code
meta.data.dawson$Genus.Code <- substr(meta.data.dawson$Species.Code, 1,2)
meta.data.dawson$Site.Type <- substr(meta.data.dawson$Site.Code, 1,1)
meta.data.dawson$Plot <- "Dawson"
meta.data.dawson$Plot.code <- "1"
  
meta.data.dawson <- meta.data.dawson %>%
  mutate(Species.Code = str_remove(Species.Code, "[XX]")) %>%
  mutate(Species.Code = str_remove(Species.Code, "[X]")) %>%
  mutate(Species.Code = substring(Species.Code, 1,4)) 
         
str(meta.data.dawson)
View(meta.data.dawson)
length(unique(meta.data.dawson$Cultivar.Code))
#161 cultivar species
length(unique(meta.data.dawson$Species.Code))
#tot species =137 
length(unique(meta.data.dawson$Genus.Code))
#tot genus=52

str(meta.data.dawson)
Dawson <- meta.data.dawson[,c(22:23,6:7, 21, 10, 20, 12, 19, 14, 11)]
str(Dawson)
#View(Dawson)
str(Dawson)
Dawson$Cultivar.Code <- as.factor(Dawson$Cultivar.Code)
Dawson.species <- distinct(Dawson, Cultivar.Code, .keep_all = TRUE)
#View(Dawson.species)
str(Dawson.species)
Dawson.species <- Dawson.species[,5:8]
#View(Dawson.species)
str(Dawson.species)
write.csv(Dawson.species, "output/dawson.species.list.csv")
spp.key <- read.csv("output/Master.key.csv")
str(spp.key)
View(spp.key)
str(spp.key)
str(Dawson.species)
Dawson.key <- Dawson.species %>% left_join(spp.key, by= c("Genus.Code","Species.Code"))
View(Dawson.key)
Dawson_missing <- dplyr::filter(Dawson.key, is.na(Native_SLL))
View(Dawson_missing)
write.csv(Dawson.key, file="output/Dawson.origin.csv")

Master <- read.csv("output/MASTER.DAWSON.csv", na.strings="NA")
#View(Master)
str(Master)
#Master <- Master[, c(2:5,7:10,12)]
View(Master)

Dawson.Native <- left_join(Dawson, Master)
View(Dawson.Native)
str(Dawson.Native)
Dawson.Native <- Dawson.Native %>% 
  mutate(Native_SLL =as.factor(Native_SLL)) %>%
  drop_na(Native_SLL) 
View(Dawson.Native)
str(Dawson.Native)
Dawson.Native <- Dawson.Native[, c(1:10,12, 11, 13:16)]
View(Dawson.Native)

write.csv(Dawson.Native, file="output/Dawson.Trees.With.Origin.csv")
