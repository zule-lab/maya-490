private <- read.csv("Private_Trees_10_07_2.csv")
public <- read.csv("Public_tree_sites.csv")
str(private)
str(public)
head(private)
head(public)
meta.data <- rbind(public, private)
meta.data
write.csv(meta.data, file="metadata.csv")
Dawson.trees <-read.csv("metadata.csv", header=T)
str(Dawson.trees)
dim(Dawson.trees)
summary(Dawson.trees)

unique(joined$Species.Code)

View(meta.data)
meta.data=meta.data[meta.data$Site.Code !="", ]
meta.data

install.packages("tidyverse")

library(tidyverse)
library(vegan)

## of species
unique(Dawson.trees$Species.Code)
length(unique(Dawson.trees$Species.Code))
write.csv(rbind, file="Dawson.Species.csv")

library(dplyr)

Dawson.trees %>% group_by(Site.Code) %>% add_count(Species.Code)


install.packages("funrar")
library(funrar)
dawson.matrix <- read.csv("Dawson.Species.Matrix.csv")
View(dawson.matrix)
rel.matrix <- make_relative(dawson.matrix)

rm(make_relative)
