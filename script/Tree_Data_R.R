install.packages("dplyr")
install.packages("tidyverse")

library(tidyverse)
library(dplyr)

public_trees <- read.csv("Public_Trees_Study_Area.csv")
sampled_trees <- read.csv("backyardtrees_lifetime_29_09_2021.csv")
names(sampled_trees)

sampled.trees.good <- select(sampled_trees, -c(created_at, uploaded_at, X12_Leaf, X13_Bark, X14_Full_Form, X15_FruitsFlowers_if_))
sampled.trees.good
names(sampled.trees.good)
View(sampled.trees.good)

unique(sampled.trees.good[c("X1_Land_Use_Type")])

rename(sampled.trees.good, Public ==Public)
