setwd("~/Documents/GitHub/Trees/Tree_Data")
meta.csv <- read.csv("input/Dawson.Trees.With.Origin.csv")
library(tidyverse)

##table that summarizes species by green space type
total.sp.table <- table(meta.csv$Species.Code, meta.csv$GS.Type)
list.total.sp.table <- as.data.frame(total.sp.table)
total.sp.wide <- spread(list.total.sp.table, "Var1", "Freq")
rownames(total.sp.wide) <- total.sp.wide$Var2
total.sp.mat<-subset(total.sp.wide, select = -c(Var2))
#View(total.sp.mat)
write.csv(total.sp.mat, file="output/d.total.sp.mat.csv")

#SLL total species per green space type 

SLL.data <- subset(meta.csv, meta.csv$Native_SLL=="Y")
length(unique(SLL.data$Species.Code))
SLL.sp.table <- table(SLL.data$Species.Code,SLL.data$GS.Type) 
write.csv(SLL.sp.table, file="output/d.SLL.sp.table.csv")
SLL.sp.df <- as.data.frame(SLL.sp.table)
SLL.sp.wide <-spread(SLL.sp.df, "Var1", "Freq")
rownames(SLL.sp.wide) <- SLL.sp.wide$Var2
str(SLL.sp.wide)
#View(SLL.sp.wide)
SLL.sp.mat <- subset(SLL.sp.wide, select=-c(Var2))
write.csv(SLL.sp.mat, file="output/d.SLL.sp.mat.csv")

#ETF PER GREENSPACE TYPE
49/134
63/134
390/2700
570/2700


ETF.data <- subset(meta.csv, meta.csv$Native_ETF=="Y")
length(unique(ETF.data$Species.Code))
ETF.sp.table <- table(ETF.data$Species.Code,ETF.data$GS.Type) 
write.csv(ETF.sp.table, file="d.output/ETF.sp.table.csv")
ETF.sp.df <- as.data.frame(ETF.sp.table)
ETF.sp.wide <-spread(ETF.sp.df, "Var1", "Freq")
rownames(ETF.sp.wide) <- ETF.sp.wide$Var2
str(ETF.sp.wide)
ETF.sp.mat <- subset(ETF.sp.wide, select=-c(Var2))
write.csv(ETF.sp.mat, file="output/d.ETF.sp.mat.csv")

##table that summarizes species by site code
length(unique(meta.csv$Site.Code))
site.sp.table <- table(meta.csv$Species.Code, meta.csv$Site.Code)
list.site.sp.table<- as.data.frame(site.sp.table)
site.sp.wide <- spread(list.site.sp.table, "Var1", "Freq")
rownames(site.sp.wide) <- site.sp.wide$Var2
site.sp.mat<-subset(site.sp.wide, select = -c(Var2))
#View(site.sp.mat)
write.csv(site.sp.mat, file="output/d.site.sp.mat.csv")

#native spp/site 

SLL.site.sp.table <- table(SLL.data$Species.Code, SLL.data$Site.Code)
SLL.site.sp.df<- as.data.frame(SLL.site.sp.table)
SLL.site.sp.wide <- spread(SLL.site.sp.df, "Var1", "Freq")
rownames(SLL.site.sp.wide) <- SLL.site.sp.wide$Var2
SLL.site.sp.mat<-subset(SLL.site.sp.wide, select = -c(Var2))
#View(SLL.site.sp.mat)
write.csv(SLL.site.sp.mat, file="output/d.SLL.site.sp.mat.csv")


#ETF PER SITE 
ETF.site.sp.table <- table(ETF.data$Species.Code, ETF.data$Site.Code)
ETF.site.sp.df<- as.data.frame(ETF.site.sp.table)
ETF.site.sp.wide <- spread(ETF.site.sp.df, "Var1", "Freq")
rownames(ETF.site.sp.wide) <- ETF.site.sp.wide$Var2
ETF.site.sp.mat<-subset(ETF.site.sp.wide, select = -c(Var2))
#View(ETF.site.sp.mat)
write.csv(ETF.site.sp.mat, file="output/d.ETF.site.sp.mat.csv")

#table that summarizes private vs public (govenernance)

m.sp.table <- table(meta.csv$Species.Code, meta.csv$Classification)
write.csv(m.sp.table, file="output/d.gm.sp.table.csv")
m.sp.df<- as.data.frame(m.sp.table)
str(m.sp.df)
m.sp.wide <- spread(m.sp.df, "Var1", "Freq")
rownames(m.sp.wide) <- m.sp.table$Var2
View(m.sp.table)
str(m.sp.table)
m.sp.mat <- m.sp.wide[, 2:131]
#View(m.sp.mat)
write.csv(m.sp.mat, file="output/d.m.sp.mat.csv")

#table that summarizes SLL species by governance/management type 

m.SLL.sp.table <- table(SLL.data$Species.Code, SLL.data$Classification)
write.csv(m.SLL.sp.table, file="output/d.m.SLL.sp.table.csv")
m.SLL.sp.df<- as.data.frame(m.SLL.sp.table)
str(m.SLL.sp.df)
m.SLL.sp.wide <- spread(m.SLL.sp.df, "Var1", "Freq")
rownames(m.SLL.sp.wide) <- m.SLL.sp.wide$Var2
View(m.SLL.sp.table)
str(m.SLL.sp.table)
m.SLL.sp.mat <- m.SLL.sp.wide[, 2:49]
View(m.SLL.sp.mat)
write.csv(m.SLL.sp.mat, file="output/d.m.SLL.sp.mat.csv")

#table that summarizes ETF species by governance/management type 

m.ETF.sp.table <- table(ETF.data$Species.Code, ETF.data$Classification)
write.csv(m.ETF.sp.table, file="output/m.ETF.sp.table.csv")
m.ETF.sp.df<- as.data.frame(m.ETF.sp.table)
str(m.ETF.sp.df)
m.ETF.sp.wide <- spread(m.ETF.sp.df, "Var1", "Freq")
rownames(m.ETF.sp.wide) <- m.ETF.sp.wide$Var2
str(m.ETF.sp.table)
m.ETF.sp.mat <- m.ETF.sp.wide[, 2:63]
write.csv(m.ETF.sp.mat, file="output/d.m.ETF.sp.mat.csv")

#table that summarizes species --> Residential 
meta.csv$Site.Type
res.data <- subset(meta.csv, meta.csv$Site.Type=="Y")
#View(res.data)
res.sp.table <- table(res.data$Species.Code, res.data$Site.Code)
write.csv(res.sp.table, file="output/d.res.sp.table.csv")
res.sp.df <- as.data.frame(res.sp.table)
str(res.sp.df)
res.wide <- spread(res.sp.df, "Var1", "Freq")
rownames(res.wide) <- res.wide$Var2
str(res.wide)
res.sp.mat <- res.wide[, 2:89]
write.csv(res.sp.mat, file="output/d.res.sp.mat.csv")

#table that summarizes SLL species --> Residential 
res.SLL.data <- subset(SLL.data, SLL.data$Site.Type=="R")
View(res.SLL.data)
res.SLL.sp.table <- table(res.SLL.data$Species.Code, res.SLL.data$Site.Code)
res.SLL.sp.df <- as.data.frame(res.SLL.sp.table)
res.SLL.wide <- spread(res.SLL.sp.df, "Var1", "Freq")
rownames(res.SLL.wide) <- res.SLL.wide$Var2
str(res.SLL.wide)
res.SLL.sp.mat <- res.SLL.wide[, 2:41]
write.csv(res.SLL.sp.mat, file="output/d.res.SLL.sp.mat.csv")

#table that summarizes ETF species --> Residential 
res.ETF.data <- subset(ETF.data, ETF.data$Site.Type=="R")
View(res.ETF.data)
res.ETF.sp.table <- table(res.ETF.data$Species.Code, res.ETF.data$Site.Code)
res.ETF.sp.df <- as.data.frame(res.ETF.sp.table)
res.ETF.wide <- spread(res.ETF.sp.df, "Var1", "Freq")
rownames(res.ETF.wide) <- res.ETF.wide$Var2
str(res.ETF.wide)
res.ETF.sp.mat <- res.ETF.wide[, 2:53]
write.csv(res.ETF.sp.mat, file="output/d.res.ETF.sp.mat.csv")

#species / institutional
ins.data <- subset(meta.csv, meta.csv$Site.Type=="I")
ins.sp.table <- table(ins.data$Species.Code, ins.data$Site.Code) 
write.csv(ins.sp.table, file="output/d.ins.sp.table.csv")
ins.sp.df <- as.data.frame(ins.sp.table)
ins.sp.wide <-spread(ins.sp.df, "Var1", "Freq")
rownames(ins.sp.wide) <- ins.sp.wide$Var2
str(ins.sp.wide)
ins.sp.mat <- ins.sp.wide[, 2:53]
write.csv(ins.sp.mat, file="output/d.ins.sp.mat.csv")

#species (SLL) / institutional
ins.SLL.data <- subset(SLL.data, SLL.data$Site.Type=="I")
ins.SLL.sp.table <- table(ins.SLL.data$Species.Code, ins.SLL.data$Site.Code) 
ins.SLL.sp.df <- as.data.frame(ins.SLL.sp.table)
ins.SLL.sp.wide <-spread(ins.SLL.sp.df, "Var1", "Freq")
rownames(ins.SLL.sp.wide) <- ins.SLL.sp.wide$Var2
str(ins.SLL.sp.wide)
View(ins.SLL.sp.wide)
ins.SLL.sp.mat <- ins.SLL.sp.wide[, 2:28]
write.csv(ins.SLL.sp.mat, file="output/d.ins.SLL.sp.mat.csv")

#species (ETF) / institutional
ins.ETF.data <- subset(ETF.data, ETF.data$Site.Type=="I")
ins.ETF.sp.table <- table(ins.ETF.data$Species.Code, ins.ETF.data$Site.Code) 
ins.ETF.sp.df <- as.data.frame(ins.ETF.sp.table)
ins.ETF.sp.wide <-spread(ins.ETF.sp.df, "Var1", "Freq")
rownames(ins.ETF.sp.wide) <- ins.ETF.sp.wide$Var2
str(ins.ETF.sp.wide)
ins.ETF.sp.mat <- ins.ETF.sp.wide[, 2:32]
write.csv(ins.ETF.sp.mat, file="output/d.ins.ETF.sp.mat.csv")

#species/park (will be adding more information with westmount)
parks.data <- subset(meta.csv, meta.csv$Site.Type=="P")
parks.sp.table <- table(parks.data$Species.Code, parks.data$Site.Code) 
write.csv(parks.sp.table, file="output/d.parks.sp.table.csv")
parks.sp.df <- as.data.frame(parks.sp.table)
parks.sp.wide <-spread(parks.sp.df, "Var1", "Freq")
rownames(parks.sp.wide) <- parks.sp.wide$Var2
str(parks.sp.wide)
parks.sp.mat <- parks.sp.wide[, 2:23]
write.csv(parks.sp.mat, file="output/d.parks.sp.mat.csv")

#species SLL/park (will be adding more information with westmount)
parks.SLL.data <- subset(SLL.data, SLL.data$Site.Type=="P")
parks.SLL.sp.table <- table(parks.SLL.data$Species.Code, parks.SLL.data$Site.Code) 
parks.SLL.sp.df <- as.data.frame(parks.SLL.sp.table)
parks.SLL.sp.wide <-spread(parks.SLL.sp.df, "Var1", "Freq")
rownames(parks.SLL.sp.wide) <- parks.SLL.sp.wide$Var2
str(parks.SLL.sp.wide)
parks.SLL.sp.mat <- parks.SLL.sp.wide[, 2:11]
write.csv(parks.SLL.sp.mat, file="output/d.parks.SLL.sp.mat.csv")

#species ETF/park (will be adding more information with westmount)
parks.ETF.data <- subset(ETF.data, ETF.data$Site.Type=="P")
parks.ETF.sp.table <- table(parks.ETF.data$Species.Code, parks.ETF.data$Site.Code) 
parks.ETF.sp.df <- as.data.frame(parks.ETF.sp.table)
parks.ETF.sp.wide <-spread(parks.ETF.sp.df, "Var1", "Freq")
rownames(parks.ETF.sp.wide) <- parks.ETF.sp.wide$Var2
str(parks.ETF.sp.wide)
parks.ETF.sp.mat <- parks.ETF.sp.wide[, 2:13]
write.csv(parks.ETF.sp.mat, file="output/d.parks.ETF.sp.mat.csv")

#species/street

s.data <- subset(meta.csv, meta.csv$Site.Type=="S")
s.sp.table <- table(s.data$Species.Code, s.data$Site.Code) 
write.csv(s.sp.table, file="output/d.s.sp.table.csv")
s.sp.df <- as.data.frame(s.sp.table)
s.sp.wide <-spread(s.sp.df, "Var1", "Freq")
rownames(s.sp.wide) <- s.sp.wide$Var2
str(s.sp.wide)
s.sp.mat <- s.sp.wide[, 2:42]
write.csv(s.sp.mat, file="output/d.s.sp.mat.csv")

#species SLL/street

s.SLL.data <- subset(SLL.data, SLL.data$Site.Type=="S")
s.SLL.sp.table <- table(s.SLL.data$Species.Code, s.SLL.data$Site.Code) 
s.SLL.sp.df <- as.data.frame(s.SLL.sp.table)
s.SLL.sp.wide <-spread(s.SLL.sp.df, "Var1", "Freq")
rownames(s.SLL.sp.wide) <- s.SLL.sp.wide$Var2
str(s.SLL.sp.wide)
s.SLL.sp.mat <- s.SLL.sp.wide[, 2:17]
write.csv(s.SLL.sp.mat, file="output/d.s.SLL.sp.mat.csv")

#species ETF/street

s.ETF.data <- subset(ETF.data, ETF.data$Site.Type=="S")
s.ETF.sp.table <- table(s.ETF.data$Species.Code, s.ETF.data$Site.Code) 
s.ETF.sp.df <- as.data.frame(s.ETF.sp.table)
s.ETF.sp.wide <-spread(s.ETF.sp.df, "Var1", "Freq")
rownames(s.ETF.sp.wide) <- s.ETF.sp.wide$Var2
str(s.ETF.sp.wide)
s.ETF.sp.mat <- s.ETF.sp.wide[, 2:24]
write.csv(s.ETF.sp.mat, file="output/d.s.ETF.sp.mat.csv")

# species/alleyway

a.data <- subset(meta.csv, meta.csv$Site.Type=="A")
a.sp.table <- table(a.data$Species.Code,a.data$Site.Code) 
write.csv(a.sp.table, file="output/d.a.sp.table.csv")
a.sp.df <- as.data.frame(a.sp.table)
a.sp.wide <-spread(a.sp.df, "Var1", "Freq")
rownames(a.sp.wide) <- a.sp.wide$Var2
str(a.sp.wide)
a.sp.mat <- a.sp.wide[, 2:28]
write.csv(a.sp.mat, file="output/d.a.sp.mat.csv")

# species SLL/alleyway

a.SLL.data <- subset(SLL.data, SLL.data$Site.Type=="A")
a.SLL.sp.table <- table(a.SLL.data$Species.Code,a.SLL.data$Site.Code) 
a.SLL.sp.df <- as.data.frame(a.SLL.sp.table)
a.SLL.sp.wide <-spread(a.SLL.sp.df, "Var1", "Freq")
rownames(a.SLL.sp.wide) <- a.SLL.sp.wide$Var2
str(a.SLL.sp.wide)
a.SLL.sp.mat <- a.SLL.sp.wide[, 2:16]
write.csv(a.SLL.sp.mat, file="output/d.a.SLL.sp.mat.csv")


# species ETF/alleyway

a.ETF.data <- subset(ETF.data, ETF.data$Site.Type=="A")
a.ETF.sp.table <- table(a.ETF.data$Species.Code,a.ETF.data$Site.Code) 
a.ETF.sp.df <- as.data.frame(a.ETF.sp.table)
a.ETF.sp.wide <-spread(a.ETF.sp.df, "Var1", "Freq")
rownames(a.ETF.sp.wide) <- a.ETF.sp.wide$Var2
str(a.ETF.sp.wide)
a.ETF.sp.mat <- a.ETF.sp.wide[, 2:18]
write.csv(a.ETF.sp.mat, file="output/d.a.ETF.sp.mat.csv")