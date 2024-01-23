
library(tidyr)
library(tidyverse)
library(plyr)
library(dplyr)
library(psych)

Dawson.trees.total <- read.csv("input/Dawson.Trees.With.Origin.csv")

SLL.Species <- subset(Dawson.trees.total, Native_SLL=='Y', select=c(Site.Type, Site.Code))
SLL.Species <- SLL.Species %>% 
  add_count(Site.Type) %>%
  distinct()


#total species richness/greenspacetype
tot.sp.table <- read.csv("output/d.total.sp.mat.csv")
pr.total<-ddply(tot.sp.table, ~X, function(x) {
  data.frame(RICHNESS= sum((x[-1]>0)))})
names(pr.total)[names(pr.total)=='X'] <- 'GS.Type'
pr.total

#SLL sp richness/green space type 

SLL.sp.matrix<- read.csv("output/d.SLL.sp.mat.csv")
str(SLL.sp.matrix)
##View(SLL.sp.matrix)
SLL.per.total<-ddply(SLL.sp.matrix, ~X, function(x) {
  data.frame(RICHNESS_NAT= sum((x[-1]>0)))
})
SLL.per.total
names(SLL.per.total)[names(SLL.per.total)=='X'] <- 'GS.Type'
describe(SLL.per.total$RICHNESS_NAT)

#ETF sp richness/green space type 

ETF.sp.matrix<- read.csv("output/d.ETF.sp.mat.csv")
str(ETF.sp.matrix)
ETF.per.total<-ddply(ETF.sp.matrix, ~X, function(x) {
  data.frame(RICHNESS_NAT= sum((x[-1]>0)))
})
ETF.per.total
names(ETF.per.total)[names(ETF.per.total)=='X'] <- 'GS.Type'
describe(ETF.per.total$RICHNESS_NAT)

#SLL PROP/GREEN SPACE

SLL.rich.prop <- left_join(pr.total, SLL.per.total)
SLL.rich.prop[is.na(SLL.rich.prop)] <-0
View(SLL.rich.prop)
SLL.rich.prop$Proportion <- SLL.rich.prop$RICHNESS_NAT/SLL.rich.prop$RICHNESS
SLL.rich.prop$Region <- "SLL"
str(SLL.rich.prop)
SLL.rich.prop

#ETF PROP/GREENSPACE
ETF.rich.prop <- left_join(pr.total, ETF.per.total)
ETF.rich.prop[is.na(ETF.rich.prop)] <-0
ETF.rich.prop$Proportion <- ETF.rich.prop$RICHNESS_NAT/ETF.rich.prop$RICHNESS
ETF.rich.prop$Region <- "ETF"
str(ETF.rich.prop)

#JOIN ETF AND SLL (TOTAL PROPORTION PER GREEN SPACE TYPE)

prop.rich.tot <- rbind(ETF.rich.prop, SLL.rich.prop)
View(prop.rich.tot)
prop.rich.tot <- prop.rich.tot %>%
  distinct()
prop.rich.tot$Region <- recode(prop.rich.tot$Region, "0"="NON-NAT")
View(prop.rich.tot)
write.csv(prop.rich.tot, "output/d.prop.rich.tot.csv")

#total sp richness/site
site.sp.table <- read.csv("output/d.site.sp.mat.csv")
per.site.total<-ddply(site.sp.table, ~X, function(x) {
  data.frame(RICHNESS= sum((x[-1]>0)))
})
per.site.total
describe(per.site.total$RICHNESS)

#SLL sp richness/site
SLL.site.sp.matrix<- read.csv("output/d.SLL.site.sp.mat.csv")
str(SLL.site.sp.matrix)
SLL.per.site.total<-ddply(SLL.site.sp.matrix, ~X, function(x) {
  data.frame(RICHNESS_NAT= sum((x[-1]>0)))
})
SLL.per.site.total
describe(SLL.per.site.total$RICHNESS)

#ETF sp richness/site
ETF.site.sp.matrix<- read.csv("output/d.ETF.site.sp.mat.csv")
str(ETF.site.sp.matrix)
ETF.per.site.total<-ddply(ETF.site.sp.matrix, ~X, function(x) {
  data.frame(RICHNESS_NAT= sum((x[-1]>0)))
})
ETF.per.site.total
describe(ETF.per.site.total$RICHNESS)

#SLL.prop

SLL.rich.site.prop <- left_join(per.site.total, SLL.per.site.total)
SLL.rich.site.prop[is.na(SLL.rich.site.prop)] <-0
SLL.rich.site.prop$Proportion <- SLL.rich.site.prop$RICHNESS_NAT/SLL.rich.site.prop$RICHNESS
SLL.rich.site.prop$Region <- "SLL"
str(SLL.rich.site.prop)
SLL.rich.site.prop

#ETF PROP/SITE
ETF.rich.site.prop <- left_join(per.site.total, ETF.per.site.total)
ETF.rich.site.prop[is.na(ETF.rich.site.prop)] <-0
ETF.rich.site.prop$Proportion <- ETF.rich.site.prop$RICHNESS_NAT/ETF.rich.site.prop$RICHNESS
ETF.rich.site.prop$Region <- "ETF"
ETF.rich.site.prop
str(ETF.rich.site.prop)

#JOIN ETF AND SLL

prop.rich.site.tot <- rbind(ETF.rich.site.prop, SLL.rich.site.prop)
View(prop.rich.site.tot)
prop.rich.tot <- prop.rich.site.tot %>%
  distinct()
prop.rich.site.tot <- dplyr::rename(prop.rich.site.tot, "Site.Code"=X)
View(prop.rich.site.tot)
write.csv(prop.rich.site.tot, "output/d.prop.rich.site.csv")

##

####species richness per green space type (extra, not needed for analysis)
#sp richness/park
park.sp.table <- read.csv("output/d.parks.sp.mat.csv")
str(park.sp.table)
per.park.total<-ddply(park.sp.table,~X, function(x) {
  data.frame(RICHNESS= sum((x[-1]>0)))
})
per.park.total
describe(per.park.total$RICHNESS)

#sp richness/street

s.sp.table <- read.csv("output/d.s.sp.mat.csv")
str(s.sp.table)
per.s.total<-ddply(s.sp.table,~X, function(x) {
  data.frame(RICHNESS= sum((x[-1]>0)))
})
per.s.total
describe(per.s.total$RICHNESS)
#sp richness/yard

res.sp.table <- read.csv("output/d.res.sp.mat.csv")
str(res.sp.table)
per.res.total<-ddply(res.sp.table, ~X, function(x) {
  data.frame(RICHNESS= sum((x[-1]>0)))
})
per.res.total
describe(per.res.total$RICHNESS)
#sp richness/institute

ins.sp.table <- read.csv("output/d.ins.sp.mat.csv")
str(ins.sp.table)
per.ins.total<-ddply(ins.sp.table,~X, function(x) {
  data.frame(RICHNESS= sum((x[-1]>0)))
})
per.ins.total
describe(per.ins.total$RICHNESS)
#sp richness/allyeyway

a.sp.table <- read.csv("output/d.a.sp.mat.csv")
str(a.sp.table)
per.a.total<-ddply(a.sp.table,~X, function(x) {
  data.frame(RICHNESS= sum((x[-1]>0)))
})
per.a.total
describe(per.a.total$RICHNESS)

