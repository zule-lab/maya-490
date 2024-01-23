trees.total <- read.csv("output/NDG_DAWSON_ORIGIN.csv")
View(trees.total)
str(trees.total)
library(dplyr)
library(plyr)
library(psych)
library(car)
library(DescTools)
length(unique(trees.total$Site.Code))
trees.total

#SLL subset

SLL.Species <- subset(trees.total, Native_SLL=='Y', select=c(Plot.code, Site.Type, Site.Code))
SLL.Species <- SLL.Species %>% 
  add_count(Site.Code) %>%
  distinct()
str(SLL.Species)
SLL.Species <- dplyr::rename(SLL.Species, "SLL.per.site"=n)
SLL.Species$Region <- "SLL"
View(SLL.Species)
sum(SLL.Species$SLL.per.site)

#SLL / GS TYPE
SLL.sp.matrix<- read.csv("output/SLL.sp.mat.csv")
str(SLL.sp.matrix)
abund.SLL.GS<-ddply(SLL.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE_NAT=sum(x[-1]))  
})
abund.SLL.GS$Region <- "SLL"

#SLL / SITE

SLL.site.sp.matrix<- read.csv("output/SLL.site.sp.mat.csv")
str(SLL.site.sp.matrix)
SLL.abund.site<-ddply(SLL.site.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE_NAT=sum(x[-1]))  
})
SLL.abund.site
SLL.abund.site$Region <- "SLL"
sum(SLL.abund.site$ABUNDANCE_NAT)

#ETF subset
ETF.Species <- subset(trees.total, Native_ETF=='Y',select=c(Plot.code, Site.Type, Site.Code))
ETF.Species <- ETF.Species %>%
  add_count(Site.Code) %>%
  distinct() %>%
  dplyr::rename("ETF.per.site"=n)
ETF.Species$Region <- "ETF"
View(ETF.Species)

#ETF / GS TYPE
ETF.sp.matrix<- read.csv("output/ETF.sp.mat.csv")
str(ETF.sp.matrix)
abund.ETF.GS<-ddply(ETF.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE_NAT=sum(x[-1]))  
})
abund.ETF.GS$Region <- "ETF"

#ETF / SITE

ETF.site.sp.matrix<- read.csv("output/ETF.site.sp.mat.csv")
str(ETF.site.sp.matrix)
ETF.abund.site<-ddply(ETF.site.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE_NAT=sum(x[-1]))  
})
ETF.abund.site$Region <- "ETF"
sum(ETF.abund.site$ABUNDANCE_NAT)

str(trees.total)

#Total trees per site
Trees.per.site <- trees.total[,c(3,6:7)] %>%
  add_count(Site.Code) %>%
  distinct() %>%
  dplyr::rename("total.per.site"=n)
View(Trees.per.site)

#total trees per greenspace type 
tot.sp.matrix<- read.csv("output/total.sp.mat.csv")
str(tot.sp.matrix)
abund.total<-ddply(tot.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE=sum(x[-1]))  
})
abund.total

#total trees per site
site.sp.matrix<- read.csv("output/site.sp.mat.csv")
str(site.sp.matrix)
tot.abund.site<-ddply(site.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE_TOT=sum(x[-1]))  
})
tot.abund.site

#Proportion SLL per site 

SLL.tot.sp.abund <- left_join(tot.abund.site, SLL.abund.site)
SLL.tot.sp.abund[is.na(SLL.tot.sp.abund)]<-'0'
View(SLL.tot.sp.abund)
SLL.tot.sp.abund$Proportions <- SLL.tot.sp.abund$ABUNDANCE_NAT/SLL.tot.sp.abund$ABUNDANCE_TOT
SLL.tot.sp.abund$Region <- dplyr::recode(SLL.tot.sp.abund$Region, '0'="SLL")
str(SLL.tot.sp.abund)
describe(SLL.tot.sp.abund$Proportions)

#Proportions ETF / site 

ETF.tot.sp.abund <- left_join(tot.abund.site, ETF.abund.site)
ETF.tot.sp.abund[is.na(ETF.tot.sp.abund)]<-0
ETF.tot.sp.abund$Region
ETF.tot.sp.abund$Proportions <- ETF.tot.sp.abund$ABUNDANCE_NAT/ETF.tot.sp.abund$ABUNDANCE_TOT
ETF.tot.sp.abund$Region <- dplyr::recode(ETF.tot.sp.abund$Region, '0'="ETF")
ETF.tot.sp.abund
describe(ETF.tot.sp.abund$Proportions)
str(ETF.tot.sp.abund)

#proportions per site (SLL & ETF)
prop.tot <- rbind(ETF.tot.sp.abund, SLL.tot.sp.abund)
View(prop.tot)
write.csv(prop.tot, "output/prop.abund.tot.csv")

#Proportion per site

SLL.proportions <- left_join(Trees.per.site, SLL.Species)
SLL.proportions[is.na(SLL.proportions)] <- 0
SLL.proportions$Proportion <- SLL.proportions$SLL.per.site/SLL.proportions$total.per.site
SLL.proportions$Region <- dplyr::recode(SLL.proportions$Region, '0'="SLL")
SLL.proportions

ETF.proportions <- left_join(Trees.per.site, ETF.Species)
ETF.proportions[is.na(ETF.proportions)] <-0
ETF.proportions$Proportion <- ETF.proportions$ETF.per.site/ETF.proportions$total.per.site
ETF.proportions$Region <- dplyr::recode(ETF.proportions$Region, '0'="ETF")

Native.proportions <- merge(SLL.proportions, ETF.proportions, all=TRUE)
View(Native.proportions)
write.csv(Native.proportions, "output/Native.abund.proportions.csv")



