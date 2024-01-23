Dawson.trees.total <- read.csv("input/Dawson.Trees.With.Origin.csv")
#View(Dawson.trees.total)
str(Dawson.trees.total)
library(dplyr)
library(plyr)
library(psych)
length(unique(Dawson.trees.total$Site.Code))

#SLL subset

SLL.Species <- subset(Dawson.trees.total, Native_SLL=='Y', select=c(Site.Type, Site.Code))
SLL.Species <- SLL.Species %>% 
  add_count(Site.Code) %>%
  distinct()
str(SLL.Species)
SLL.Species <- dplyr::rename(SLL.Species, "SLL.per.site"=n)
SLL.Species$Region <- "SLL"
#View(SLL.Species)
sum(SLL.Species$SLL.per.site)

SLL.tot <- 

#SLL / GS TYPE
SLL.sp.matrix<- read.csv("output/d.SLL.sp.mat.csv")
str(SLL.sp.matrix)
abund.SLL.GS<-ddply(SLL.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE_NAT=sum(x[-1]))  
})
abund.SLL.GS$Region <- "SLL"
abund.SLL.GS

#SLL / SITE

SLL.site.sp.matrix<- read.csv("output/d.SLL.site.sp.mat.csv")
str(SLL.site.sp.matrix)
SLL.abund.site<-ddply(SLL.site.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE_NAT=sum(x[-1]))  
})
SLL.abund.site
SLL.abund.site$Region <- "SLL"
sum(SLL.abund.site$ABUNDANCE_NAT)

#ETF subset
ETF.Species <- subset(Dawson.trees.total, Native_ETF=='Y',select=c(Site.Type, Site.Code))
ETF.Species <- ETF.Species %>%
  add_count(Site.Code) %>%
  distinct() %>%
  dplyr::rename("ETF.per.site"=n)
ETF.Species$Region <- "ETF"
#View(ETF.Species)
sum(ETF.Species$ETF.per.site)

str(Dawson.trees.total)

#ETF / GS TYPE
ETF.sp.matrix<- read.csv("output/d.ETF.sp.mat.csv")z
str(ETF.sp.matrix)
abund.ETF.GS<-ddply(ETF.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE_NAT=sum(x[-1]))  
})
abund.ETF.GS$Region <- "ETF"
abund.ETF.GS
#ETF / SITE

ETF.site.sp.matrix<- read.csv("output/d.ETF.site.sp.mat.csv")
str(ETF.site.sp.matrix)
ETF.abund.site<-ddply(ETF.site.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE_NAT=sum(x[-1]))  
})
ETF.abund.site$Region <- "ETF"
sum(ETF.abund.site$ABUNDANCE_NAT)

#Total trees per site
Trees.per.site <- Dawson.trees.total[,4:5] %>%
  add_count(Site.Code) %>%
  distinct() %>%
  dplyr::rename("total.per.site"=n)
View(Trees.per.site)

#total trees per greenspace type 
tot.sp.matrix<- read.csv("output/d.total.sp.mat.csv")
str(tot.sp.matrix)
abund.total<-ddply(tot.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE=sum(x[-1]))  
})
abund.total

#total trees per site
site.sp.matrix<- read.csv("output/d.site.sp.mat.csv")
str(site.sp.matrix)
tot.abund.site<-ddply(site.sp.matrix, ~X, function(x) {
  data.frame(ABUNDANCE_TOT=sum(x[-1]))  
})
tot.abund.site

#Proportion SLL per site 

SLL.tot.sp.abund <- left_join(tot.abund.site, SLL.abund.site)
SLL.tot.sp.abund[is.na(SLL.tot.sp.abund)]<-'0'
View(SLL.tot.sp.abund)
str(SLL.tot.sp.abund)
SLL.tot.sp.abund$Proportions <- as.numeric(SLL.tot.sp.abund$ABUNDANCE_NAT)/SLL.tot.sp.abund$ABUNDANCE_TOT
SLL.tot.sp.abund$Region <- dplyr::recode(SLL.tot.sp.abund$Region, '0'="SLL")
SLL.tot.sp.abund
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
write.csv(prop.tot, "output/d.prop.abund.tot.csv")

#Proportion per site

SLL.proportions <- left_join(Trees.per.site, SLL.Species)
SLL.proportions[is.na(SLL.proportions)] <- 0
SLL.proportions$Region <- dplyr::recode(SLL.proportions$Region, '0'="SLL")
str(Native.proportions)
SLL.proportions$Proportion <- SLL.proportions$SLL.per.site/SLL.proportions$total.per.site

ETF.proportions <- left_join(Trees.per.site, ETF.Species)
ETF.proportions[is.na(ETF.proportions)] <-0
ETF.proportions$Region <- dplyr::recode(ETF.proportions$Region, '0'="ETF")
ETF.proportions$Proportion <- ETF.proportions$ETF.per.site/ETF.proportions$total.per.site

Dawson.proportions <- merge(SLL.proportions, ETF.proportions, all=TRUE)
write.csv(Dawson.proportions, "output/Dawson.proportions.csv")
##View(Dawson.proportions)

####
library(car)
str(Dawson.proportions)
hist(Dawson.proportions$Proportion)
View(Dawson.proportions)
glm.ab <- glm(Proportion~ Site.Type+Region,weights=total.per.site, family=binomial, Dawson.proportions)
Anova(glm.ab)
summary(glm.ab)

plot(glm.ab)
shapiro.test(residuals(glm.abundance))
bptest(glm.ab)


library(emmeans)
lsmeans(glm.ab, pairwise~Site.Type|Region, adjust="Tukey")

PseudoR2(glm.ab, which="Nagelkerke")


