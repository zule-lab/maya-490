library(tidyverse)
library(car)
library(emmeans)
library(lmtest)

#summary stats
meta <- read.csv("output/NDG_DAWSON_ORIGIN.csv")
str(meta)
#View(meta)

Sites <- meta[, c(2:7)]
View(Sites)
Sites <- distinct(Sites)


Site.Rich <- read.csv("output/prop.rich.site.csv")
str(Site.Rich)
Site.Rich <- dplyr::rename(Site.Rich, "Proportion_RICH"= Proportion)
Site.Rich <- subset(Site.Rich, select = -c(X))
Site.Rich[order(Site.Rich[,'Site.Code']), ]
Meta.Rich <- dplyr::left_join(Sites, Site.Rich) 
ETF.Rich <- subset(Site.Rich, Region=="ETF")
sum(ETF.Rich$RICHNESS_NAT)


Dawson.ETF.Rich <- subset(ETF.Rich, Site.Code=="I1")
Dawson.ETF.Rich
View(Dawson.ETF.Rich)
sum(Dawson.ETF.Rich$RICHNESS_NAT)
SLL.Rich <- subset(Site.Rich, Region=="SLL")
sum(SLL.Rich$RICHNESS_NAT)
Dawson.SLL.Rich <- subset(SLL.Rich, Site.Code=="I1")
View(Dawson.SLL.Rich)
sum(Dawson.SLL.Rich$RICHNESS_NAT)

Sites.Abund <- read.csv("output/prop.abund.tot.csv")
str(Sites.Abund)
Sites.Abund <- dplyr::rename(Sites.Abund, "Proportion_ABUND"= Proportions)
Sites.Abund <- dplyr::rename(Sites.Abund, "Site.Code"=X)
Sites.Abund <- subset(Sites.Abund, select = -c(X.1))
str(Site.Rich)
Meta.Abund <- dplyr::left_join(Sites, Sites.Abund) 
View(Meta.Abund)
ETF.Abund <- subset(Sites.Abund, Region=="ETF")
str(ETF.Abund)
sum(ETF.Abund$ABUNDANCE_NAT)

##Dawson abundance
Dawson.ETF.Abund <- subset(ETF.Abund, Site.Code=="I1")
sum(Dawson.ETF.Abund$ABUNDANCE_NAT)
SLL.Abund <- subset(Sites.Abund, Region=="SLL")
SLL.Abund
Dawson.SLL.Abund <- subset(SLL.Abund, Site.Code=="I1")
sum(Dawson.SLL.Abund$ABUNDANCE_NAT)

134/261
138/261

Meta.Final <- merge(Meta.Abund, Meta.Rich)
View(Meta.Final)
write.csv(Meta.Final, "output/meta.analysis.csv")

#View(Meta.Rich)
head(Meta.Rich)
str(Meta.Rich)
hist(Meta.Rich$Proportion_RICH)
sum(Meta.Rich$Proportion_RICH==0)
sum(Meta.Rich$Proportion_RICH==1)

#richness model
glm.richness <- glm(Proportion_RICH~Plot.code+Site.Type+Region, data=Meta.Rich)
glm.richness
Anova(glm.richness)
glm.b.richness <- glm(Proportion_RICH~Plot.code+Site.Type+Region, family=binomial(link="logit"), data=Meta.Rich, weights = RICHNESS)
car::vif(glm.b.richness)
summary(glm.b.richness)
Anova(glm.b.richness)

##Assumptions
plot(glm.b.richness)

#normality
shapiro.test(resid(glm.b.richness))
qqnorm(residuals(glm.b.richness))
qqline(residuals(glm.b.richness))

#homogeneity
bptest(glm.b.richness)

#pairwise comparisons
lsmeans(glm.b.richness, pairwise ~ Site.Type | Region , adjust="Tukey")
PseudoR2(glm.b.richness, which = "Nagelkerke")

#abundance model

glm.b.abundance <- glm(Proportion_ABUND~Plot.code+Site.Type+Region, family=binomial, data=Meta.Abund, weights = ABUNDANCE_TOT)

library(rsq)
rsq(glm.b.abundance)
summary(glm.b.abundance)
car::Anova(glm.b.abundance)
print(Anova(glm.b.abundance))
hist(Meta.Abund$Proportion_ABUND)

##Assumptions
plot(glm.b.abundance)

#normality
shapiro.test(resid(glm.b.abundance))
qqnorm(residuals(glm.b.abundance))
qqline(residuals(glm.b.abundance))

#homogeneity
bptest(glm.b.abundance)

#pairwise comparisons
lsmeans(glm.b.abundance, pairwise ~ Site.Type | Region , adjust="Tukey")

PseudoR2(glm.b.richness, which = "Nagelkerke")

PseudoR2(glm.b.abundance, which="Nagelkerke")

