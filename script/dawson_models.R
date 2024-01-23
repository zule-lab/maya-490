library(tidyverse)
library(car)
library(emmeans)
library(lmtest)
library(DescTools)

#summary stats
meta <- read.csv("output/Dawson.Trees.With.Origin.csv")
str(meta)
meta <- subset(meta, select=-c(X))
#View(meta)
str(Sites)

Sites <- meta[, c(1:6)]
str(Sites)
Sites <- subset(Sites, select=-c(GS.Type))
View(Sites)
Sites <- Sites %>%
  distinct()
length(unique(Sites$Site.Code))

#sample size of each land use type
I <- subset(Sites, Site.Type=="I")
I
length(unique(I$Site.Code))

S<- subset(Sites, Site.Type=="S")
length(unique(S$Site.Code))

Y <- subset(Sites, Site.Type=="Y")
length(unique(Y$Site.Code))

A <- subset(Sites, Site.Type=="A")
A
length(unique(A$Site.Code))

P <- subset(Sites, Site.Type=="P")
length(unique(P$Site.Code))

Site.Rich <- read.csv("output/d.prop.rich.site.csv")
str(Site.Rich)
Site.Rich <- dplyr::rename(Site.Rich, "Proportion_RICH"= Proportion)
Site.Rich <- subset(Site.Rich, select = -c(X))
Site.Rich[order(Site.Rich[,'Site.Code']), ]
Meta.Rich <- dplyr::left_join(Sites, Site.Rich) 

##for dawson presentation
ETF.Rich <- subset(Site.Rich, Region=="ETF")
sum(ETF.Rich$RICHNESS_NAT)
Dawson.ETF.Rich <- subset(ETF.Rich, Site.Code=="I1")
Dawson.ETF.Rich

#View(Dawson.ETF.Rich)
sum(Dawson.ETF.Rich$RICHNESS_NAT)
SLL.Rich <- subset(Site.Rich, Region=="SLL")
sum(SLL.Rich$RICHNESS_NAT)
Dawson.SLL.Rich <- subset(SLL.Rich, Site.Code=="I1")

#View(Dawson.SLL.Rich)
sum(Dawson.SLL.Rich$RICHNESS_NAT)

Sites.Abund <- read.csv("output/d.prop.abund.tot.csv")
str(Sites.Abund)
Sites.Abund <- dplyr::rename(Sites.Abund, "Proportion_ABUND"= Proportions)
Sites.Abund <- dplyr::rename(Sites.Abund, "Site.Code"=X)
Sites.Abund <- subset(Sites.Abund, select = -c(X.1))
str(Sites.Abund)
Meta.Abund <- dplyr::left_join(Sites, Sites.Abund) 
View(Meta.Abund)

##dawson presentation
ETF.Abund <- subset(Sites.Abund, Region=="ETF")
str(ETF.Abund)
sum(ETF.Abund$ABUNDANCE_NAT)
Dawson.ETF.Abund <- subset(ETF.Abund, Site.Code=="I1")
sum(Dawson.ETF.Abund$ABUNDANCE_NAT)
SLL.Abund <- subset(Sites.Abund, Region=="SLL")
SLL.Abund
Dawson.SLL.Abund <- subset(SLL.Abund, Site.Code=="I1")
sum(Dawson.SLL.Abund$ABUNDANCE_NAT)

Meta.Final <- merge(Meta.Abund, Meta.Rich)
View(Meta.Final)
Meta.Final <- Meta.Final %>%
  distinct()
write.csv(Meta.Final, "output/dawson.meta.analysis.csv")

#View(Meta.Rich)
head(Meta.Rich)
str(Meta.Rich)
hist(Meta.Rich$Proportion_RICH)
sum(Meta.Rich$Proportion_RICH==0)
sum(Meta.Rich$Proportion_RICH==1)

#richness model

glm.b.d.r<- glm(Proportion_RICH~Site.Type+Region, family=binomial(link="logit"), data=Meta.Rich, weights = RICHNESS)
car::vif(glm.b.d.r)
summary(glm.b.d.r)
car::Anova(glm.b.d.r)

##Assumptions
plot(glm.b.d.r)

#normality
shapiro.test(resid(glm.b.richness))
qqnorm(residuals(glm.b.richness))
qqline(residuals(glm.b.richness))

#homogeneity
bptest(glm.b.richness)

#pairwise comparisons
lsmeans(glm.b.d.r, pairwise ~ Site.Type | Region , adjust="Tukey")
PseudoR2(glm.b.d.r, which = "Nagelkerke")

#abundance model
aov.abundance <- aov(Proportion_ABUND~Site.Type*Region, data=Meta.Abund)
anova(aov.abundance)
plot(aov.abundance)
shapiro.test(resid(aov.abundance))
leveneTest(Proportion_ABUND~Site.Type*Region, data=Meta.Abund)
#aov = innapropriate, non-normal residuals and heteroscedasticity

glm.b.d.a <- glm(Proportion_ABUND~Site.Type+Region, family=binomial, data=Meta.Abund, weights = ABUNDANCE_TOT)

library(rsq)

summary(glm.b.d.a)
car::Anova(glm.b.d.a)
print(Anova(glm.b.d.a))

##Assumptions
plot(glm.b.d.a)

#pairwise comparisons
lsmeans(glm.b.d.a, pairwise ~ Site.Type | Region , adjust="Tukey")

PseudoR2(glm.b.d.a, which="Nagelkerke")

