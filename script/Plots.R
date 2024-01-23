meta <- read.csv("output/dawson.meta.analysis.csv")
View(meta)
library(ggplot2)
library(gridExtra)


meta.SLL <- subset(meta, Region=="SLL")
meta.ETF <- subset(meta, Region=="ETF")

SLL <- ggplot(meta.SLL, aes(Site.Type, Proportion_ABUND, fill=Site.Type)) +
  geom_boxplot()+
  geom_point() +
  ggtitle("St Lawrence Lowlands") +
  labs(x="Land Use Type", y="Proportion of Native Trees", title="St Lawrence Lowlands")+
  theme(legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))

  
ETF <- ggplot(meta.ETF, aes(Site.Type, Proportion_ABUND, fill=Site.Type)) +
    geom_boxplot()+
    geom_point() +
    ggtitle("Eastern Temperate Forest") +
    labs(x="Land Use Type", y="Proportion of Native Trees", title="Eastern Temperate Forest")+
  theme( legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
ETF
grid.arrange(SLL, ETF, ncol=2)
ggsave("output/Abund_Proportion_Boxplot.png")

#

SLL <- ggplot(meta.SLL, aes(Site.Type, Proportion_RICH, fill=Site.Type)) +
  geom_boxplot()+
  geom_point() +
  ggtitle("St Lawrence Lowlands") +
  labs(x="Land Use Type", y="Proportion of Native Species", title="St Lawrence Lowlands")+
  theme(legend.position="none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))


ETF <- ggplot(meta.ETF, aes(Site.Type, Proportion_RICH, fill=Site.Type)) +
  geom_boxplot()+
  geom_point() +
  ggtitle("St Lawrence Lowlands") +
  labs(x="Land Use Type", y="Proportion of Native Species", title="Eastern Temperate Forest")+
  theme( legend.position="none",axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
ETF
grid.arrange(SLL, ETF, ncol=2)
ggsave("output/Abund_Proportion_Boxplot.png")

top.5 <- read.csv("output/top.5.species.csv")
View(top.5)
str(top.5)
inst <- subset(top.5, GS.Type=="Institutional" )
inst
park <- subset(top.5, GS.Type=="Parc" )
park
res <- subset(top.5, GS.Type=="Residential" )
all <- subset(top.5, GS.Type=="Alleyways" )
Street <- subset(top.5, GS.Type=="Street" )

#institutional

res.plot <- ggplot(res, aes(Species.Code, Percent.Abundance))+
  geom_boxplot()

inst.plot <- ggplot(inst, aes(Species.Code, Percent.Abundance))+
  geom_boxplot()

all.plot <- ggplot(all, aes(Species.Code, Percent.Abundance))+
  geom_boxplot()

park.plot <- ggplot(park, aes(Species.Code, Percent.Abundance))+
  geom_boxplot()

street.plot <-ggplot(Street, aes(Species.Code, Percent.Abundance))+
  geom_boxplot()

grid.arrange(all.plot, park.plot, street.plot, ncol=3)

tot.spe <- read.csv("output/total.sp.table.csv")
tot.spe
rownames(tot.spe) <- tot.spe$X
tot.spe <- tot.spe[,-1]
tot.spe <- t(tot.spe)
tot.spe


library(data.table)
long <- melt(setDT)
str(tot.spe)
ggplot(tot.spe)
