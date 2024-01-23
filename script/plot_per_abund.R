library(ggplot2)
library(ggpubr)
meta <- read.csv("output/Dawson.Trees.With.Origin.csv")
str(meta)

df <- dplyr::select(meta, Species.Code, Site.Type)

species.unique <-lapply(split(df, df$Site.Type), unique)
length(species.unique$A) #2
length(species.unique$I) #2
View(species.unique)
####Public vs. Private Land 
#First, use metadata to compare public vs. private green space types
#use metadata to create a table of abundances according to land classification

meta.sp.table<- table(meta$Species.Code, meta$Site.Type)
meta.sp.table <- as.data.frame(meta.sp.table)
sum(meta.sp.table$Freq)
meta.sp.table
meta.table <- meta.sp.table
meta.table$per.ab <- (meta.sp.table$Freq/2451)*100
meta.table <- meta.table[order(meta.table$Freq, decreasing=TRUE),]
top.10 <- meta.table %>% slice(1:10)
sum(top.10$per.ab)
top.15 <- meta.table %>% slice(1:15)
sum(top.15$per.ab)
sum(top.5.res$per.abund)
sum(top.5)
str(top.5)
#create dataframe with species abundances based on species code and land-use classification (public/private)
meta.sp.table<- as.data.frame(meta.sp.table)
meta.sp.table
sum(meta.sp.table$Freq)
all.trees <- meta.sp.table
all.trees$per.ab <- (all.private.ab$Freq/2876)*100
class.table <- table(meta$Species.Code, meta$Classification)
class.table <- as.data.frame(class.table)
class.table

#Private
#Create private land classification only file by subsetting only private
all.private.ab<- subset(class.table, Var2=="Private")
all.private.ab
#remove zeros since these species are not present 
all.private.ab<-all.private.ab[apply(all.private.ab!=0, 1, all),]
all.private.ab<-all.private.ab[order(all.private.ab$Freq, decreasing = TRUE),]
#calculate sum of trees to calculate percent abundance
sum(all.private.ab$Freq)

#Calculate the percent abundance of each species based on total private trees
#create a new column with percent abundance value
all.private.ab$per.abund<- (all.private.ab$Freq/1487)*100
all.private.ab
#subet only the top five most abundant species 
top.5.private<- all.private.ab %>% slice(1:5)
top.5.private

#institutions
insti.ab <- subset(meta.sp.table, Var2=="I")
insti.ab<-insti.ab[apply(insti.ab!=0, 1, all),]
insti.ab <- insti.ab[order(insti.ab$Freq, decreasing=TRUE),]
sum(insti.ab$Freq)
insti.ab$per.abund <- (insti.ab$Freq/513)*100
top.5.inst <- insti.ab %>% slice(1:5)
top.5.inst

#residential 
res.ab <- subset(meta.sp.table, Var2=="Y")
res.ab<-res.ab[apply(res.ab!=0, 1, all),]
res.ab <- res.ab[order(res.ab$Freq, decreasing=TRUE),]
sum(res.ab$Freq)
res.ab$per.abund <- (res.ab$Freq/971)*100
top.5.res <- res.ab %>% slice(1:5)
top.5.res

#Public
#Create public file by subsetting only public land classification
all.public.ab<- subset(class.table, Var2=="Public")
#removing zeros so that trees that are not present are not included in calculation 
all.public.ab<- all.public.ab[apply(all.public.ab!=0, 1, all),]
all.public.ab<- all.public.ab[order(all.public.ab$Freq, decreasing = TRUE),]
#finding total number of trees for percent abundance calculation
sum(all.public.ab$Freq)

#calculate percent abundance of each species and create a new column 
all.public.ab$per.abund<- (all.public.ab$Freq/964)*100
all.public.ab
#subset only the top five most abundant species
top.5.public<- all.public.ab %>% slice(1:5)
top.5.public

#streets
street.ab <- subset(meta.sp.table, Var2=="S")
street.ab<-street.ab[apply(street.ab!=0, 1, all),]
street.ab <- street.ab[order(street.ab$Freq, decreasing=TRUE),]
sum(street.ab$Freq)
street.ab$per.abund <- (street.ab$Freq/764)*100
top.5.street <- street.ab %>% slice(1:5)
top.5.street

#alleyways
alley.ab <- subset(meta.sp.table, Var2=="A")
alley.ab<-alley.ab[apply(alley.ab!=0, 1, all),]
alley.ab <- alley.ab[order(alley.ab$Freq, decreasing=TRUE),]
sum(alley.ab$Freq)
alley.ab$per.abund <- (alley.ab$Freq/77)*100
top.5.alley <- alley.ab %>% slice(1:5)
top.5.alley

#parcs
parc.ab <- subset(meta.sp.table, Var2=="P")
parc.ab<-parc.ab[apply(parc.ab!=0, 1, all),]
parc.ab <- parc.ab[order(parc.ab$Freq, decreasing=TRUE),]
sum(parc.ab$Freq)
parc.ab$per.abund <- (parc.ab$Freq/126)*100
top.5.parc <- parc.ab %>% slice(1:5)
top.5.parc

####Plotting Figure
##plotting 5 Most Abundant Species in PUBLIC 
##create colour palette for figure and abundance labels
top.5.public
colour.table.publicab<- tibble(
  GS.Type= c("ACSA", "GLTR", "GYDI", "TICO", "ULMO"),
  Colour= c("lightpink","seagreen", "seagreen", "skyblue", "skyblue"))
abundance.labs<- labs(x= "Species Code", y= "Percent Abundance")

Public5<- ggplot(data = top.5.public,
                 mapping = aes(x= reorder(Var1, -per.abund), y= per.abund, fill= Var1))
Public5.final<- Public5 + geom_col(alpha= 0.7, width= 0.7) + theme_classic() +
  abundance.labs + scale_fill_manual(values = colour.table.publicab$Colour) +
  theme(legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
Public5.final

##plotting 5 Most Abundant Species in PRIVATE
#creating a table of colour 
top.5.private
sum(top.5.private$per.abund)
sum(top.5.alley$per.abund)
sum(top.5.public$per.abund)

colour.table.privateab<- tibble(
  GS.Type= c("ACNE", "ACPL", "FRPE", "GLTR", "THOC"),
  Colour= c("seagreen", "skyblue","lightpink", "seagreen", "lightpink"))

Private5<- ggplot(data= top.5.private,
                  mapping = aes(x= reorder(Var1, -per.abund), y= per.abund, fill= Var1))
Private5.final<-Private5 + geom_col(alpha= 0.7, width = 0.7) + theme_classic()+ 
  abundance.labs + scale_fill_manual(values= colour.table.privateab$Colour) + 
  theme(axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
Private5.final

##Arrange public and private plots into one figures
Public.Private5<-ggarrange(Public5.final, Private5.final,
                           labels = c("A", "B"), font.label = list(size= 18), hjust = -1, vjust= 1.2)
Public.Private5
ggsave("output/public.private.top.5.pdf")

## Private plots
top.5.inst

colour.table.inst.ab<- tibble(
  GS.Type= c("ACPL", "FRPE", "PNNI", "THOC", "ULPU"),
  Colour= c("skyblue", "lightpink","skyblue", "lightpink", "skyblue"))

Inst5<- ggplot(data= top.5.inst,
                  mapping = aes(x= reorder(Var1, -per.abund), y= per.abund, fill= Var1))
Inst5.final<-Inst5 + geom_col(alpha= 0.7, width = 0.7)+ theme_classic()+ 
  abundance.labs + scale_fill_manual(values= colour.table.inst.ab$Colour) + 
  theme(legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
Inst5.final


top.5.res

colour.table.res.ab<- tibble(
  GS.Type= c("ACNE", "ACPL",  "GLTR", "SYVU", "THOC"),
  Colour= c( "seagreen","skyblue", "seagreen", "skyblue", "lightpink"))

Res5<- ggplot(data= top.5.res,
               mapping = aes(x= reorder(Var1, -per.abund), y= per.abund, fill= Var1))
Res5.final<-Res5 + geom_col(alpha= 0.7, width = 0.7)+ theme_classic()+ 
  abundance.labs + scale_fill_manual(values= colour.table.res.ab$Colour) + 
  theme(legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
Res5.final

Private.GS <-ggarrange(Res5.final, Inst5.final,
                           labels = c("Private Yards", "Institutions"), font.label = list(size= 18), hjust = -1, vjust= 1.5)
Private.GS

###public land 

top.5.alley

colour.table.alley.ab<- tibble(
  GS.Type= c("ACNE", "PRVI", "THOC", "ULAM", "ULPU"),
  Colour= c("seagreen", "lightpink","lightpink", "lightpink", "skyblue"))

Alley5<- ggplot(data= top.5.alley,
               mapping = aes(x= reorder(Var1, -per.abund), y= per.abund, fill= Var1))
Alley5.final<-Alley5 + geom_col(alpha= 0.7, width = 0.7)+ theme_classic()+ 
  abundance.labs + scale_fill_manual(values= colour.table.alley.ab$Colour) + 
  theme(legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
Alley5.final


top.5.street

colour.table.street.ab<- tibble(
  GS.Type= c("ACPL", "ACSA",  "GLTR", "GYDI","ULMO"),
  Colour= c("skyblue", "lightpink","seagreen", "seagreen", "skyblue"))

Street5<- ggplot(data= top.5.street,
              mapping = aes(x= reorder(Var1, -per.abund), y= per.abund, fill= Var1))
Street5.final<-Street5 + geom_col(alpha= 0.7, width = 0.7)+ theme_classic()+ 
  abundance.labs + scale_fill_manual(values= colour.table.street.ab$Colour) + 
  theme(legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
Street5.final

top.5.parc

colour.table.parc.ab<- tibble(
  GS.Type= c("ACSA", "GLTR", "MATH", "TIAM","TICO"),
  Colour= c("lightpink","seagreen", "skyblue", "lightpink", "skyblue"))

Parc5<- ggplot(data= top.5.parc,
                 mapping = aes(x= reorder(Var1, -per.abund), y= per.abund, fill= Var1))
Parc5.final<-Parc5 + geom_col(alpha= 0.7, width = 0.7)+ theme_classic()+ 
  abundance.labs + scale_fill_manual(values= colour.table.parc.ab$Colour) + 
  theme(legend.position = "none", axis.title = element_text(face= "bold", size= 20), axis.text.x = element_text(size= 18), axis.text.y = element_text(size= 18))
Parc5.final

Public.GS <-ggarrange(Street5.final, Alley5.final, Parc5.final, ncol=3,
                       labels = c("A", "B", "C"), font.label = list(size= 18), hjust = -1, vjust= 1.2)
Public.GS

