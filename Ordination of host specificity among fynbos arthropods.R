#Charlene Prac
#Good luck amigo 

#clear environment
rm(list = ls())

setwd("C:/Users/bensc/Downloads/Ben UCT/BIO3018/Charlene Prac")
rm(list = ls())

inverts <- read.csv2("charlene.prac.csv")
View(inverts)

dehoop <- read.csv2("dehoop.csv")

Data <- inverts[-20,]
dehoop <- dehoop[-20,]
View(Data)
#Now I make an NMDS 

library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)

Spp <- inverts[-20,c(4:53)]
names(Spp)
View(Spp)

SppMDS <- metaMDS(Spp, distance = "bray", k=2)


windows()
plot(SppMDS, type = "t")

colours <- c("Aulax Male (M)" = "#f35b04",
             "Aulax Female (F)" = "#f7b801",
             "Erica (E)" = "#3d348b")

#Extract the scores from the MDS plot 
scrs <- as.data.frame(scores(SppMDS)$sites)

points <- SppMDS$points

windows()
plot(points,col=c("grey","deeppink", "green")[as.factor(inverts$plant)], pch=c(17,1)[as.factor(inverts$plant)])

leg.txt<-c("Aulax Male","Aulax Female", "Erica")
legend(-2.4, 0.9, leg.txt, pch =c(17,1), col =c("grey","deeppink", "green"), bty = "o")
text(points, labels = inverts[,2], cex=.7, pos=4)

#well that didn't really work 

#Should I use scores? 
#Add columns to the data 

scores <- as.data.frame(scores(SppMDS)$sites)
points <- as.data.frame(points(SppMDS))
points$plant <- rownames(points)
scores <- rownames(scores)
scores$plant <- Data$plant
scores$Species <- Data$Species
head(points)


ggplot
xx = ggplot(scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = plant, shape = Species))+ 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA,       size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "sample", y = "NMDS2", shape = "Type")  + 
  scale_colour_manual(values = c("#009E73", "#E69F00","#00008b"))

ggplot()
windows()
 plot(xx)
 
#Now maybe I should just do between Aulax and Erica? 
 
ggplot
 yy = ggplot(scores, aes(x = NMDS1, y = NMDS2)) + 
   geom_point(size = 4, aes(colour = Species))+ 
   theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
         axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
         legend.text = element_text(size = 12, face ="bold", colour ="black"), 
         legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
         axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
         legend.title = element_text(size = 14, colour = "black", face = "bold"), 
         panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA,       size = 1.2),
         legend.key=element_blank()) + 
   labs(x = "NMDS1", colour = "sample", y = "NMDS2", shape = "Type")  + 
   scale_colour_manual(values = c("#009E73", "#E69F00"))
 
 ggplot()
 windows()
 plot(yy)
#ANOSIM test between all categories
anosim(Spp,Data$plant , permutations = 9999, distance = "bray", strata = NULL)
#There does appear to be a difference between Aulax Males, Females and Ericas 
Dissimilarity: bray 

ANOSIM statistic R: 0.307 
Significance: 2e-04 

Permutation: free
Number of permutations: 9999

#ANOSIM test between the two species 
anosim(Spp,Data$Species , permutations = 9999, distance = "bray", strata = NULL)
#Again, there does appear to be some difference 
Dissimilarity: bray 

ANOSIM statistic R: 0.1957 
Significance: 0.0246 

Permutation: free
Number of permutations: 9999

#So, given the host specialization we know occurs in fynbos plant/insect interactions, we can conclude that the inverts using the different species are different 

#Alternately, the different heights of the vegetation may indicate that inverts hide in different heights of vegetation 

#ANOSIM BY SITE
anosim(Spp,dehoop$Site , permutations = 9999, distance = "bray", strata = NULL)

#No difference in community composition between sites

#Now for the nitty gritty data 
t.test(Data[Data$Species == "Erica",54],Data[Data$Species == "Aulax", 54])

#Output 
t = 1.867, df = 7.0797, p-value = 0.1037
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -6.130043 52.630043
sample estimates:
  mean of x mean of y 
30.25      7.00 

#So there's no difference in total counts between Ericas and Aulax 

#But there was a difference in community composition 
#No difference between thrip counts between Erica and Aulax 

t.test(Data[Data$Species == "Erica", 4], Data[Data$Species == "Aulax", 4])

anova <- aov(Thr.1 ~ Species, data = Data)
summary(anova)

View(Spider)
Spider <- Data[,17:30]
Spider[] <- lapply(Spider, as.numeric)
anosim(Spider,Data$Species, permutations = 9999, distance = "bray", strata = NULL)

Aulax <- read.csv2("Aulax.csv")
Aulax <- Aulax[-14,]
Aulax.Mat <- Aulax[,4:53]
Aulax.Mat[] <- lapply(Aulax.Mat, as.numeric)
anosim(Aulax.Mat,Aulax$plant, permutations = 9999, distance = "bray", strata = NULL)

#Ooooo so there is a difference between Male/Female Aulax 
ANOSIM statistic R: 0.2092 
Significance: 0.03
#Let's see what species are responsible 
Simp2 <- simper(Aulax.Mat, Aulax$plant, permutations = 9999)
summary(Simp2)
#Species that contribute to the difference 
#Mit 1 (12%), p = 0.01, higher in Males
#Spider 2 (5%), p = 0.03, higher in Males
anova <- aov(total ~ plant, data = Aulax)
summary(anova)


library(vegan)
diversity(Aulax.Mat, Aulax$plant, index = "shannon")

anosim(Aulax.Mat, Aulax$plant, distance = "bray", permutations = 999)

?diversity
?simper
simp <- simper(Spp, Data$Species, permutations = 9999)
summary(simp)
#So let's have a look at this output: 
#Thrips account for 35% of difference 
#Weevil 1: 5%
#Spider 13: 3%
#Cat 3: 1.4% 
#Spider 14: 1.4%
#Sti1: 1.3%
#Hemipteran 4: 1%
#Mite 4: 1%
#Spider 6
#Hemipteran 2
##Spiders 10,11,7
#Colembolla 5
#Spider 12
#Weevil 3
#Wasp 1
#Cat2

#Spider contribution: 

#Mite contribution


#Boxplot
windows()
par(mfrow = c(2,1))
box <- ggplot(Data, aes(x = plant, y = total, fill = "blue")) + 
  geom_boxplot()


plot(box)

box2 <- ggplot(Data, aes(x = Species, y = Richness, fill = "blue")) + 
  geom_boxplot()

windows()
plot(box2)

anova1 <- aov(total ~ plant, data = Data)
summary(anova1)
TukeyHSD(anova1)
anova2 <- aov(total ~ plant, data = Data)
summary(anova2)
TukeyHSD(anova2)
anova3 <- aov(total~Site, data = dehoop)
summary(anova3)

#Now for species counts by species
Richness <- specnumber(Spp, Data$Species)
Richness
barplot(Richness)

Rich2 <- specnumber(Spp, Data$plant)
Rich2

Rich3 <- specnumber(Spp, dehoop$Site)
Rich3


#Let's have Beta diversity (Sorenson's)
beta <- designdist(Spp, method = "(A+B-2*J)/(A+B)", terms = "binary")
summary(beta)

#Now I need to present it nicely 
species_beta <- as.data.frame(as.matrix(beta)) # make it a rectangular table
species_beta[upper.tri(species_beta, diag = T)] <- NA # set the top-right to "NA"
options(knitr.kable.NA = '')

#Present as table 
kable(species_beta, digits = 3)


anova <- aov(total ~ plant, data = Data)
summary(anova)

Tuk <- TukeyHSD(anova)
Tuk
sum(Data$total)
