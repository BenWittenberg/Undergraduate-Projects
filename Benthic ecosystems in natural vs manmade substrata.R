#Tidal Pool Project
#Let's see how this goes 
rm(list = ls())
setwd("C:/Users/bensc/Downloads/Ben UCT/BIO3002F/Tidal Pool Project")
data <- read.csv2("Marine_data.csv")
View(data)

#I'm having problems with numeric variables 
library(dplyr)



as.factor(data)

#Well that didn't work
#Let's try the following 
Matrix <- data[,4:79]
#Now I need to coerce them to numeric without killing my categorical variables, so I made a matrix of species presence/absence + relative abundance and applied the as.numeric function to all columns within 
Matrix[] <- lapply(Matrix, as.numeric)
View(Matrix)
Matrix <- log(Matrix)
MDS <- metaMDS(Matrix, distance = "bray", k=3,autotransform = FALSE, trymax = 999, e3xpand = TRUE, wascores = TRUE)
#Whooo something happened 
scores <- as.data.frame(scores(MDS)$sites)
scores$treatment <- data$treatment
scores$site <- data$site

#Now all we have to do is make the actual NMDS
ggplot
xx = ggplot(scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = treatment))+ 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA,       size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "sample", y = "NMDS2", shape = "Type")  + 
  scale_colour_manual(values = c("#FF3030","#104E8B"))

windows()
plot(xx)

#Now I should try an ANOSIM
anosim(Matrix,data$treatment , permutations = 9999, distance = "bray", strata = NULL)
#And yet, somehow, there is a significant result 
ANOSIM statistic R: 0.08939 
Significance: 0.0206 

Permutation: free
Number of permutations: 9999
?anosim
?vegdist
#perhaps I should ask Natasha about this 


#Now I shall do an NMDS separating sites 
ggplot
yy = ggplot(scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = site))+ 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA,       size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "sample", y = "NMDS2", shape = "Type")  + 
  scale_colour_manual(values = c("#FF3030","#104E8B","#FF7F00","#00CD00"))

windows()
plot(yy)

#And now we do an ANOSIM on this data
anosim(Matrix,data$site , permutations = 9999, distance = "bray", strata = NULL)
?capscale
??capscale
#There appears to be a defined difference in communities between sites 
ANOSIM statistic R: 0.2658 
Significance: 1e-04 

Permutation: free
Number of permutations: 9999

#Now perhaps I should do a SIMPER analysis to see what species are most responsible for the observed differences 

?simper
#First do for the treatments
simp <- simper(Matrix, data$treatment, permutations = 999)
#So that gave me some output I'm not entirely sure how to interpret 
cumulative contributions of most influential species:
  
  $manmade_natural
Ulva.species 
0.1960025 
Phymatolithon.foveatum 
0.2819687 
Bifurcariopsis.capensis 
0.3646416 
Ecklonia.maxima 
0.4310326 
Parechinus.angulosus 
0.4904494 
Jania.adhaerens 
0.5402846 
natural 
0.5788169 
Hildenbrandia.lecanelleri 
0.6162283 
Spongites.yendoi 
0.6493377 
Chaetomorpha.linum 
0.6815117 
phymatolithon.ferox 
0.7134775 

#There are no test statistics attached to these so I have no idea if these contributions are significant 
summary(simp)
#BWAHAHAHAH I have p-values 
#I will only show those with a significant or close-to-significant p-val
Chaetomorpha.linum                               0.039
phymatolithon.ferox                              0.022
Encrusting                                       0.046
Leathesia.marina                                 0.070
#So not all that many species are actually important in distinguishing between man-made and natural areas
#Curious 


#Data exploration 
names(Matrix)
summary(Matrix)

#First I'm just going to try and see what the species richness looks like 
Richness <- specnumber(Matrix, data$treatment)
Richness
#manmade natural 
#56      43

#Need to check Shannon-Wiener and all the other diversity metrics 
diversity(Matrix, index = "shannon", data$treatment)
#manmade = 2.571472
#natural = 2.547111
#Get diversity indices for each image and then run an anova to test the differences in diversity between each site/treatment 
??capscale
cap <- capscale(Matrix ~ data$treatment, data, distance = "bray", sqrt.dist = FALSE,
         comm = NULL, add = FALSE,  dfun = vegdist, metaMDSdist = FALSE,
         na.action = na.fail, subset = NULL)

windows()
plot(cap)

data.dis <- vegdist(wisconsin(Matrix))
MDS$species <- wascores(MSD$points,data,expand = TRUE)

#Now we have added Species Richness to the data frame, let's compare the two treatments: 

anova1 <- aov(Spp.richness ~ treatment, data = data)
summary(anova1)
#Output 
Df Sum Sq Mean Sq F value Pr(>F)  
treatment    1  46.04   46.04   5.409 0.0256 *
  Residuals   37 314.94    8.51                 
#So there's clearly a difference in means 

anova2 <- aov(Spp.richness ~ site*treatment, data = data)
summary(anova2)
#Output 
Df Sum Sq Mean Sq F value   Pr(>F)    
Interaction  7  200.2  28.603   5.516 0.000345 ***
  Residuals   31  160.8   5.185
#So once again we do have a significant result 
#Let's try a t.test on this bad boi


#Post-Hoc analyses 
Tuk1 <- TukeyHSD(anova1)
#So yeah there is a difference between man-made and natural (p = 0.0256255)

Tuk2 <- TukeyHSD(anova2)
#Here I shall copy those comparisons that were significant 
                                       upr       p adj
KOMMETJIE-manmade-FBYC-manmade      10.87527598 0.0034459
KOMMETJIE-manmade-FBYC-natural      11.47527598 0.0011067
KOMMETJIE-manmade-GLENCAIRN-manmade 12.00887902 0.0014829
KOMMETJIE-manmade-GLENCAIRN-natural  9.87527598 0.0208826
KOMMETJIE-natural-KOMMETJIE-manmade -1.92472402 0.0016211
MILLERS-natural-KOMMETJIE-manmade   -2.72472402 0.0003477
#Uhhh so Kommetjie is different to everything

anova3 <- aov(Spp.richness ~ site, data = data)
summary(anova3)
TukeyHSD(anova3)
summary(faunawide)


citation("vegan")
sum(as.numeric(data$Ulva.species)) = 0.9100929
sum(as.numeric(data$Ecklonia.maxima)) = 0.2218587
sum(as.numeric(data$Phymatolithon.foveatum)) = 0.329295
sum(as.numeric(data$phymatolithon.ferox)) = 0.1237636
sum(as.numeric(data$Parechinus.angulosus)) = 0.1769099
sum(as.numeric(data$Encrusting)) = 0.07310987
sum(as.numeric(data$Chaetomorpha.linum)) = 0.1272392
sum(as.numeric(data$Bifurcariopsis.capensis)) = 0.3765705
sum(as.numeric(data$Cymbula.granatina)) = 0.01692097


#FBYC MANMADE
mean(as.numeric(data[1:5,81]))
#Mean = 4.6
#FBYC natural
mean(as.numeric(data[6:10,81]))
#Mean = 4
#Glencairn - manmade
mean(as.numeric(data[11:14,81]))
#Mean = 3.75
#Glencairn - natural 
mean(as.numeric(data[15:19,81]))
#Mean = 5.6
#Kommetjie - manmade
mean(as.numeric(data[20:24,81]))
sd(as.numeric(data[20:24,81]))
#Mean = 10.8
#Kommetjie - natural
mean(as.numeric(data[25:29,81]))
#Mean = 4.2
#Millers - manmade
mean(as.numeric(data[30:34,81]))
#Mean = 6.2
#Millers - natural 
mean(as.numeric(data[35:39,81]))
sd(as.numeric(data[35:39,81]))
#Mean = 3.4


boxplot(as.numeric(Spp.richness) ~ Interaction, data = data, ylab = "Species Richness", xlab = "Site and Treatment", las = 2, cex = )
?boxplot

#Creating a boxplot in ggplot
ggplot(data, aes(x = treatment, y = Spp.richness)) + 
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "white")
?geom_boxplot
?ggplot



#Let's bloody well get this finished
faunawide<-read.csv2("Tidal_data.csv")
names(faunawide)

Spp <- faunawide[,4:79] # create a new dataframe with just the species information
names(Spp)
Spp[] <- lapply(Spp, as.numeric)
Spp<-as.numeric(Spp)
summary(Spp)

library(vegan)
cap.Spp<-capscale(Spp~treatment, faunawide, dist = "bray", sqrt.dist= TRUE)
plot(cap.Spp)
str(cap.Spp)
windows()
plot(cap.Spp, type="n")
points(cap.Spp, pch=16, col=c("blue","orange")[as.factor(faunawide$treatment)], cex=1.2)
text(cap.Spp,labels = faunawide [,1], cex=.7, pos=4,)
leg.txt<-c("Natural","Manmade")
legend(1.5, 1.5, leg.txt, pch=c(16,16), col=c("orange","blue"), cex=.7)
?capscale
citation()
faunawide


#Now I'm trying to make boxplots comparing ghe different sites 
"Species Richness" <- as.numeric(data$Spp.richness)
box <- ggplot(data, aes(x = treatment, y = `Species Richness`, fill = "blue")) + 
  geom_boxplot()
windows()
box
names(data)
spiky1 <- data[data$treatment == "natural",52]
spiky2 <- data[data$treatment == "manmade",52]

anova.rand <- aov(data$Chaetomorpha.linum~data$treatment)
summary(anova.rand)
anova <- aov(data$Parechinus.angulosus ~ data$treatment)
summary(anova)

lm(data$Parechinus.angulosus~data$Phymatolithon.foveatum)
sum(as.numeric(data$natural))
citation("vegan")



