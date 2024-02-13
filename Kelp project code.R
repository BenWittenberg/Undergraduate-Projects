#PRIMER_Rcode
#Use the help tab in the bottom right panel to learn more about any of the functions
#1) Create a folder on your desktop called "benthos" and save "infaunawide.csv" into the folder
#2) Open Rstudio or R
#3) Clear R's history
rm(list=ls())

#4) Set your working directory 
#setwd("C:/...benthos")
setwd("C:/Users/bensc/Downloads/Ben UCT/BIO2016S/Kelp.project")
plants <- read.csv2("BIO2017S-field-trip-coverage-data.csv")
View(plants)
plants$Restio
#5) Read in the data file and assign it a na
kelp<-read.csv2("Kelp project data_September 2022 (2).csv") #for MDS analysis
View(kelp)
head(kelp)
windows()
boxplot(kelp$Counts~kelp$Kelp.section, ylab = "Organism Counts", xlab = "Kelp Section")
#6) install a few important packages
install.packages("vegan") # if this doesn't work, go to packages install in the right bottom panel and choose vegan and its dependencies there.
View(Spp)
(kelp[kelp$Kelp.section == "Stipe",])
sum(kelp$Counts)
#7)open libraries

library(stats)
library(vegan)
names(kelp)


kelp2<-kelp[kelp$Counts>0,]#remove all the stipes that had no species
row.names(kelp2)<-kelp2$Sample#change row names to the sample names
Spp <- kelp2[,c(8:114)] # create a new dataframe with just the species information
spp.br <- vegdist(Spp, method="bray")#create a similarity matrix
spp.br.average <- hclust(spp.br, method="average")
windows()
plot(spp.br.average, labels=kelp2$Kelp.section, hang=-1)
windows()
#labels are factor kelp section
plot(spp.br.average, labels=kelp2$Sample, hang=-1) #labels are samples

#Run an ANOSIM: Null Hypothesis: Communities before and after harbour development are not different
anosim(Spp,kelp2$Kelp.section , permutations = 999, distance = "bray", strata = NULL)
citation("vegan")
#OUTPUT FOR ANOSIM
Call:
  anosim(x = Spp, grouping = kelp2$Kelp.section, permutations = 999,      distance = "bray", strata = NULL) 
Dissimilarity: bray 

ANOSIM statistic R: 0.8071 
Significance: 0.001 

Permutation: free
Number of permutations: 999

#which species are responsible for dissimilarity? 
simper.spp<-simper(Spp, kelp2$Kelp.section)
str(simper.spp)
## S3 method for class 'simper'
summary(simper.spp, ordered = TRUE,
        digits = max(3,getOption("digits") - 3))
?simper
sum(kelp[kelp$Kelp.section == "Blade",115])
sum(kelp[kelp$Kelp.section == "Holdfast",115])
sum(kelp[kelp$Kelp.section == "Stipe",115])
sum(c(111,1421,4))

kelp[kelp$Kelp.section == "Holdfast", ]
#nMDS visualisation data cleaning and ordination

kelp3<-kelp[kelp$Counts>1,]#remove outlier samples for the nMDS only
row.names(kelp3)<-kelp3$Sample#change row names to the sample names
names(kelp3)
Spp2 <- kelp3[,c(8:114)]
Spp2 <- Spp2[,-c(97)]
row.names(Spp)
spp2.br <- vegdist(Spp2, method="bray")#create the similarity matrix

set.seed(19781231)#giving the algorithm a starting point
Spp2.mds <- metaMDS(spp2.br)#doing the nMDS using the similarity matrix

#Pulling out the co-ordinates of the nMDS
single2<-Spp2.mds$points
windows()
plot(single2,col=c("red","deeppink","royalblue")[as.factor(kelp3$Kelp.section)], pch=c(17,1,0)[as.factor(kelp3$Kelp.section)])

#add a legend
leg.txt<-c("Blade","Holdfast", "Stipe")
legend(-0.6, -0.2, leg.txt, pch =c(17,1,0), col =c("red","deeppink","royalblue"), cex=.7, bty = "o")



#produce cluster
x.sum <-apply(Spp,1,sum)
names(Spp)
citation("vegan")

3.152e-06/0.000003152
1421/1536
0.9251302*100
96/107
0.8971963*100
