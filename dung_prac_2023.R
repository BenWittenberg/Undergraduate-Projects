library(package)
library(plyr)
library(agricolae)
library(dplyr)
library(tidyr)
library(tidyverse)
install.packages("agricolae")
citation("vegan")

setwd("C:/Users/bensc/Downloads/Ben UCT/BIO3018/Dawood Prac")

##### Dung data #####
#Read in data - you just need to change the filepath
dung <- read_csv("dung_2023_edited.csv")

# make na's 0
dung[is.na(dung)] = 0
head(dung)
View(dung)
mean(dung[dung$Veg.type == "renosterveld",dung$Bontebok])
sum(dung[9:12,9:10])/4

mean(c(11,17,11,6))
#reorganise data frame
dung2 <- gather(dung, Species, Count, 3:10)
?gather
head(dung2)
View(dung2)

#Diversity analyses - how diverse are the communities? How evenly distributed i.e. is there lots of dung from a few animals vs lots of dung from lots of animals.

#Species richness (S)
# number of species for each veg type
no_0s <- dung2 %>%
  group_by(Veg.type)%>%
  filter(Count > 0)


dung_table <- table(no_0s$Veg.type, no_0s$Species)


#Species richness (S)
S_dung <- specnumber(dung_table)
S_dung


#Shannon's diversity index (H)
#the index increases as both the richness and the evenness of the community increase
# we'll use the 'diversity' function
H_dung <- diversity(dung_table)
H_dung
#OUTPUT
Fynbos    Grassland Renosterveld 
1.273028     1.579233     1.285293

#Simpson's diversity index (simp)
#ranges between 0 and 1. Smaller values indicate high diversity with larger values indicating low diversity. Therefore use 1-D, where larger number now indicate higher diversity.
simp_dung <- diversity(dung_table, "simpson")
simp_dung
###OUTPUT 
Fynbos    Grassland Renosterveld 
0.6913580    0.7889273    0.7100592
#Because Smaller values indicate Higher diversity in Simpson's Index, I transformed it by 1-simp_dung so that larger values would signal greater diversity which is more intuitive 
simp_dung2 <- 1 -simp_dung
simp_dung2
####OUTPUT 
Fynbos    Grassland Renosterveld 
0.3086420    0.2110727    0.2899408 
#Pielou's evenness (J)
#ranges between 0 and 1. 0, where 0 signifying no evenness and 1 signifying complete evenness
J_dung <- H_dung/log(S_dung)
J_dung
###OUTPUT
Fynbos    Grassland Renosterveld 
0.9182958    0.9812327    0.9271429
## decide which diversity index you will use and motivate why, in your methods
## this data should be presented in a table, in your results section. 

############################################
#Community analysis - how similar are the Fynbos, Grassland, and Renosterveld communities?

# rearrange data
# get total dung counts for each group
dung_sum <- dung2 %>%
  group_by(Veg.type, Species) %>%
  summarise(total_dung=sum(Count)) %>% 
  rename(Site = Veg.type)

dung_ord <- spread(dung_sum, Species, total_dung)


#sort your data
dung_ord<-dung_ord[order(dung_ord$Site),]


dung_ord1 <- subset(dung_ord, select = c(2:9)) 
# vegdist (which computes similarity) runs with only numeric data, so take out the first column
# Take note of the order that the sites were in: (1) fynbos, (2) grassland, (3) renosterveld

#Cluster dendrogram - shows us the grouping of the sites
dis_dung <- vegdist(dung_ord1)
# vegdist computes dissimilarity indices, useful for community ecologists.

clust_dung <- hclust(dis_dung, "average")
windows()
plot(clust_dung)
dung_ord

#You are looking at the grouping of your sites i.e. are they all random or do certain sites group together.
#Axis on the left represents similarity. So 0.1 indicates 100% similar and gets less similar as you go up the axis.
#Branching occurs where the clusters break from each other and group according to similarity.

###############################################################################
# dung count analysis
# we're dealing with count (discrete) data, not continuous, which means we can't use linear models and ANOVAs to test for differences

# have a look at your data
View(dung2)

# jitter plot
windows()
ggplot(dung2, aes(x = Species, y = Count, colour = Species)) +
  geom_jitter() +
  labs(x = "Species", y = "Total dung count") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1 , vjust = 0.3)) +
  facet_wrap(~Veg.type)

# lots of zeros in this data frame! Not uncommon for count data. 

# plot for report:
# with count data you can report a mean dung count for each site, even though this value will not be discrete
# You just can't analyse the data as if they are continuous

# plot for report: get means and SDs, to plot a bar graph with SD
# mean dung count per site

head(dung_sum)

dung_mean <- dung_sum %>%
  group_by(Site) %>%
  summarise(mean = mean(total_dung), sd = sd(total_dung))

windows()
ggplot(dung_mean, aes(x = Site, y = mean, fill = Site)) +
  geom_errorbar(aes(ymin = 0, ymax = (mean+sd)), width = 0.25, 
                position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
 # scale_y_continuous(expand = c(0,0), limits = c(0,50)) +
  labs(x = "Site", y = "Mean dung count") +
  theme_classic() +
  theme(legend.position = "none")


## use Kruskal Wallis
kw_dung <- kruskal.test(total_dung~Site, data = dung_sum)
kw_dung
View(dung_sum)
# From the output of the Kruskal-Wallis test, we see no significant difference between groups
View(dung)

barplot(dung2$Species, dung2Count)
kruskal.test(Eland ~ Veg.type, data = dung)
library(FSA)
posthoc <- dunnTest(Eland ~ Veg.type, data = dung,
                    method="bh")
kable(posthoc$res, digits = 3)
posthoc$res
?dunnTest
??dunnTest
?kruskal.test

