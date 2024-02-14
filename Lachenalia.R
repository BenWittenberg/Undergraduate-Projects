#install.packages(c("ape", "geiger", "motmot", "phylolm"))

library(ape)
library(geiger)
library(motmot)
library(phylolm)

setwd("C:/Users/bensc/Downloads/Ben UCT/BIO3019S Quantitative/Tony practical")

### Lachenalia data ###

# Read a tree from a newick format file
tree <- read.tree("Lachtree.tre")
tree <- ladderize(tree, right = TRUE)
par(mfrow = c(1,1))
windows()
plot(tree, cex = 0.4)

# Read trait data from a csv file and reorder rows to match taxon sequence in tree
traits <- read.csv("Lachtraits.csv", row.names = 1)
traits <- traits[tree$tip.label,] #This makes the species names a row label and matches this to the order of tree tips 

# Fit BM model to Height 
Height <- traits$Height
names(Height) <- rownames(traits)
fitContinuous(tree, Height, model = "BM")
#This uses ML to estimate the diffusion parameter and the z0 (ancestral condition)
#The likelihood is logged because the untrasnformed likelihood is so tiny it is impractical to work with 

# Lambda-transform branch lengths and fit BM model
tree2 <- transformPhylo(tree, model = "lambda", lambda = 1)
fitContinuous(tree2, Height, model = "BM")
#Because lambda is one it is untransformed so it shoud be the same as tree1
#And it is 

# Lambda-transform branch lengths and fit BM model
tree2 <- transformPhylo(tree, model = "lambda", lambda = 0)
fitContinuous(tree2, Height, model = "BM")
#Here we've added a transformation (we've squashed the internal branches to nothing) 
#So we'll end with a star tree (no internal branches)
#And it has a higher AIC therefore not a better fit 
#This shows that a model that accounts for the internal branches is statistically more consistent with the data (i.e. a better model) 
#Therefore there is a phylogenetic signal


# Fit BM-lambda model to Height; lambda estimated 
fitContinuous(tree, Height, model = "lambda")
#Here we're getting to lambda via maximum likelihood estimate (most consistent with the terminal state)
#And the AIC is much better (1370) so it is the best descriptor so obviously there is a phylogenetic signal, and it isn't 1. So this MLE estimate is significantly better than any other models 

# Kappa-transform branch lengths and fit BM model
tree3 <- transformPhylo(tree, model = "kappa", kappa = 1)
fitContinuous(tree3, Height, model = "BM")
#This is an AIC of 1387, so identical to the original tree as we expect

Flower.number <- traits$Flower.number
names(Flower.number) <- rownames(traits)
tree_flower <- transformPhylo(tree, model = "kappa", kappa = 0)
fitContinuous(tree_flower, Flower.number, model = "kappa")

# Kappa-transform branch lengths and fit BM model
tree3 <- transformPhylo(tree, model = "kappa", kappa = 0)
fitContinuous(tree3, Height, model = "BM")
#Actually has the same AIC as the untransformed model 

# Fit BM-kappa model to Height; kappa estimated 
fitContinuous(tree, Height, model = "kappa")
#Using MLE to get to kappa 
#And it has a marginally lower AIC therefore significantly better 

# Fit OU model to Height 
fitContinuous(tree, Height, model = "OU")  #Note that this implementation does not allow the optimal value to be set or estimated
#Has one of the lowest AIC (1375) therefore some constraint is clearly happening in the data

# Estimate and plot ordinary least squares regression: Height vs Flower.number
par(mfrow = c(1,1))
windows()
plot(traits$Height, traits$Flower.number)
mod <- lm(Flower.number ~ Height, data = traits)
summary(mod)
abline(mod)

# Estimate and plot phylogenetic least squares regression: Height vs Pollination.mode"
pmod <- phylolm(Flower.number ~ Height, tree, model="BM", data = traits) 
summary(pmod)
windows()
plot(pmod)
abline(pmod, lty = "dashed")
#Here we're downweighting the relatedness to introduce independence in the data

# Fit Mk2 model to Pollination.mode (1 = insect pollinated; 1 = bird pollinated)
Poll.mode <- traits$Poll.mode
names(Poll.mode) <- rownames(traits)
fitDiscrete(tree, Poll.mode, model = "ARD")
#ARD is "MK2" so there are different rates for different transitions  
#AIC = 73
1            2
1 -0.006404281  0.006404281
2  0.209994134 -0.209994134

#So the rate from insect to bird (1 - 2; 0.006) is slower than the rate from bird to insect (2 -1; 0.2)

# Fit Mk1 model to Pollination.mode
fitDiscrete(tree, Poll.mode, model = "ER")
#ER stands for Equal rates (so reverse transitions have the same rate)
#AIC = 75
#Therefore ARD is better so there are different rates for each transition

# Reconstruct Pollination.mode on tree
windows()
mk2.anc <- ace(Poll.mode, tree, type = "discrete", model = "ARD")
plot(tree, cex = 0.5, label.offset = 2)
tiplabels(Poll.mode[1:length(tree$tip.label)], cex = 0.5, offset = 1)
nodelabels(pie = mk2.anc$lik.anc, cex=0.5)
#That appears to have worked

# Estimate and plot ordinary least squares regression: Perianth.length vs Pollination.mode
windows()
boxplot(traits$Perianth.length ~ traits$Poll.mode)
mod <- lm(Perianth.length ~ Poll.mode, data = traits)
summary(mod)
#This linear model is testing the relationship of perianth length to pollination mode 
#There is a significant result so perianth length differs between bird and insect pollination 

# Estimate and plot phylogenetic least squares regression: Perianth.length vs Pollination.mode
pmod <- phylolm(Perianth.length ~ Poll.mode, tree, model="BM", data = traits) 
summary(pmod)
#Here we're testing the same relationship (perianth length as a functikon of pollination mode) but within the framework of the phylogeny 
#This gives us a non-significant p-value which probably means that given the phylogeny there's a much greater chance the difference is due to chance alone. 

# Fit a simple pure-birth (Yule) model to the phylogenetic tree
yule(tree)

# Fit a simple birth-death model to a phylogenetic tree
birthdeath(tree)

# Fit piecewise birth-death models to a phylogenetic tree
med.result <- medusa(tree, criterion = "aicc", model = "mixed")
med.result
plot(med.result)

#Onthou AIC difference > 10 is strong, AIC diff < 2 is weak