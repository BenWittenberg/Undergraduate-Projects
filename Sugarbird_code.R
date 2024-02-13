#Quantitative Biology Project 
#Sugarbird genetic mixing as a consequence of fire perturbation 


#Try this with energy moveSIM and energySIM 
#install.packages("abmR")
library(abmR)
#install.packages("geosphere")


#install.packages("terra")
library(terra)
#install.packages("tidyverse")
library(tidyverse)


#set a working directory
setwd("C:/Users/bensc/Downloads/Ben UCT/BIO3019S Quantitative/Sugarbird Project")

age <- rast("vegage_karoo.tif")
#age2 <- rast("vegage_karoo.tif") 

age %>% as.data.frame(xy = TRUE) %>% 
  pivot_longer(cols = as.character(1980:2020), names_to = "Year") %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = value)) +
  facet_wrap(.~Year)

age %>% as.data.frame(xy = TRUE) %>% 
  pivot_longer(cols = as.character(2020), names_to = "Year") %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = value)) +
  facet_wrap(.~Year)

#ageK <- subst(age,NA, 0)
age_stack <- raster::stack(age)
age_stack2 <- raster::stack(age)

age_stack[is.na(age_stack[])] <- 40
age_stack2[is.na(age_stack2[])] <- 1

plot(age_stack$X2020)


#both energySIM and moveSIM require as.species as an input which sets the geographical location of the agent 



#days = 14 (arbitrary but perhaps informative to see where they go in the first two weeks, if no suitable spot found after this I assume mortality)
#modeled_species = class "species" which I'll set 
#env_rast = a raster of veg age. Different pixels will have different "energy" depending on age with moderate aged (say 3 years) having poor energy and older having good energy. Age < 3 years = zero 
#I think veg age (and potentially diversity) should constitute energy
#optimum_lo: a numeric lower bound (say three years)
#optimum_hi: a numeric optimal environmental value ()
#Sigma is a randomness parameter. I think I shall play around with numerous models and see which one has the best fit relative to predictive power (AIC) or perhaps I'm forgetting that this is mechanistic not empirical 


#test <- as.species(x = 18.425486, y = -33.966779) #Setting my agent to the top of Maclear's Beacon
#destination being the top of Devil's Peak 
#I coudl even run a test with a simple isolines raster 
#hey that seems to have worked 
#But before I can fully test drive this I need to import a raster file 
#I'll steal one from Jasper's site or get the fire data 


#Test drive
#In this test drive I'm a hypothetical agent sitting on top of Maclear's Beacon trying to get to Devil's Peak. I'm a high-altitude specialist so my optimum low is 800m asl with optimum high = 1800 asl
#I'll need a raster of isolines, unless I just drop this test in favour of the actual fire data 

#As expected it didn't like that I didn't have a raster file, but other things seem to be working since the raster call was at the end of the function


#Use example code
# Define species object
# pop1 <- as.species(
#   x = -98.7, y = 34.7)
# ?as.species
# Run function
# EX2 <- energySIM(
#   replicates = 100, days = 3, env_rast = ex_raster, search_radius = 400,
#   sigma = .1, dest_x = -108.6, dest_y = 26.2, mot_x = .1, mot_y = .1,
#   modeled_species = pop1,
#   optimum_lo = .6, optimum_hi = .8, init_energy = 100,
#   direction = "S", write_results = FALSE, single_rast = TRUE, mortality = TRUE)
# # View Results in Clean Format
# tidy_results(EX1, type = "results")
# tidy_results(EX1, type = "run_params")
# 
# EX1$results
#Now plot this
# windows()
# energyVIZ(EX1,title="Visualizing EnergySIM results",type="plot", aspect_ratio=5/3,
#           label=TRUE)
# #Plot the lower motivation example 
# windows()
# energyVIZ(EX2,title="Visualizing EnergySIM results",type="plot", aspect_ratio=5/3,
#           label=TRUE)
# 
# EX2$results
#So if you decrease the motivation, you decrease the range it'll move and they can still survive even if they're nowhere near the target

#More example code 
# energyVIZ(
#   EX1,
#   type = "plot",
#   title = "energySIM results",
#   aspect_ratio = 1,
#   label = NULL,
#   xlim = NULL,
#   ylim = NULL
# )


#In the absence of a raster of fire I'll continue with the energyVIZ function
#This plots a visualization of the energySIM output 




#Squash the veg age raster using an arcsine transformation to have veg quality be a non-linear function of age 
#Are we looking for the proportion of birds that survive a jump from one range to another? 
#Get Vegetation age raster 
#
# trail_data <- EX1$results
# 
# anova_dist <- aov(distance ~ final_status, data = trail_data)
# summary(anova_dist)



##########
#END OF TRIAL
#Actual sugarbird code
#Make a population
pop1 <- as.species(
  x = 22.0, y = -33.4)
#MOD1 is a cross-range model with search radius = 50km. motivation = 1
#Zero motivation is the null model (remember, this is still cross-range movement)
Mod1 <- energySIM(
  replicates = 250, days = 7, env_rast = age_stack2, search_radius = 5,
  sigma = .1, dest_x = 21.25, dest_y = -33.45, mot_x = 1, mot_y = 1,
  modeled_species = pop1,
  optimum_lo = 15, optimum_hi = 50, init_energy = 50,
  direction = "W", write_results = FALSE, single_rast = TRUE, mortality = TRUE)
# View Results in Clean Format
tidy_results(Mod1, type = "results")
tidy_results(Mod1, type = "run_params")

#Let's try do something with this 
Results1 <- Mod1$results
ggplot(Results1, aes(x=lon, y=lat)) +
  geom_point(shape=1, col = "blue", pch = 16) # Use hollow circles

t.test(as.numeric(Results1[Results1$day == "7",9]),as.numeric(Results3[Results3$day == "7",9]))

#Play around with the data 
Results1[Results1$final_status == "Alive",8]

Results1[complete.cases(Results1),1:9]
mean(as.numeric(Results1$distance))




ex_anova <- aov(Results3$distance~Results3$final_status)
summary(ex_anova)
#So there is no difference in distance traveled within models
ex_anova2 <- aov(Results1$distance ~Results1$final_status)
summary(ex_anova2)
#So there is no difference in distance traveled within models 

anova1 <- aov(distance ~ final_status, data = R1)
summary(anova1)



?energyVIZ
windows()
energyVIZ(Mod1,title="Visualizing high within-range motivation",type="plot", aspect_ratio=5/3,
          label=TRUE, xlim = c(21.0,22.5), ylim = c(-34.5,-33.2))
plot(Results1$energy)
#Mod2 is a contiguous model (within range) with search radius = 50, motivation = 0.1
#Mod2 tests the response to a small fire as well as the movement potential as a result of normal foraging (50km)
Mod2 <- energySIM(
  replicates = 100, days = 7, env_rast = age_stack2, search_radius = 5,
  sigma = .1, dest_x = 21.25, dest_y = -33.45, mot_x = 0, mot_y = 0,
  modeled_species = pop1,
  optimum_lo = 25, optimum_hi = 35, init_energy = 50,
  direction = "W", write_results = FALSE, single_rast = TRUE, mortality = TRUE)
# View Results in Clean Format
tidy_results(Mod2, type = "results")
tidy_results(Mod2, type = "run_params")


windows()
energyVIZ(Mod2,title="Visualizing high cross-range motivation",type="summary_table", aspect_ratio=5/3,
          label=TRUE, xlim = c(21.0,22.5), ylim = c(-34.5,-33.2))
#Average daily = 7.3km (upper = 9.7km)
#Most individuals either remained at 50% or rose to 100% energy 



#Do another anova 
Results2 <- Mod2$results
windows()
barplot(Results2$distance)

Success1 <- Results2[Results2$day == "7",1:9]
windows()
ggplot(Success, aes(x=lon, y=lat)) +
  geom_point(shape=1, col = "blue", pch = 16)  
ex_anova2 <- aov(Results2$distance~Results2$final_status)
summary(ex_anova2)
#So there don't appear to be any significant differences between final statuses in terms of distance, 
#That must simply be due to the uniform search radius

#How about an ANOVA comparing distance between alive individuals for cross-range and within-range movements 

t.test(Results1[Results1$final_status=="Alive",8],Results2[Results2$final_status=="Alive",8])

#Mod3 is a within-range movement with distance = 50km, motivation = 0.5 (moderate perturbation) 

Mod3 <- energySIM(
  replicates = 100, days = 7, env_rast = age_stack, search_radius = 50,
  sigma = .1, dest_x = 22.0, dest_y = -33.8, mot_x = 0, mot_y = 0,
  modeled_species = pop1,
  optimum_lo = 15, optimum_hi = 50, init_energy = 50,
  direction = "S", write_results = FALSE, single_rast = TRUE, mortality = TRUE)
tidy_results(Mod3, type = "results")
tidy_results(Mod3, type = "run_params")
#Enhanced frequency: Average = 4.54km per day, max = 5.90km, most agents lost energy (ended on 30% or 10%) 
6.99*2 - 6.61

Results3 <- Mod3$results
Success <- Results3[Results3$day=="6",1:9]

windows()
energyVIZ(Mod3,title="Visualizing moderate motivation",type="summary_table", aspect_ratio=5/3,
          label=TRUE, xlim = c(21.0,22.5), ylim = c(-34.5,-33.2))
#Within-range distance = 7.6km, 99% survival (Mot = 0.1)
#Within-range distance = 7.6km, 95% survival (Mot = 0)
#Within-range distance = 8.3km, 97% survival (Mot = 1)
#Cross-range distance = 6.59km, 96% survival (Mot = 1)
#Cross-range distance = 6.38km, 97% survival (Mot = 0.1)
#Cross-range distance = 6.48km, 95% survival (Mot = 0)
#Enhanced cross-range: dist = 4.14km (Mot = 0)
#Enhanced cross-range: dist = 4.60km (Mot = 0.1)
#Enhanced cross-range: dist = 4.72km (Mot = 0.5)
#Enhanced cross-range: dist = 4.56 (Mot = 1)
(7.37+7.6+7.6+8.3)/4
#Enhanced within-range: dist = 7.5km (Mot = 1)
#Enhanced within-range: dist = 7.0km (Mot = 0.5)



Mod4 <- energySIM(
  replicates = 100, days = 7, env_rast = age_stack, search_radius = 50,
  sigma = .1, dest_x = 21.25, dest_y = -33.45, mot_x = 1, mot_y = 1,
  modeled_species = pop1,
  optimum_lo = 35, optimum_hi = 50, init_energy = 50,
  direction = "W", write_results = FALSE, single_rast = TRUE, mortality = TRUE)
tidy_results(Mod4, type = "results")
tidy_results(Mod4, type = "run_params")

Results4 <- Mod4$results
final_location4 <- Results4[Results4$day == "7", 3:4]
plot(final_location4$lat~final_location4$lon)

plot()

#Plot these results
windows()
energyVIZ(Mod4,title="Visualizing Altered Fire Regime",type="summary_table", aspect_ratio=5/3,
          label=TRUE, xlim = c(21.0,22.5), ylim = c(-34.5,-33.2))

1500*4*0.2

Mod5 <- energySIM(
  replicates = 100, days = 7, env_rast = age_stack, search_radius = 5,
  sigma = .1, dest_x = 21.25, dest_y = -33.45, mot_x = 1, mot_y = 1,
  modeled_species = pop1,
  optimum_lo = 25, optimum_hi = 35, init_energy = 50,
  direction = "W", write_results = FALSE, single_rast = TRUE, mortality = TRUE)
tidy_results(Mod5, type = "results")
tidy_results(Mod3, type = "run_params")

Results5 <- Mod5$results

Success2 <- Results5[Results5$day == "7",1:9]
windows()
ggplot(Success2, aes(x=lon, y=lat)) +
  geom_point(shape=1, col = "blue", pch = 16) 



#This is a within-range worst-case scenario 
test_mod <- energySIM(
  replicates = 100, days = 7, env_rast = age_stack2, search_radius = 5,
  sigma = .1, dest_x = 21.25, dest_y = -33.45, mot_x = 1, mot_y = 1,
  modeled_species = pop1,
  optimum_lo = 25, optimum_hi = 50, init_energy = 50,
  direction = "W", write_results = FALSE, single_rast = TRUE, mortality = TRUE)
# View Results in Clean Format
tidy_results(test_mod, type = "results")
tidy_results(test_mod, type = "run_params")
#So when you really restrict the suitability and energy then they either die or run into a dead end and when you further increase the search radius they tend to die more 












windows()
energyVIZ(test_mod,title="Visualizing Worst-Case Scenario",type="plot", aspect_ratio=5/3,
          label=TRUE, xlim = c(21.0,22.5), ylim = c(-34.5,-33.2))

#Worst-case cross-range movement 
test_mod2 <- energySIM(
  replicates = 100, days = 7, env_rast = age_stack2, search_radius = 5,
  sigma = .1, dest_x = 22.0, dest_y = -33.8, mot_x = 0.1, mot_y = 0.1,
  modeled_species = pop1,
  optimum_lo = 25, optimum_hi = 35, init_energy = 50,
  direction = "S", write_results = FALSE, single_rast = TRUE, mortality = TRUE)


# View Results in Clean Format
tidy_results(test_mod2, type = "results")
tidy_results(test_mod2, type = "run_params")

barplot(Results1$distance)
#So under moderate search radius and high motivation they actually all just die 
#Must be because they are being forced to move more so they're putting themselves at more risk and 

windows()
energyVIZ(test_mod2,title="Visualizing High Motivation",type="plot", aspect_ratio=5/3,
          label=TRUE, xlim = c(21.0,22.5), ylim = c(-34.5,-33.2))

citation("abmR")

