#Reproductive strategies in two Sparidae: 

setwd("C:/Users/bensc/Downloads/Ben UCT/BIO3017S Marine Resources/Project 1")
dgetwd()


Pachy <- read.csv("Pachymetopon_blochii.csv")
Pachy2 <- read.csv2("Pachymetopon_blochii.csv")
Pachy_clean <- read.csv2("Pachymetopon_blochii_clean.csv")
#NotePachy2 is the data frame wherein I have added the season in which the fish was caught 
#1 = Autumn (Apr - Jun)
#2 = Winter (Jul - Sep)
#3 = Spring (Oct - Dec)
#4 = Summer (Jan - March)
Spony <- read.csv("Spondyliosoma_emarginatum.csv")
Spony2 <- read.csv2("Spondyliosoma_emarginatum.csv")
#In the code below Pachy is the df for Cape Bream, Spony is the df for Steenjie 


#Plot a histogram of Total Lenght of female and male P. blochii

par(mfrow = c(1,2))
windows()
ggplot(Pachy_clean, aes(x = TL..mm.,group = Sex, fill = Sex) +
       geom_histogram(position = "identity", colour = c("orange","blue"))
                      
hist(as.numeric(Pachy$TL..mm.), xlab = "Total length (mm)", col = c("orange"), main = NULL)
hist(as.numeric(Pachymetopon[Pachymetopon$Sex == "M",3]), xlab = "Total length (mm) of male P. blochii", col = "orange", main = NULL)



#Ploty a histogram of Spondyliosoma female and male total length 
windows()
par(mfrow = c(1,2))
hist(as.numeric(Spony[Spony$Sex == "F",3]), xlab = "Total length (mm) of female S. emarginatum", col = "orange", main = NULL)
hist(as.numeric(Spony[Spony$Sex == "M",3]), xlab = "Total length (mm) of male S. emarginatum", col = "orange", main = NULL)




#Make a new variable called GSI for Pachymetopon 
#GSI is GW/(Mass-GW)
GSI.pachy <- Pachymetopon$GW/(Pachymetopon$Mass..g.- Pachymetopon$GW)
Pachy2$GSI <- as.numeric(Pachy2$GW)/(as.numeric(Pachy2$Mass..g.)- as.numeric(Pachy2$GW))
GSI.pachy <- GSI.pachy[-142]
#note the NA in this list is the single hermaphrodite 


#Do this for the clean Pachy data 
Pachy_clean$GSI <- as.numeric(Pachy_clean$GW)/(as.numeric(Pachy_clean$Mass..g.) - as.numeric(Pachy_clean$GW))

#Now I need to make GSI values for S. emarginatum 
Spony2$GSI <- as.numeric(Spony2$Gonad.mass..g.)/(as.numeric(Spony2$Mass..g.)-as.numeric(Spony2$Gonad.mass..g.))

plot(GSI ~ Gonad.stage, data = Spony)
#Now I need to find a way to get the average GSI per Gonad Stage for both sexes and for both fish 
#That might be a challenge 

#Let's see if I can do some wizardry here: 
Spony2$Fat <- as.factor(Spony2$Fat)

#But I can create a pivot table in R using the group_by() and summarize() functions in the dplyr package

install.packages("dplyr")
library(dplyr)
#Example code
Pachy2%>% 
 group_by(Sex,Season) %>% summarize(ave_GW = mean(as.numeric(GW)))
#My main problem remains that this programme doesn't understand that Fat and Gonad Stage are categorical variables 


#Oh my god it worked 
# A tibble: 14 × 3
# Groups:   Sex [5]
Sex   Season   ave_GSI
<chr>  <int>     <dbl>
2 "F"        1  0.0488  
3 "F"        2  0.0397  
4 "F"        3  0.0157  
5 "F"        4  0.0162  
11 "M"        1  0.0423  
12 "M"        2  0.0260  
13 "M"        3  0.00462 
14 "M"        4  0.0175 

#GSI is highest in Autumn and Winter, which is what you would expect if breeding commences in winter, and for herbivorous fish they need to start the condition cycle early 

#This looks very much like how Colin wanted it, but without the Gonad Stage (GS) variable
#So, for the time being, I am happy 

#I'm going to do this the long way for Gonad stage and Fat content 
#Manual, baby
#This should give us the modal gonad stage for season 1 

hist(as.numeric(Pachy2[Pachy2$Season == 1, 7]))
#This gives us a fairly normally distributed gonad stage histogram, but stage 4 is the mode 
hist(as.numeric(Pachy2[Pachy2$Season == 1, 7]))
  


#When I've done that I need to produce a histogram of average GSI per season for both sexes and species 

#After that I need to determine how many fish are in each gonad stage in each season 
#And I do this for each sex in each species
#This should tell us the breeding season as it will let us see when the gonads are at what stage and from this we can gauge the breeding season 


#The data shows that these guys breeding in season 2 & 3 (i.e. autumn and winter) this is because Pachymetopon is a herbivore. And algae are the first things to grow in spring so they need to spawn early to get their larvae enough food to grow 
#Let's brainstorm how to do that: 


#We'll also need to do a chisq test to see whether fat stage (1 - 3) has a relationship to season (1 - 4) 
#Because both season and fat level are discrete we have to do a chisq test 


#We also need to do predicted versus actual mass for each species to get Le Cren's condition index. 

#Here I'm plotting Mass as a function of Length for Pachymetepon 

LeCren <- lm(Mass..g.~TL..mm., data = Pachy2)
LeCren
#This is the output: 
lm(formula = Mass..g. ~ TL..mm., data = Pachy)

Coefficients:
  (Intercept)      TL..mm.  
-577.438        3.672 
summary(LeCren)
#The summary of the linear model: 
Coefficients:
                 Estimate   Std. Error    t value Pr(>|t|)    
(Intercept) -577.43817      14.09919   -40.95   <2e-16 ***
  TL..mm.        3.67155    0.05829     62.99   <2e-16 ***
  
Residual standard error: 56.4 on 504 degrees of freedom
(1 observation deleted due to missingness)
Multiple R-squared:  0.8873,	Adjusted R-squared:  0.8871 
F-statistic:  3968 on 1 and 504 DF,  p-value: < 2.2e-16

#So we have an R-sq of 0.88, which means that Length explains a lot of the variation in Mass. It will probably be even more if I log-transform the data

#Log-transformed linear model of mass as a function of length

logleCren <- lm(log(Mass..g.)~log(TL..mm.), data = Pachy_clean)
summary(logleCren)


#Now let's look at the diagnostics for the log-transformed linear model 
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -10.18461    0.13564  -75.08   <2e-16 ***
  log(TL..mm.)   2.88442    0.02485  116.09   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1015 on 504 degrees of freedom
(1 observation deleted due to missingness)
Multiple R-squared:  0.9639,	Adjusted R-squared:  0.9639 
F-statistic: 1.348e+04 on 1 and 504 DF,  p-value: < 2.2e-16

#We have an R-sq of 0.96 so length explains 96% of the variation in weight. We are interested in the 4-3% it doesn't explain 

#So let's calculate the predicted log-transformed mass for Pachymetepon blochii
predicted_log_2 <- -10.14393 + 2.87700*log(Pachy_clean$TL..mm.)
predicted_log_2

#This gives us a plausible-looking list 
#Now we need to back-transform it
predicted_2 <- exp(predicted_log_2)
predicted_2
#Okay this looks good: but we'll see how these weight values look after we find le Cren's index 
Pachy_clean$predicted <- predicted_2
#Okay just by eyeballing the df it looks good 

#le Cren's index is actual mass/predicted mass for that length 
leCren_2 <- Pachy_clean$Mass..g./Pachy_clean$predicted
leCren_2
#This looks right!
Pachy_clean$leCren <- leCren_2


#AND REMEMBER I NEED TO DO THIS FOR BOTH SPECIES
#So let me now do this for Steenjie 
leCren_spony <- lm(log(as.numeric(Mass..g.))~log(as.numeric(TL..mm.)), data = Spony2)
summary(leCren_spony)

#Let's have a look at this linear model
Residuals:
  Min      1Q  Median      3Q     Max 
-66.454 -18.980  -3.681  12.757 135.240 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -529.51076    8.95294  -59.14   <2e-16 ***
  TL..mm.        3.22433    0.03602   89.53   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 30.62 on 656 degrees of freedom
Multiple R-squared:  0.9243,	Adjusted R-squared:  0.9242 
F-statistic:  8015 on 1 and 656 DF,  p-value: < 2.2e-16

#So the Steenjie model has an R-sqr of 0.9243 (so the total length explains 90% of the variation in weight) but we are interested in the 10% that it doesn't explain 
#Intercept is -529.51076
#And the slope is 3.22433
predicted <- -529.51076 + 3.22433*Spony2$TL..mm. 
Spony2$predicted <- predicted

Spony2$leCren <- as.numeric(Spony2$Mass..g.)/as.numeric(Spony2$predicted)

#So this looks more or less correct, I'll have to check it against some other people's code but looks plausible for now 
Spony2$leCren 
#I can also make pivot tables in R using the group_by() function and the summarize() functions in the dpylr package 

#In the code below I copy the pivot table code I used for Pachy and apply it to Spony 
Spony2%>% 
  group_by(Sex, Season) %>% summarize(summary_Gonad.stage = summary(as.factor(Gonad.stage)))
#Which returns a pivot table. I think Colin also wanted me to include Gonad Stage (GS) but I'll settle for this for now 
# A tibble: 9 × 3
# Groups:   Sex [3]
Sex   Season   ave_GSI
<chr>  <int>     <dbl>
1 F          1  0.0166  
2 F          2  0.0678  
3 F          3  0.0323  
4 F          4  0.00878 
6 M          1  0.00119 
7 M          2  0.00440 
8 M          3  0.00458 
9 M          4  0.000882

Spony2%>% 
  group_by(Sex, Season) %>% summarize(ave_GSI = mean(GSI))

#GSI is highest in Spony during the winter and spring, which roughly fits with the spawning season. As predators these should start the condition cycle slightly behind the Cape bream 

#I think my main problem with getting the modal Gonad stage is R thinks this is a number and therefore doesn't want to give me a mode? 

#Modal Gonad stage for different seasons in Spony2
Spony2[Spony2$Season == 1, 7]
#So modal Gonad stage for season 1 in Spony is 2
hist(Spony2[Spony2$Season == 2, 7])
#So the modal gonad stage in season 2 is 5, followed by 6
hist(Spony2[Spony2$Season == 3, 7])
#So the modal gonad stage in season 3 is gonad stage 2 
hist(Spony2[Spony2$Season == 4, 7])
#By far the modal gonad stage in season 4 is gonad stage 1 


#I should do an ANOVA to test the GSI differences between seasons, followed by a Tukey test to see where the differences lie 

anova1 <- aov(GSI ~ Season, data = Pachy2)
summary(anova1)
TukeyHSD(anova1)


#Output shows that there is a difference in average GSI between seasons 
Df Sum Sq Mean Sq F value Pr(>F)    
Season        1 0.0652 0.06522   87.21 <2e-16 ***
  Residuals   504 0.3769 0.00075                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1 observation deleted due to missingness
#But which observations are different? 

TukeyHSD(anova1)

#What about differences in GSI per Sex? 
anova2 <- aov(GSI~as.factor(Sex)*as.factor(Season), data = Pachy2)
summary(anova2)

TukeyHSD(anova2)
#Tukey shows that the only significant difference is between Juveniles and Females in terms of GSI - this is to be expected given that these are gonochoristic and therefore don't vary hugely in terms of weight and age of sexes (p = 0.0011607)

#But what about other condition indicators? 
anova3 <- aov(Fat ~ Season, data = Pachy2)
summary(anova3)
#Statistical significant difference in fat content between seasons (F = 7.211, p = 0.00749)

anova_condition <- aov(leCren ~ GS, data = Pachy2)
summary(anova_condition)
#Significant: GS df(9)  Sum Sq(0.308) Mean Sq(0.03424)   F-value(3.264) P(0.000718 ***)
anova_condition2 <- aov(leCren ~ Gonad.stage, data = Spondy_female)
summary(anova_condition2)

anova4 <- aov(Fat ~ Sex, data = Pachy2)
summary(anova4)
#No difference in fat content between sexes 

#Perhaps I should look for differences in Gonad weight? 
anova5 <- aov(as.numeric(Gonad.mass..g.) ~ as.factor(Season), data = Spony2)
summary(anova5)
TukeyHSD(anova5)


#Let me now look for differences in GSI between sexes 
anova6 <- aov(GSI ~ Sex, data = Spony2)
summary(anova6)
TukeyHSD(anova6)




#So there is a difference in mean GSI, with higher values fior female fish (0.03612472) than male fish (0.02953647)

#What about seaosnal variation in GSI in just female fish 
anova7 <- aov(GSI ~ as.factor(Season), data = Pachy2)
summary(anova7)
TukeyHSD(anova7)
#So there are significant differences in GSI between all seasons except Summer and Spring 
anova8 <- aov(leCren ~ as.factor(Fat), data = Pachy2)
summary(anova8)
#Condition varies significantly with fat (df = 3, Mean Sq = 0.13, F = 12.79, p = 4.58e-08)

anova9 <- aov(leCren ~ as.factor(Season), data = Pachy2)
summary(anova9)
#Significant: (df = 3, Mean Sq = 0.07, F = 6.3, p = 0.000325)
anova10 <- aov(leCren ~ Season, data = Spondy_male)
summary(anova10)
#So it's only significant when you account for sex 

#Now to do the tedious reporting of statistics: 
anova_test <- aov(as.numeric(GW) ~ Season, data = Pachy2)
summary(anova_test)





#And I need to find a way to display the equation on the regression
#b should be 2.88 or something close to 3 because the mass of any object should increase to the cube of its length 
#If the b (exponent) is under 3 it is hypometric (i.e. the mass is under what you would expect)
#If b is over 3 it is hypermetric (mass is greater than what you'd expect from the length)
#When running our regression we look at R^2, look at observations (n) and regression significance (p-value) 
#The inverse of R (i.e. what length doesn't explain, will be explained by the sexual cycle - we hope)
#To get the regression equation look at intercept and the slope coefficient (as well as the 95% confidence intervals on the slope estimate - if it doesn't include 3 then we know it isn't isometric)
#Write a function to predict the mass by using the regression equation (intercept + slope*logarithm of the Total length)
#From this we can make a new colum which we shall call difference: difference will be predicted ln(mass) - ln(actual mass)
#Also back-transform the mass so it's no longer log-transformed 
#And then compare actual mass to predicted mass 
#LeCren's K (condition) = actual mass/predicted mass 
#If the ratio is below one then the fish is skinny, if above 1 the fish is fat 
#Once we have the LeCren value we can plot Le Cren's index against season for each sex and species



#Fish in season 3 should be lighter than predicted (e.g. a Le Cren ratio of 0.98) and in autumn they shoudl be heavier than expected 

#We'll have to check these with statistical tests (ANOVA and Chisq) and the graphs we need are the histograms, the linear regression, we'll need some tables: GSI versus Gonad Stage by Sex for both species, figure of GSI vs Season for both sexes for both species 

#Table of gonad stage per season by sex 
#Fat versus season by sex 
#Condition versus season by sex 


#Now we're doing a test on GSI in GS 5 between sexes

t.test(Pachy2$[Pachy2$GS == "5",11])



#I need to separate the sexes to get the frequencies of gonad stage and fat level per season 
Pachy_female <- Pachy2[Pachy2$Sex == "F",1:13]
Pachy_male <- Pachy2[Pachy2$Sex == "M",1:13]
#So that seems to have worked: now let's find modal frequencies of fat and gonad stage!
#Plot histograms showing Gonad stage in Season 1 (Autumn)
hist(as.numeric(Pachy_female[Pachy_female$Season == 1, 7]))
summary(as.factor(Pachy_female[Pachy_female$Season == 1, 9]))
c(23)/c(23+43+20+3+1)
#So modal Gonad stage in Autumn is stage 4, followed by stage 5 and no stage 7
hist(as.numeric(Pachy_female[Pachy_female$Season == 2, 7]))
summary(as.factor(Pachy_female[Pachy_female$Season == 2, 9]))
c(54)/c(54+40+20+2)
#Modal gonad stage in Winter is Stage 5, followed by 4, again no stage 7 
hist(as.numeric(Pachy_female[Pachy_female$Season == 3, 7]))
summary(as.factor(Pachy_female[Pachy_female$Season == 3, 9]))
6/c(6+16+6+2+1)
#Modal gonad stage in Spring is stage 1, with very few in other stages and a couple in stage 7
hist(as.numeric(Pachy_female[Pachy_female$Season == 4, 7]))
summary(as.factor(Pachy_female[Pachy_female$Season == 4, 9]))
6/c(6+19+18+2+1)
#Modal gonad stage in summer is still stage 1 with 2 & 3 being runners-up


#Now let us find the modal Fat level for each female P. blochii for each season 
hist(as.numeric(Pachy_female[Pachy_female$Season == 4, 9]))
#This shows that fat level 1 is the most common in summer
hist(as.numeric(Pachy_female[Pachy_female$Season == 3, 9]))
#This shows fat level 1 ist still the most common in spring 
hist(as.numeric(Pachy_female[Pachy_female$Season == 2, 9]))
#In winter fat level zero is the most common, followed by fat level one 
hist(as.numeric(Pachy_female[Pachy_female$Season == 1, 9]))
#In autumn fat level one is the most common


#Now let's replicate our success with male P. blochii 
Pachy_male <- Pachy2[Pachy2$Sex == "M",1:13]
hist(as.numeric(Pachy_male[Pachy_male$Season == 1, 9]))
summary(as.factor(Pachy_male[Pachy_male$Season == 4, 9]))
3/c(3+11+7+4)
#82 male individuals in season 1 
summary(as.factor(Pachy_female[Pachy_female$Season == 1, 7]))
#90 female individuals in season 1 
#Modal gonad stage in autumn is 3, followed by 4. No individuals in stage 7 
hist(as.numeric(Pachy_male[Pachy_male$Season == 2, 7]))
summary(as.factor(Pachy_male[Pachy_male$Season == 2, 7]))
summary(as.factor(Pachy_female[Pachy_female$Season == 2, 7]))
#Percentage ripe gonads 
c(1)/c(1+11+16+15+30+7+4+1+6+20+21+26+33+10)
#Modal gonad stage in winter is 3, followed by 1 
hist(as.numeric(Pachy_male[Pachy_male$Season == 3, 7]))
summary(as.factor(Pachy_male[Pachy_male$Season == 3, 7]))
summary(as.factor(Pachy_female[Pachy_female$Season == 3, 7]))
c(1)/c(4+9+4+1 + 4+16+3+3+1+3+1)
#Modal gonad stage is 1 in Spring
hist(as.numeric(Pachy_male[Pachy_male$Season == 4, 7]))
summary(as.factor(Pachy_male[Pachy_male$Season == 4, 7]))
summary(as.factor(Pachy_female[Pachy_female$Season == 4, 7]))
c(5+18+5+5)/c(5+18+11+10+1+1+5+5+6+4+5)
#Gonad stage in summer is a mix of stage 2, stage 4 and stage 1 


#Now I need to do the same stunt but for Spondyliosoma emarginatum 
Spondy_female <- Spony2[Spony2$Sex== "F",1:13]
Spondy_male <- Spony2[Spony2$Sex == "M",1:13]
summary(as.factor(Spony2[Spony2$Season == 1,7]))
#Clearly the vast majority in Autumn are in stage 2 
c(10 + 48)/c(10+48+1+1)
summary(as.factor(Spondy_female[Spondy_female$Season == 2,7]))
summary(as.factor(Spony2[Spony2$Season == 2,7]))
c(8 + 51)/c(8+51+53+30+72+56+2)
#In Winter there are many more samples, and of these proportionaly more are in stages 5 and 6, with many also in stage 4 and stage 3 
summary(as.factor(Spondy_female[Spondy_female$Season == 3,7]))
summary(as.factor(Spony2[Spony2$Season == 3,7]))
c(25 + 121)/c(25+121+30+19+24+4+35)
#In spring there are many (28) spent individuals with some individuals still maturing their gonads (23 in stage 5 and 16 in stage 4, 22 in stage 3). However majority in are in stage 2 
summary(as.factor(Spondy_male[Spondy_male$Season == 4,9]))
summary(as.factor(Spondy_female[Spondy_female$Season == 4,9]))
summary(as.factor(Spony2[Spony2$Season == 1,9]))
c(1 +2)/c(20+40+1+2+5)
#Summer has the fewest samples but of these most (26) are in stage 2 with runner-up being stage 1


#Let's see how fat levels vary with season for P. blochii 

summary(as.factor(Pachy2[Pachy2$Season == 1,9]))
# 0    1    2    3 NA's 
#38   81   49    5    1

#Looks like Autumn has a lot of Level 1 and level 2 for P. blochii 

summary(as.factor(Pachy2[Pachy2$Season == 2,9]))
#  0    1    2    3 NA's 
#  91   68   37    7    1
#Looks like a lot more in stages 1 and zero in winter 
summary(Spony2$TL..mm.)
summary(Pachy2$TL..mm.)
#   0    1    2    3 NA's
#   9   24   17    4    1
#Looks like there are actually fewer in the lowest levels - maybe because winter is over and spring brings more food and after breeding peak in winter means less resource stress at a time when more food available 
summary(as.factor(Pachy2[Pachy2$Season == 4,9]))
#   0    1    2    3   NA's
#  9   32   26    6    1
#Still most in the middle region
#So it looks as though fat levels are actually lowest in Autumn and Winter - because as herbivores they likely are more exposed to fluctuations whereas the predominantly predatory male S. emarginatum will be more buffered and have a later cycle




?chisq.test
chisq.test(Pachy_male$GS, Pachy_male$Season)
chisq.test(Pachy_female$GS, Pachy_female$Season)
chisq.test(Pachy2$GS, Pachy2$Fat)




#Significant relationship between fat level and gonad stage for P. blochii! (X-squared = 72.76, df = 24, p-value = 8.286e-07)

chisq.test(Spony2$Gonad.stage, Spony2$Fat)
#A significant result! Fat influences gonad stage (X-squared = 73.251, df = 18, p-value = 1.264e-08)
summary(aov(Spony2$GSI ~ Spony2$Sex))
summary(aov(Spony2$leCren ~Spony2$Sex))
summary(aov(Spondy_male$leCren ~ Spondy_male$Gonad.stage))


chisq.test(Spondy_female$Fat, Spondy_female$Season) #Significant (X-squared = 28.469,p = 0.0007962)
chisq.test(Spondy_male$Fat, Spondy_male$Season)#Significant (X-squared = 66.05, p-value = 9.011e-11)
chisq.test(Pachy_female$Fat, Pachy_female$Season) #Significant (X-squared = 28.02,p-value = 0.0009465)
chisq.test(Pachy_male$Fat, Pachy_male$Season)#Significant (X-squared = 33.068,p-value = 0.00013)

chisq.test(Spony2$Fat,Spony2$Season) #Significant (X-squared = 37.594,p-value = 2.061e-05)
chisq.test(Pachy2$Fat,Pachy2$Season) #Significant (X-squared = 47.871, p-value = 2.7e-07)

#Now let's see whether fat varies with sex 
chisq.test(Spony2$Fat, Spony2$Sex) # Significant (X-squared = 96.247, p-value < 2.2e-16)
#And let's see whether fat varies with sex in P. blochii 
chisq.test(Pachy2$Fat, Pachy2$Sex) # NOT significant (X-squared = 10.86, p-value = 0.541)

chisq.test(Spony2$Gonad.stage, Spony2$Season) # Significant (X-squared = 78.566, p-value = 7.741e-12)
chisq.test(Pachy2$GS, Pachy2$Sex)#Significant (X-squared = 463.33, p-value < 2.2e-16)

chisq.test(Spony2$Gonad.stage,Spony2$Sex)#Huh there's a sexual difference 


#Plot the Mass of P. blochii as a function of length
windows()
plot(log(Mass..g.) ~ log(TL..mm.), data = Pachy2, pch = 16, col = "blue", xlab = "Total length (mm)", ylab = "Mass (g)")
abline(lm(log(Mass..g.) ~ log(TL..mm.), data = Pachy2), col = "red")


#Now let's do this simultaneously for both species 
windows()
par(mfrow = c(1,2))
plot(log(Mass..g.) ~ log(TL..mm.), data = Pachy2, pch = 16, col = "blue", xlab = "Log Total length (mm) P. blochii", ylab = "log Mass (g)")
abline(lm(log(Mass..g.) ~ log(TL..mm.), data = Pachy2), col = "red")
plot(log(as.numeric(Mass..g.)) ~ log(TL..mm.), data = Spony2, pch = 16, col = "blue", xlab = "Log Total length (mm) S. emarginatum", ylab = "log Mass (g)")
abline(lm(log(as.numeric(Mass..g.)) ~ log(TL..mm.), data = Spony2), col = "red")

par(mfrow = c(1,1))

#Shall I try doing barplots of GSI by season? 
?barplot

ave_condition <- Pachy_clean[-139,1:13] %>%
  group_by(Season,Sex) %>%
  summarise(mean_condition = mean(leCren), sd = sd(leCren))

ave_GSI
windows()
par(mfrow = c(1,2))

windows()
ggplot(ave_GSI, aes(x = Season, y = mean_GSI, fill = Sex)) +
  geom_errorbar(aes(ymin = 0, ymax = (mean_GSI + sd)), width = 0.25, 
                position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  # scale_y_continuous(expand = c(0,0), limits = c(0,50)) +
  labs(x = "Season", y = "Mean condition", main = "Pachymetopon blochii") +
  scale_color_manual(name= NULL,
                     breaks=c('Male', 'Female'),
                     values=c('Male'='blue', 'Female'='red'))+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=14))
  theme_classic() +
  theme(legend.position = "top left")

ggplot(ave_GSI_Spondy, aes(x = Gonad.stage, y = mean_GSI, fill = Sex)) +
    geom_errorbar(aes(ymin = 0, ymax = (mean_GSI+sd)), width = 0.25, 
                  position = position_dodge(0.9)) +
    geom_bar(stat = "identity", position = "dodge", colour = "black") +
    # scale_y_continuous(expand = c(0,0), limits = c(0,50)) +
    labs(x = "Gonad stage", y = "Mean GSI", main = "Spondyliosoma emarginatum") +
    scale_color_manual(name= NULL,
                       breaks=c('Male', 'Female'),
                       values=c('Male'='blue', 'Female'='red'))+
    theme(legend.title=element_text(size=20),
          legend.text=element_text(size=14))
  theme_classic() +
    theme(legend.position = "none")

ave_GSI_Spondy <- Spony2[-339,1:13] %>%
  group_by(Season,Sex) %>%
  summarise(mean_GSI = mean(GSI), sd = sd(GSI))

windows()
par(mfrow = c(2,1))
windows()
ave_GSI_Spondy
ggplot(ave_GSI_Spondy, aes(x = Season, y = mean_GSI, fill = Sex)) +
  geom_errorbar(aes(ymin = 0, ymax = (mean_GSI+sd)), width = 0.25, 
                position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  # scale_y_continuous(expand = c(0,0), limits = c(0,50)) +
  labs(x = "Season", y = "Mean GSI", main = "Spondyliosoma emarginatum") +
  scale_color_manual(name= NULL,
                     breaks=c('Male', 'Female'),
                     values=c('Male'='blue', 'Female'='red'))+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=14))
  theme_classic() +
  theme(legend.position = "none")
?ggplot


par(mfrow = c(1,2))
windows()
ggplot(ave_GSI, aes(x = GS, y = mean_GSI, fill = Season)) +
  geom_errorbar(aes(ymin = 0, ymax = (mean_GSI+sd)), width = 0.25, 
                position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  # scale_y_continuous(expand = c(0,0), limits = c(0,50)) +
  labs(x = "Season", y = "Mean GSI", main = "Pachymetopon blochii") +
  scale_color_manual(name= NULL,
                     breaks=c('Male', 'Female'),
                     values=c('Male'='blue', 'Female'='red'))+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=14))
  theme_classic() +
  theme(legend.position = "top left") 


ggplot(ave_GSI_Sppony, aes(x = Season, y = mean_GSI, fill = Season)) +
  geom_errorbar(aes(ymin = 0, ymax = (mean_GSI+sd)), width = 0.25, 
                position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  # scale_y_continuous(expand = c(0,0), limits = c(0,50)) +
  labs(x = "Season", y = "Mean GSI", main = "Spondyliosoma emarginatum") +
  theme_classic() +
  theme(legend.position = "none")



#Tut session 
#Need to report degrees of freedom, mean squared values, for each anova and chisq test
#So I need to go back and add these
              Df Sum Sq Mean Sq F value Pr(>F)    
Season        1 0.0652 0.06522   87.21 <2e-16 ***
  Residuals   504 0.3769 0.00075                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1 observation deleted due to missingness
#E.g. df = 1,504, Mean squared = 0.06522,0.00075




#Plot some contingency tables:female P. blochii 
cont_table1 <- table(Pachy2$Fat,Pachy2$Sex)
print(cont_table1)
chisq.test(cont_table1)
19/c(4+6+19+18+2)

#Gonad stage by season for females: 
   1  2  3  4
   1  0  0  0
1  4  6  4  5
2 15 20 16 18
3 14 21  3 11
4 33 26  3 10
5 17 33  1  1
6  6 10  3  1
7  0  0  1  0
#Contingency table of gonad stage by season for male P. blochii 



#Barplot of condition by season and sex 
ave_condition <- Pachy_clean[-139,1:13] %>%
  group_by(Season,Sex) %>%
  summarise(mean_condition = mean(as.numeric(leCren)))

ave_condition


windows()
par(mfrow = c(1,2))
ggplot(ave_condition, aes(x = Season, y = mean_condition, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  # scale_y_continuous(expand = c(0,0), limits = c(0,50)) +
  labs(x = "Season", y = "Mean condition", main = "Pachymetopon blochii") +
  scale_color_manual(name= NULL,
                     breaks=c('Male', 'Female'),
                     values=c('Male'='blue', 'Female'='red'))+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=14))
theme_classic() +
  theme(legend.position = "top left")

windows()
ggplot(ave_condition_Spondy, aes(x = Season, y = mean_condition_S, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  # scale_y_continuous(expand = c(0,0), limits = c(0,50)) +
  labs(x = "Season", y = "Mean condition", main = "Pachymetopon blochii") +
  scale_color_manual(name= NULL,
                     breaks=c('Male', 'Female'),
                     values=c('Male'='blue', 'Female'='red'))+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=14))
theme_classic() +
  theme(legend.position = "top left")


ave_condition_Spondy <- Spony2[-339,1:13] %>%
  group_by(Season,Sex) %>%
  summarise(mean_condition_S = mean(leCren))

180*2
