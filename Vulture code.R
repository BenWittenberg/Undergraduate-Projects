install.packages("CircStats")
library(CircStats)
install.packages("circular")
library(circular)
rm(list = ls())
#WOOOOO it works
#Vulture Project Stats
#In this project we used directional data and therefore circular statistics are more useful 
#read the data in 
setwd("C:/Users/bensc/Downloads/Ben UCT/BIO3018/Vulture Project")
vulture <- read.csv2("Vulture data.csv")
View(vulture)
windows()
hist(vulture$Orientation)
#Definitely more around 300 bearing 

sum(vulture$Number.of.Vultures)
#377 vultures in total 

?circular

orientation <- vulture$Orientation

circular(orientation, type = "directions", units = "degrees", template = "none", modulo = "2pi", zero = 0, rotation = "clock", orientation)
#Well something happened 
Circular Data: 
  Type = directions 
Units = degrees 
Template = none 
Modulo = 2pi 
Zero = 0 
Rotation = clock
[1] 331 342 329 323 346 347  28 334 334 327 313
[12] 320 325 319 319 327 327 333 323  72 338 341
[23] 327 332 330 226  34  63  42 349 349 354 347
[34] 346 344 346 356 340  13 344 334 342 340  53
[45] 334 344   9 333 332 342 348 348 315 317 312
[56] 310 316 354 336  17 321 327   2 353   9 349
[67] 334 320 320 330 329 339 315  25 318 310 348
[78] 227 300 255 311 315 327 340 327 353 320 315
[89] 340 336 348 340 354 304 318 329 347 332 355
[100] 358 329 324 320 329 305 332 298 333 342  14
[111] 330 302 315 312  29 324  18 310 326 293 304
[122] 335  32 306 305 294 356 330 164 357 180 234
[133] 260 275 304 307 276 315 303 310 300 340 330
[144] 345 307 323 319 330 295 321 210 338 300 315
[155] 295  16 326 310 291 353 310 306 322 320



as(orientation, control.circular = list(c(331,342,329,323,346,347,28,334,334,327,313,320,325,319,319,327,327,333,323,72,338,341)),)
is(orientation)
print(orientation, info = TRUE)

direction <- circular(orientation, type = "directions", units = "degrees", template = "none", modulo = "2pi", zero = 360, rotation = "clock", orientation)
print(direction)
is.circular(direction)
windows()
plot(direction)
class(direction)

#Now try for the first day only 
Day1 <- vulture[1:66,4]
Circ_Day1 <- circular(Day1, type = "directions", units = "degrees", template = "none", modulo = "2pi", zero = 360, rotation = "clock", Day1)
print(Circ_Day1)
windows()
plot(Circ_Day1)

Day2 <- vulture[67:164,4]
Circ_Day2 <-circular(Day2, type = "directions", units = "degrees", template = "none", modulo = "2pi", zero = 360, rotation = "clock", Day2) 
print(Circ_Day2)

windows()
plot(Circ_Day1|Circ_Day2)

#Bearing: -67.190
#So bearing = 360 -67.190
360 -67.190
#Bearing = 292.81

#USING CHAT GPT
mean <- circular(direction, units = "degrees")
mean_dir <- mean(mean)
cirvar <- var.circular(mean)
library(CircStats)
mean <- rnorm.circular(100, mu = pi/2, sd = 0.1)
hist.circular(mean)


mean <- mean(direction, type = "circular")
ave <- mean(direction)



rayleigh.test(mean_dir)
#       Rayleigh Test of Uniformity 
#General Unimodal Alternative 

#Test Statistic:  1 
#P-value:  0.5122 
#Hmm I don't know man, looks a little sketchy to me 

citation("circular")

#Now let's try a Rayleigh's test on the data
?rayleigh.test
mean_dir <- circular(mean(direction), type = "directions", units = "degrees", template = "none", modulo = "2pi", zero = 360, rotation = "clock", orientation)
View(mean_dir)
x <- rayleigh.test(direction, mu = mean_dir)
print(x, digits = 5)

#Output 
Rayleigh Test of Uniformity 
Alternative with Specified Mean Direction:  2.912831 

Test Statistic:  0.73106 
P-value:  0 
#Is mean_dir even circular? 
is.circular(mean_dir)
#Apparently it is circular 
#FEK YEAH
#Or here's another example: 
y <- rvonmises(n = 164, mu = circular(pi), kappa = 2)
rayleigh.test(y)
#don't think that worked 

#Rayleigh test just with mean_dir 
rayleigh.test(direction)
?rvonmises

#Now rayleigh's test for each individual day 
mean_1 <- circular(mean(Circ_Day1), type = "directions", units = "degrees", template = "none", modulo = "2pi", zero = 360, rotation = "clock", mean(Circ_Day1))


rayleigh.test(Circ_Day1, mu = mean_1)
#Output 
Rayleigh Test of Uniformity 
Alternative with Specified Mean Direction:  2.48764 

Test Statistic:  0.8444 
P-value:  0

#Now we try for Day 2: 
mean_2 <- circular(mean(Circ_Day2), type = "directions", units = "degrees", template = "none", modulo = "2pi", zero = 360, rotation = "clock", mean(Circ_Day2))

rayleigh.test(Circ_Day2, mu = mean_2)
#Output
Rayleigh Test of Uniformity 
Alternative with Specified Mean Direction:  3.214178 

Test Statistic:  0.6548 
P-value:  0
#Well we can see if that's realistic, how about we try with the specified orientation to the restaurant? 
rayleigh.test(direction, mu = circular(302))

#Output: 
Rayleigh Test of Uniformity 
Alternative with Specified Mean Direction:  3.783476 

Test Statistic:  0.1456 
P-value:  0.0041 
#WHOOOOO IT'S STATISTICALLY SIGNIFICANT!!!!

#Let's try for the different days: 
#Day 1: 
rayleigh.test(Circ_Day1, mu = circular(292.81))

#So not significant for the first day
Rayleigh Test of Uniformity 
Alternative with Specified Mean Direction:  3.783476 

Test Statistic:  -0.0352 
P-value:  0.6566

#Let's try for the second day: 
rayleigh.test(Circ_Day2, mu = circular(292.81))
    
#So that's extremely significant
Rayleigh Test of Uniformity 
Alternative with Specified Mean Direction:  3.783476 

Test Statistic:  0.2673 
P-value:  1e-04 

#Must be because we had more samples on the second day = higher power 

#Now let's look at Incoming/Outgoing birds


View(vulture)
incoming <- vulture[vulture$Incoming..I..Outgoing..O.=="I", 4]
incoming

outgoing <- vulture[vulture$Incoming..I..Outgoing..O.=="O", 4]
outgoing

incoming_circ <- circular(incoming,type = "directions", units = "degrees", template = "none", modulo = "2pi", zero = 360, rotation = "clock", incoming )

outgoing_circ <- circular(outgoing,type = "directions", units = "degrees", template = "none", modulo = "2pi", zero = 360, rotation = "clock", outgoing )

windows()
plot(incoming_circ)

windows()
plot(outgoing_circ)

#Let's do a Rayleigh test on these bad boys 
rayleigh.test(incoming_circ, mu = circular(292.81))

#Output
Rayleigh Test of Uniformity 
Alternative with Specified Mean Direction:  3.783476 

Test Statistic:  0.1545 
P-value:  0.0038

#Incoming vultures did come in from the restaurant side 
#Not accounting for possibly advantageous wind direction? Well the actual landing wasn't done into the wind, so why else come from there?

rayleigh.test(outgoing_circ, mu = circular(292.81))

#Output 
Rayleigh Test of Uniformity 
Alternative with Specified Mean Direction:  3.783476 

Test Statistic:  0.0567 
P-value:  0.3795

#So outgoing vultures had no statistical tendency towards the restaurant 
                                              

#Power calculations 
rans = 10000

Power.sample$V[e] <- sum(1*(VonM.sample$V < 0.05))/rans

ray_out
V_out






#RANDOM DATA EXPLORATION: 

summary(Day1)
Day1
mean(vulture$Number.of.Vultures)
#Mean number of vultures ~ 2.3 per group

rayleigh.test(direction, mu = circular())
?simper
?anosim


#How many vultures were coming from each of the heights? Low, Medium or High? 
vulture[vulture$Height == "L", 2]
#So there were 90 sightings of Low-flying vultures
#What about Medium flyers? 
vulture[vulture$Height == "M",2]
#56 sightings of Medium flyers 
#And High-flyers? 
vulture[vulture$Height == "H",2]
#18 sightings of High-flying vultures 

#And what about raw numbers? 
sum(vulture[vulture$Height == "L",3])
#So 208 vultures were low-flying
sum(vulture[vulture$Height == "M",3])
#140 vultures were flying @ medium height
sum(vulture[vulture$Height=="H",3])
#And only 29 vultures were flying high

rayleigh.test(direction, mu = circular(320))


