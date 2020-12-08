# Packages
library(lavaan) #not used so far
library(piecewiseSEM)
library(nlme)
library(lme4)

# Set working directory (Dropbox Paper)
setwd("~/Dropbox/Phd/Paper IV - seagrass household poverty/Data")

# load household data into R (all data)
household<-read.csv("Household Data.csv", header = TRUE, sep=",")
as.matrix(household)
head(household)
names(household)
summary(household)
household$Country<-as.factor(household$Country)
household$Region<-as.factor(household$Region)
household$Community<-as.factor(household$Community)

#create list of structural equations
household.list<- list(
  lmer(Household.size ~ Men + Women + Children
       + (1 | Country:Region:Community), data = household, na.action=na.omit),
                      
  lmer(Household.Occupations ~ Household.size
       + (1 | Country:Region:Community), data = household, na.action=na.omit),
                      
  lmer(Dollar.Day ~ Household.Occupations + Household.size + Families
       + (1 | Country:Region:Community), data = household, na.action=na.omit),
  
  lmer(Seagrass ~ Household.size + Families + Men + Women + Children
        + Own.Boat + Boat.Quantity + Own.Fishing.Gear + Gear.Quantity
        + Household.Occupations + Dollar.Day
        + Fishing.for.food + Fishing.for.livilihood
        + (1 | Country:Region:Community), data = household, na.action=na.omit)
)

#Convert to PiecewiseSEM object
household.psem <- as.psem(household.list)

#Create summary
household.summary <- summary(household.psem, .progressBar = F)
household.summary
plot(household.psem)

# Refine structral equations
household.list2<- list(
  lmer(Seagrass ~ Own.Boat + Children
       + Household.Occupations
       + Fishing.for.livilihood
       + (1 | Country:Region:Community), data = household, na.action=na.omit)
)

household.psem2 <- as.psem(household.list2)
(household.summary2 <- summary(household.psem2, .progressBar = F))
household.summary2
plot(household.psem2)

