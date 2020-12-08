# Packages
library(lavaan)
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
       + (1 | Region) + (1 | Community), data = household),
                      
  lmer(Household.Occupations ~ Household.size
       + (1 | Region) + (1 | Community), data = household),
                      
  lmer(Annual.Household.Income ~ Household.Occupations + Household.size + Families
       + (1 | Region) + (1 | Community), data = household),
  
  lmer(Seagrass ~ Household.size + Families
        + Own.Boat + Boat.Quantity + Own.Fishing.Gear + Gear.Quantity
        + Household.Occupations + Annual.Household.Income
        + Fishing.for.food + Fishing.for.livilihood
        + (1 | Region) + (1 | Community), data = household)
)

#Convert to PiecewiseSEM object
household.psem <- as.psem(household.list)

#Create summary
household.summary <- summary(household.psem, .progressBar = F)
household.summary
plot(household.psem)

# Refine structral equations
household.list2<- list(
  lmer(Household.size ~ Men + Women + Children
       + (1 | Region) + (1 | Community), data = household),
  
  lmer(Seagrass ~ Household.size
       + Own.Boat
       + (1 | Region) + (1 | Community), data = household)
)

household.psem2 <- as.psem(household.list2)
(household.summary2 <- summary(household.psem2, .progressBar = F))
household.summary2
plot(household.psem2)
