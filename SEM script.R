#SEM ANALYSIS of seagrass SES
library(piecewiseSEM)

####DATA####
# Set working directory (Dropbox Paper IV)
setwd("~/Dropbox/Phd/Paper IV - seagrass household poverty/Data")

####SEM####
household_all<-read.csv("household_data.csv", header = TRUE, sep=",")
as.matrix(household_all)
head(household_all)
names(household_all)
summary(household_all)
household_all$country<-as.factor(household_all$country)
household_all$region<-as.factor(household_all$region)
household_all$area<-as.factor(household_all$area)
household_all$community<-as.factor(household_all$community)
household_all$prefered_habitat<-as.factor(household_all$prefered_habitat)
household_all$collect_marine_resources<-(factor(household_all$collect_marine_resources, levels = c(0, 1, 2, 3, 4, 5), labels = c("No","Yes","Yes","Yes","Yes","Yes")))
household_all$why_habitat<-as.factor(household_all$why_habitat)

#omit households that do not collect marine resources
household<-subset(household_all, collect_marine_resources!="No")

#omit NA
household<-na.omit(household) #654 households left - main loss of households without annual income

nlevels(household$community)

#define variables
household$adults_total<-(household$men_n + household$women_n)
household$adj_income<-log(household$adj_income)

#FULL SEM
sem_fit1<-glmer(own_motorboat ~ adj_income + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit2<-glmer(static ~ adj_income + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit3<-glmer(motile ~ adj_income + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit4<-glmer(traps ~ adj_income + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit5<-glmer(basic ~ adj_income + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit6<-glmer(seagrass_only ~ own_motorboat + static + basic + motile + traps + non_fishing_livilihoods + adj_income + dependents_n + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit7<-lmer(adj_income ~ non_fishing_livilihoods + (1| country:region), data = household)
sem_fit8<-glmer(non_fishing_livilihoods ~ adults_total + (1| country:region), data = household, family = poisson(link="log"))

household.sem<-psem(sem_fit1, sem_fit2, sem_fit3, sem_fit4, sem_fit5, sem_fit6, sem_fit7, sem_fit8, data=household)
household.sem_summary<-summary(household.sem, conserve = TRUE)
household.sem_summary #Tests of directed separation reveal two logical paths: dependents_n ~ adj_income & own_motorboat ~ non_fishing_livilihoods. 

#SEM1 #full model with missing links
sem_fit1<-glmer(own_motorboat ~ adj_income + non_fishing_livilihoods + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit2<-glmer(static ~ adj_income + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit3<-glmer(motile ~ adj_income + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit4<-glmer(traps ~ adj_income + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit5<-glmer(basic ~ adj_income + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit6<-glmer(seagrass_only ~ own_motorboat + static + basic + motile + traps + non_fishing_livilihoods + adj_income + dependents_n + (1| country:region), data = household, family = binomial(link="logit"))
sem_fit7<-lmer(adj_income ~ non_fishing_livilihoods + dependents_n + (1| country:region), data = household)
sem_fit8<-glmer(non_fishing_livilihoods ~ adults_total + (1| country:region), data = household, family = poisson(link="log"))

household.sem<-psem(sem_fit1, sem_fit2, sem_fit3, sem_fit4, sem_fit5, sem_fit6, sem_fit7, sem_fit8, data=household)
household.sem_summary<-summary(household.sem, household.sem)
household.sem_summary


#SEM2 #model including only significant paths from SEM1
sem_fit1<-glmer(own_motorboat ~ adj_income + non_fishing_livilihoods + (1| regional_hdi), data = household, family = binomial(link="logit"))
sem_fit2<-glmer(static ~ adj_income + (1| regional_hdi), data = household, family = binomial(link="logit"))
sem_fit6<-glmer(seagrass_only ~ own_motorboat + static + basic + traps + non_fishing_livilihoods + adj_income + dependents_n + (1| regional_hdi), data = household, family = binomial(link="logit"))
sem_fit9<-lmer(adj_income ~ dependents_n + non_fishing_livilihoods + (1| regional_hdi), data = household)

household.sem<-psem(sem_fit1, sem_fit2, sem_fit6, sem_fit9, data=household)
household.sem_summary<-summary(household.sem, conserve = TRUE)
household.sem_summary 


#regional hdi random factor
sem_fit1<-glmer(own_motorboat ~ adj_income + non_fishing_livilihoods + (1| regional_hdi), data = household, family = binomial(link="logit"))
sem_fit2<-glmer(static ~ adj_income + (1| regional_hdi), data = household, family = binomial(link="logit"))
sem_fit6<-glmer(seagrass_only ~ own_motorboat + static + dependents_n + (1| regional_hdi), data = household, family = binomial(link="logit"))
sem_fit7<-lmer(adj_income ~ non_fishing_livilihoods + dependents_n + (1| regional_hdi), data = household)

household.sem<-psem(sem_fit1, sem_fit2, sem_fit6, sem_fit7, data=household)
household.sem_summary<-summary(household.sem, conserve = TRUE)
household.sem_summary


gam(seagrass_only ~ own_motorboat + static + dependents_n + (1| regional_hdi), data = household, family = binomial(link="logit"))