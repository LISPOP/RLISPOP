library(haven)
library(here)
library(tidyverse)

library(haven)
shs<-read_sav(here('..','course_data', 'shs.sav'))

shs %>% 
  select(Income=HH_TotInc,Dwelling=DwelTyp, Taxes=TX001)->shs_subset
#Convert labelled variables to factors
shs_subset<-as_factor(shs_subset)

#Measures of Central Tendency
#Mean
mean(shs_subset$Income)
mean(shs_subset$Income, na.rm=T)
mean_income<-mean(shs_subset$Income)
mean_income

#Median
median(shs_subset$Income)

#Mode
table(shs_subset$Dwelling)

#Measures of Dispersion
sd(shs_subset$Income)
