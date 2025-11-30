1+'1'
1+1

?glimpse()

#Generate a random number 
var1<-rnorm(n=100, mean=55, sd=10)
#If your numeric variable has no decimals, round the new fake one. 
var1<-round(var1, 0)
#check
var1

#sample(*)
var2<-sample(
  #The first argument is a vector of character strings that are sample from
  c("red", "blue", "orange"), 
  #The size argument is how many times you will sample
  #Note, this should match the number of data points that you are generating in other fake variables
  #i.e. n=100 above
  size=100, 
  #usually specify T because if not, it will sample red, but not replace it, sample blue, not replace sample orange, not replace it and then have nothing more to sample 
  replace=T, 
  #If you have a sense that your factor variable more instances of one value than other, then you can specify the probability of each response. 
  prob=(c(0.2, 0.25, 0.5)))
# Note that this a *character* vector
class(var2)

#Convert factor
var2<-factor(var2)

library(tidyverse)
df<-data.frame(var1=var1, var2=var2)
head(df)
glimpse(df)

#Install this package
#install.packages("missForest")
#Load
library(missForest)
#Introduce this 
df<-prodNA(df, noNA=0.1)
glimpse(df) #notice how NA values have started to appear. 

cor(df$var1, df$var2, use="everything")

#Exercises
shs <- shs %>%
  mutate(
    West = case_when(
      Prov == "Alberta" ~ 1,
      Prov == "Saskatchewan" ~ 1,
      Prov == "Manitoba" ~ 1,
      Prov == "British Columbia" ~ 1,
      Prov == "Ontario" ~ 0,
      Prov == "Quebec" ~ 0,
      Prov == "Atlantic provinces" ~ 0,
      TRUE ~ NA_real_
    )
  )
shs %>% map(var_label)
shs <- shs %>%
  select(HH_TotInc, RP_Sex, SH001, Tenure)
shs <- shs %>%
  rename(
    Income = HH_TotInc,
    Sex = RP_Sex,
    Shelter_Costs = SH001
  )
with(shs, cor(as.numeric(Income), as.numeric(Sex), use = "complete.obs"))
