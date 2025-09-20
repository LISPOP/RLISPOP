knitr::opts_chunk$set(echo = T, message=F, warning=F)
library(knitr)
library(here)



1+2

2-1

2*2

10000/25

#Store the results of 1+2 in `a`
a<-1+2


a

a+3













c(1,2,3,4,5,6)


#Assign vec1
vec1<-c(1,2,3,4,5,6)
#Print vec1
vec1

#Assign to housing
c("house", "condo", "apartment", "house", "condo")

#Assign to housing
housing<-c("house", "condo", "apartment", "house", "condo")
housing

#first name
#last name
#Sex
#age
#date of birth





# #Make a data frame with first_name from the vector first_name
# data.frame(first_name=first_name)
# #Make a data frame with first_name and last_name
# data.frame(first_name=first_name, last_name=last_name)

# data.frame(variable1=first_name, variable2=last_name)

# Make the data frame of all the class variables here
# Remember to assign it to a name. 


# #Get the first name variable
# df$first_name
# 

#Practice getting the other variables

# length(df$first_name)

# ncol(df)

# nrow(df)

# str(df)

# summary(df)

# View(df)

# #Delete
# rm(df)
# 

df

# #Delete
# #rm(df)
# 

# install.packages('janitor')

# library(janitor)

#install.packages('tidyverse')
library(tidyverse)





summary(df)


glimpse(df)

df$first_name


vec1<-c(1, 1, 1, 1, 1)

vec2<-c("1", "1", "1", "1", "1")

vec1
vec2

vec1+1


vec2+1

#Step 1
factor(df$sex)


factor_sex<-factor(df$sex)
# factor_sex



glimpse(df)

#Factor the sex variable and store it attached to the data frame
df$factor_sex<-factor(df$sex)



#print the original
df$sex
#Print the new one
df$factor_sex
#Glimpse
glimpse(df)

glimpse(df)
class(df)


#Try converting dob to date
as.Date(df$dob)
#Store it
df$dob2<-as.Date(df$dob)
#Compare
glimpse(df)

# #Try to convert first_name to a number
# as.numeric(df$first_name)
# #Try to convert sex to a number
# as.numeric(df$sex)
# #Try to convert factor_sex to a number
# as.numeric(df$factor_sex)
# #Try to convert age to an integer
# as.integer(df$age)
# 
