#1.1 Calculator
1 + 2
2 - 1
2 * 2
1000 / 25

#1.2 Assignment
#Storing the result of 1+2 in a
a <- 1+2

#Math with a
a + 3

#Reverse assignment
1 + 2 -> a

#1.3 The Script Is Where The Work Is
#Cleared workspace/ environment with broom
a

#2.0 Vectors
c(1, 2, 3, 4, 5, 6)
#Assign it to vec1
vec1 <- c(1, 2, 3, 4, 5, 6)
#Print vecl
vec1
#Assign to housing
c("house", "condo", "apartment", "house", "condo")
#Assign to housing part two
housing<-c("house", "condo", "apartment", "house", "condo")
housing

#first name
first_name<-c('Nicole')
#last name
last_name<-c('Lehecka')
#Sex
sex<-c('Female')
#age
age<-c(21)
#date of birth
dob<-c('2004-04-09')

#4 Data Frames
#Make a data frame with first_name from the vector first_name
data.frame(first_name=first_name)
#Make a data frame with first_name and last_name
data.frame(first_name=first_name, last_name=last_name)
#OR
data.frame(variable1=first_name, variable2=last_name)

df<-data.frame(first_name=first_name,
               last_name=last_name,
               dob=dob,age=age, sex=sex)

#4.1 Accessing variables in a data frame
#Get the first name variable 
df$first_name

#Practice getting the other variables
#Using $

#5 Functions for working with objects
length(df$first_name)
ncol(df)
nrow(df)
str(df)
summary(df)
View(df)
#Delete 
rm(df)
df

#Delete 
#rm(df)

#6 Libraries
install.packages('janitor')

#6.1 Base R versus the Tidyverse
install.packages('tidyverse')
library(tidyverse)

#7 Data Types
summary(df)
glimpse(df)

#7.2 Data types and levels of measurement
#7.21 Characters
df$first_name
vec1<-c(1, 1, 1, 1, 1)
vec2<-c("1", "1", "1", "1", "1")

vec1+1
vec2+1

#7.2.2 Factors
factor(df$sex)
factor_sex<-factor(df$sex)
include_graphics(path="images/factor_sex.png")
glimpse(df)

#Factoring the sex variable and storing it attached to the data frame
df$factor_sex<-factor(df$sex)
df$sex
df$factor_sex
glimpse(df)

#7.2.3 Numeric variables
#numeric, double, integer
#double and numeric are basically identical, allows for decimals
#integer does not allow for variables

#7.2.4 Dates
glimpse(df)
class(df)
as.Date(df$dob)
#store
df$dob2<-as.Date(df$dob)
glimpse(df)

#7.2.4.1
#conversion first_name to number
as.numeric(df$first_name)
#sex to number
as.numeric(df$sex)
#factor_sex to number
as.numeric(df$factor_sex)
#age to integer
as.integer(df$age)