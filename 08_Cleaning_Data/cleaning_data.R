## ------------------------------------------------------------------------------------
#| label: load-libraries 
#load libraries
#This library raeds in excel worksheets
library(readxl)
library(haven)
library(tidyverse)
#This library is extremely useful for modifying dates
library(lubridate)
library(here)
# This library provides a very useful Recode command
library(car)



## ------------------------------------------------------------------------------------
data("iris")
glimpse(iris)
iris$Species


## ------------------------------------------------------------------------------------
#| label: table-species
table(iris$Species)



## ------------------------------------------------------------------------------------
#| label: factor-species
factor(iris$Species)
  


## ------------------------------------------------------------------------------------
#| label: table-length
table(iris$Sepal.Length)
#view(iris$Sepal.Length)


## ------------------------------------------------------------------------------------
#| label: species-view


# iris %>% 
#   mutate(Species2=Recode(Species,"'versicolor'='setosa'")) %>% view()


## ------------------------------------------------------------------------------------
#| label: save-inspect
iris %>% 
mutate(Species2=Recode(Species,"'versicolor'='setosa'")) ->test
table(test$Species2)
#Now repeat, but resave the dataset over the old one. 

iris %>% 
mutate(Species2=Recode(Species,"'versicolor'='setosa'")) ->iris

