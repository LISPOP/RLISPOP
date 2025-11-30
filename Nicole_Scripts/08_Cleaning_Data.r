#libraries
library(readxl)
library(haven)
library(tidyverse)
library(lubridate)
library(here)
library(car)

table(iris$Species)

table(iris$Sepal.Length)

#view(iris$Sepal.Length)

# iris %>% 
#   mutate(Species2=Recode(Species,"'versicolor'='setosa'")) %>% view()

iris %>% 
  mutate(Species2=Recode(Species,"'versicolor'='setosa'")) ->test
table(test$Species2)

#Now repeat and resave the dataset
iris %>% 
  mutate(Species2=Recode(Species,"'versicolor'='setosa'")) ->iris

#Exercise 1
salaries <- read_excel("course_data/salary_scrambled.xlsx")

#Exercise 2
glimpse(salaries)
summary(salaries)
str(salaries)
head(salaries)
tail(salaries)

#Exercise 3
salaries <- salaries %>%
  rename(
    timestamp = Timestamp,
    age = Age,
    industry = Industry,
    salary = Salary,
    gender = Gender)

#Exercise 4
salaries <- salaries %>%
  mutate(timestamp = ymd_hms(timestamp) %>% as_date())

#Exercise 5
salaries <- salaries %>%
  mutate(age = gsub("[^0-9]", "", age)) %>%
  mutate(age = as.numeric(age)) %>% 
  filter(age >= 18, age <= 65) %>% 
  mutate(age = as.factor(age))

#Exercise 6
salaries <- salaries %>%
  mutate(industry = trimws(industry)) %>%          # remove leading/trailing spaces
  mutate(industry = gsub("[^A-Za-z /&]", "", industry)) %>%  
  mutate(industry = as.factor(industry))

#Exercise 7
salaries <- salaries %>%
  mutate(salary = gsub("[\\$,]", "", salary)) %>%   # remove $ and ,
  mutate(salary = gsub("k", "000", salary, ignore.case = TRUE)) %>% 
  mutate(salary = trimws(salary)) %>%
  mutate(salary = as.numeric(salary))

summary(salaries$salary)

#Exercise 8
salaries <- salaries %>%
  mutate(gender = tolower(gender)) %>%              # make consistent
  mutate(gender = trimws(gender)) %>%
  mutate(gender = gsub("[^a-z]", "", gender)) %>%   # remove all non-letters
  mutate(gender = as.factor(gender))

table(salaries$gender)
glimpse(salaries)
summary(salaries)