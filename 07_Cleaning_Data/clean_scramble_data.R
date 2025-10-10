#load libraries
library(readxl)
library(haven)
library(tidyverse)
library(lubridate)
library(here)

#load the data

salaries<-read_excel(path=here("data/salary_data.xlsx"))
salaries %>% 
  slice(1:50)->salaries
scramble <- function(df, difficulty = "easy") {
  # Set percentages by difficulty level
  if (difficulty == "easy") {
    perc_missing <- 0.05
    perc_outlier <- 0.02
    perc_char <- 0.03
  } else if (difficulty == "medium") {
    perc_missing <- 0.08
    perc_outlier <- 0.05
    perc_char <- 0.10
  } else if (difficulty == "hard") {
    perc_missing <- 0.15
    perc_outlier <- 0.08
    perc_char <- 0.40
  } else {
    stop("Invalid difficulty level. Choose 'easy', 'medium', or 'hard'.")
  }
  
  # Numeric scrambler
  scramble_numeric <- function(vec, perc_outlier, perc_missing) {
    n <- length(vec)
    vec_scrambled <- vec
    
    # Add outliers
    if (is.numeric(vec)) {
      n_outliers <- floor(n * perc_outlier)
      outlier_idx <- sample(seq_along(vec), n_outliers, replace = FALSE)
      spread <- sd(vec, na.rm = TRUE) * ifelse(difficulty == "easy", 10, 3)
      vec_scrambled[outlier_idx] <- mean(vec, na.rm = TRUE) + rnorm(n_outliers, spread, spread / 2)
    }
    
    # Introduce missing values
    n_missing <- floor(n * perc_missing)
    missing_idx <- sample(seq_along(vec), n_missing, replace = FALSE)
    vec_scrambled[missing_idx] <- NA
    
    return(vec_scrambled)
  }
  
  # Character scrambler
  scramble_char <- function(vec, perc_char, perc_missing) {
    n <- length(vec)
    vec_scrambled <- as.character(vec)
    noise <- c( "@",  "&",  ",")
    
    # Character corruption
    n_char_noise <- floor(n * perc_char)
    noise_idx <- sample(seq_along(vec), n_char_noise, replace = FALSE)
    vec_scrambled[noise_idx] <- paste0(sample(noise, n_char_noise, replace = TRUE), vec_scrambled[noise_idx])
    
    # Add/remove random spaces
    space_idx <- sample(seq_along(vec), floor(n * perc_char / 2), replace = FALSE)
    vec_scrambled[space_idx] <- gsub(" ", "", vec_scrambled[space_idx])
    vec_scrambled[space_idx] <- paste0(vec_scrambled[space_idx], " ")
    
    # Introduce missing values
    n_missing <- floor(n * perc_missing)
    missing_idx <- sample(seq_along(vec), n_missing, replace = FALSE)
    vec_scrambled[missing_idx] <- NA
    
    return(vec_scrambled)
  }
  
  # Initialize output dataframe
  df_scrambled <- df
  
  for (col in names(df)) {
    if (is.numeric(df[[col]]) || is.integer(df[[col]])) {
      df_scrambled[[col]] <- scramble_numeric(df[[col]], perc_outlier, perc_missing)
    } else if (is.character(df[[col]]) || is.factor(df[[col]])) {
      df_scrambled[[col]] <- scramble_char(df[[col]], perc_char, perc_missing)
    } else {
      df_scrambled[[col]] <- df[[col]]  # leave untouched
    }
  }
  
  return(df_scrambled)
}
salaries<-scramble(salaries, "easy") 

library(readxl)
library(openxlsx)

write.xlsx(salaries, file=here("data/salary_data_scrambled.xlsx"))

salaries<-read_excel(path=here("data/salary_data_scrambled.xlsx"))
# Names
# change the names of the variables to be meaningful, lower-case, with *no* white_space
# Use underscores to represent white_space if necessary
# variables should be short, distinct but meaningful. 
# use the name() function, the rename() function in dplyr()

library(janitor)
names(salaries)


salaries %>% 
  rename(time=1, age=2, industry=3,  salary=4, currency=5,
         country=6, state=7, experience=8, education=9, gender=10, race=11) ->salaries

# Working from left to right let's go through variables one by one. 
salaries$time

# Use the lubridate package to convert this to a date YYYY-MM-DD
class(salaries$time)
library(lubridate)
salaries$date<-as_date(salaries$time)
names(salaries)

class(salaries$age)
factor(salaries$age)
# Convert the age variable to a factor
# Clean any categories
salaries$age<-factor(salaries$age)
salaries %>% 
  mutate(age_cleaned=str_remove_all(age, ",|@|&"))->salaries
table(salaries$age_cleaned)
# filter the dataset to exclude anyone under 18
# Use the car::Recode() or the case_when() command to recode any spelling mistakes to correct them. 
salaries %>% 
  mutate(age_cleaned=car::Recode(age_cleaned, "'65orover'='65 or over'"))->salaries
table(salaries$age_cleaned)
salaries %>% 
  filter(age_cleaned!="under 18")->salaries
salaries %>% 
  filter(age_cleaned!="65 or over")->salaries
# industry

names(salaries)
table(salaries$industry)
salaries$industry<-factor(salaries$industry)
salaries$industry_cleaned
salaries %>% 
  mutate(industry_cleaned=str_remove_all(salaries$industry, "^&"))->salaries
names(salaries)
#Salary
class(salaries$salary)
view(salaries$salary)
salaries %>% 
  mutate(salary_cleaned=str_remove_all(salaries$salary, ",|&|@"))->salaries
#
salaries$salary_cleaned<-as.numeric(salaries$salary_cleaned)
names(salaries)
factor(salaries$country)
# 
salaries %>% 
  mutate(country=car::Recode(country,"'@UK'='UK';
                        'US'='USA'; 'usa'='USA'; 'US'='USA';
                             ',United States'='USA'; 'United States'='USA'"))->salaries
factor(salaries$country)
names(salaries)
salaries$gender
factor(salaries$gender)
salaries %>% 
  mutate(gender=str_remove_all(gender, "@|&"))->salaries
names(salaries)

