library(tidyverse)
library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)

library(here)
library(haven)
library(labelled)

shs<-read_sav(here("../course_data/shs.sav")

look_for(shs, "income")
              
shs %>% 
  select(TX001, HH_TotInc)
#Selecting first ten variables
shs %>% 
  select(1:10)
#Select specific variables
shs %>% 
  #Select the 1st, 5th and 7th variable
  select(c(1,5,7))

shs %>% 
  select(starts_with('TR'))

shs %>% 
  select(ends_with('_C'))

shs %>% 
  select(contains('TR'))

names(shs)

shs %>% select(CaseID:InternetYN)
shs %>% 
  select(1, contains('TR'))

#Rename
shs_subset%>% 
  rename(`Total Income`=HH_TotInc, `Earned Income`=HH_EarnInc, `Household Size`=HHSize, `Household Type`=HHType6, Education=`RP_Educ`, `Income Taxes`=TX001)->shs_subset

#Filtering Rows
names(shs_subset)
glimpse(shs_subset)
shs_subset<-as_factor(shs_subset)

shs_subset %>% 
  filter(., `Earned Income`> 0)-> earned_income
nrow(earned_income)
nrow(shs_subset)
shs_subset %>% 
  filter(`Total Income`> 10000& `Total Income` <15000)

#Average Household Total Income
shs_subset %>% 
  filter(`Total Income` > mean(shs_subset$`Total Income`))

#Filtering on factors and characters
#Check the levels
levels(shs_subset$`Household Size`)

shs_subset %>% 
  filter(., `Household Size`>1)
shs_subset %>% 
  filter(., `Household Size`=="4 or more")
shs_subset %>% 
  filter(., `Household Size`=="4 or more" | `Household Size`==1)

#Filtering missing values

#Sprinkling missing values
#This makes a function called insert_missing
# Function to randomly insert NAs into a dataframe
insert_missing <- function(df, missing_frac = 0.1) {
  df_missing <- df
  total_cells <- prod(dim(df))
  num_missing <- round(total_cells * missing_frac)
  
  for (i in seq_len(num_missing)) {
    row <- sample(nrow(df), 1)
    col <- sample(ncol(df), 1)
    df_missing[row, col] <- NA
  }
  
  return(df_missing)
}
#This runs insert_missing on the dataset and saves it in shs_subset
shs_subset<-insert_missing(shs_subset, missing_frac=0.05)
#This runs summary()
summary(shs_subset)

is.na(shs_subset$`Earned Income`)
shs_subset %>% 
  filter(is.na(`Earned Income`))

#Save new df in new save to avoid overwriting original
#Compare rows
shs_subset %>% 
  filter(!is.na(`Earned Income`))->shs_subset_no_missing
nrow(shs_subset)
nrow(shs_subset_no_missing)

#Filtering using str_detect
shs_subset %>% 
  filter(str_detect(`Household Type`, "children"))
#Fixing filter
shs_subset %>% 
  filter(str_detect(`Household Type`, "with children"))

#Inverting
shs_subset %>% 
  filter(!str_detect(`Household Type`, "with children"))

#Search the string children anywhere
shs_subset %>% 
  filter(str_detect(`Household Type`, "children"))

#Search the string children anywhere
shs_subset %>% 
  filter(str_detect(`Household Type`, "children"))

#Search for the pattern with children at the end 
shs_subset %>% 
  filter(str_detect(`Household Type`, "children?"))


shs_subset %>% 
  filter(str_detect(`Household Type`, ". children"))

shs_subset %>% 
  filter(str_detect(`Household Type`, "Couple.children"))

shs_subset %>% 
  filter(str_detect(`Household Type`, "Couple.+children"))

shs_subset %>% 
  filter(str_detect(`Household Type`, "with|without"))

#Slicing as a form of filtering
shs_subset %>% 
  slice(10:100)

#Grouping and summarizing
#glimpse
glimpse(shs_subset)
shs_subset %>% 
  group_by(Prov)

include_graphics("images/groups.png")
shs_subset %>% 
  group_by(Prov) %>% 
  summarise(mean(`Total Income`, na.rm=T))->income_prov
income_prov

shs_subset %>% 
  group_by(Prov) %>%
  #insert a variable name here. 
  summarise(average=mean(`Total Income`, na.rm=T))->income_prov
income_prov

shs_subset %>% 
  group_by(Prov) %>%
  #insert a variable name here. 
  summarise(mean=mean(`Total Income`, na.rm=T),
            median=median(`Total Income`, na.rm=T), 
            SD=sd(`Total Income`, na.rm=T),
            max=max(`Total Income`, na.rm=T))->income_prov
income_prov

shs_subset %>% 
  group_by(Prov, DwelTyp) %>% 
  summarise(avg=mean(`Total Income`, na.rm=T))

#Uncomment if you need to install
#install.packages('car')
library(car)

#Recoding a categorical variable
glimpse(shs_subset)
#Check levels
levels(shs_subset$Prov)
#Or use table()
table(shs_subset$Prov)

shs_subset %>% 
  mutate(Region=Recode(Prov, "'Alberta'='West' ; 
                     'Saskatchewan'='West'; 
                     'British Columbia'='West' ;
                     'Manitoba'='West';
                     'Territorial Capitals'='North'"))->shs_subset
table(shs_subset$Region)

#Recoding a numeric value
summary(shs_subset$`Total Income`)
shs_subset %>% 
  mutate(Income2=Recode(`Total Income`, "1:50000='Less than $50K';
                        50001:100000='$50K to $100K';
                        100001=150000='$101Kto $150K';
                        15100:571250='> $150K'", as.factor=T))->shs_subset

glimpse(shs_subset)
levels(shs_subset$Income2)

?fct_relevel
levels(shs_subset$Income2)

levels(shs_subset$Income2)
levels(shs_subset$Income3)

#Alternative Recode
shs_subset %>% 
  mutate(Income4=case_when(
    `Total Income` < 50001 ~ "Less than $50K",
    `Total Income` >50000 ~ "More than $50K"
  ))

levels(shs_subset$Education)
shs_subset %>% 
  mutate(Class=case_when(
    #Poor and people with less than high school
    `Total Income`<50001 & Education=="Less than high school diploma or its equivalent"~ "poor low education",
    #poor and people with high school
    `Total Income`<50001 & Education=="High school diploma, high school equivalency certificate, or"~ "poor low education",
    #Rich and people with less than high school
    `Total Income`>50000 & Education=="Less than high school diploma or its equivalent"~ "rich low education",
    #Rich and people with high school
    `Total Income`>50000 & Education=="High school diploma, high school equivalency certificate, or"~ "poor low education",
    #Poor and people with diploma
    `Total Income`<50001 & Education=="Certificate or diploma from a trades school, college, CEGEP"~ "poor high education",
    #Poor and people with degree,
    `Total Income`<50001 & Education=="University certificate or diploma"~ "poor high education",
    #rich and people with diploma
    `Total Income`>5000 & Education=="Certificate or diploma from a trades school, college, CEGEP"~ "rich high education",
    #Rich and people with degree
    `Total Income`>50000 & Education=="University certificate or diploma"~ "poor high education"
  ))->shs_subset
shs_subset %>% 
  select(Income3, Education, Class) %>% 
  View()
names(shs_subset)

shs_subset %>% 
  mutate(Class=case_when(
    #Poor and people with less than high school
    `Total Income`<50001 & str_detect(Education, "Less than|^High school")~ "poor low education",
    #Poor and people with college or universityl
    `Total Income`<50001 & str_detect(Education, "^Certificate|^University")~ "poor high education",
    #Poor and people with less than high school or high school
    `Total Income`>50001 & str_detect(Education, "Less than|^High school")~ "rich low education",
    #Richand people with college or universityl
    `Total Income`>50001 & str_detect(Education, "^Certificate|^University")~ "poor high education"))->shs_subset

