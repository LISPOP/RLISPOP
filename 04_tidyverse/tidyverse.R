## ------------------------------------------------------------------------------------------------------
#| label: output-lines
#| echo: false
library(knitr)
hook_output <- knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
  lines <- options$output.lines
  if (is.null(lines)) {
    return(hook_output(x, options))  # pass to default hook
  }
  x <- unlist(strsplit(x, "\n"))
  more <- "..."
  if (length(lines)==1) {        # first n lines
    if (length(x) > lines) {
      # truncate the output, but add ....
      x <- c(head(x, lines), more)
    }
  } else {
    x <- c(more, x[lines], more)
  }
  # paste these lines together
  x <- paste(c(x, ""), collapse = "\n")
  hook_output(x, options)
})


## ------------------------------------------------------------------------------------------------------
#| label: load-libraries
#| output.lines: 10
library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)


#alternatively you can load the whole tidyverse package
#All of the packages above are loaded automatically with the `tidyverse`
library(tidyverse)
#These are some other ones. 
library(here)
library(haven)
library(labelled)


## ------------------------------------------------------------------------------------------------------
#| label: data-import

shs<-read_sav(here("../course_data/shs.sav"))



## ------------------------------------------------------------------------------------------------------
#| label: glimpse
#| eval: true
#| echo: false
#| output.lines: 19
glimpse(shs)


## ------------------------------------------------------------------------------------------------------
#| label: look-for
look_for(shs, "income") 
lookfor(shs, "income")


## ------------------------------------------------------------------------------------------------------
#| label: select-pick 
shs %>% 
  select(TX001, HH_TotInc)


## ------------------------------------------------------------------------------------------------------
#| label: select-numbers
shs %>% 
  select(1:10)


## ------------------------------------------------------------------------------------------------------
#| label: select-numbers-10
shs %>% 
  #Select the 1st, 5th and 7th variable
  select(c(1,5,7))


## ------------------------------------------------------------------------------------------------------
#| label: tr
shs %>% 
  select(starts_with('TR'))


## ------------------------------------------------------------------------------------------------------
#| label: -C
shs %>% 
  select(ends_with('_C'))


## ------------------------------------------------------------------------------------------------------
#| label: contains
shs %>% 
  select(contains('TR'))


## ------------------------------------------------------------------------------------------------------
#| label: select-range
#| output.lines: 30
names(shs)
shs %>% select(CaseID:InternetYN)


## ------------------------------------------------------------------------------------------------------
shs %>% 
  select(1, contains('TR'))


## ------------------------------------------------------------------------------------------------------
#| label: select-answer
#| echo: false

shs %>% 
select(contains("HH"), YearBuilt,DwelTyp, Prov, HHSize, HHType6, RP_Educ, TX001)->shs_subset


## ------------------------------------------------------------------------------------------------------
#| label: rename-answer

shs_subset%>% 
  rename(`Total Income`=HH_TotInc, `Earned Income`=HH_EarnInc, `Household Size`=HHSize, `Household Type`=HHType6, Education=`RP_Educ`, `Income Taxes`=TX001)->shs_subset


## ----names-glimpse-------------------------------------------------------------------------------------
names(shs_subset)
glimpse(shs_subset)



## ----as-factor-----------------------------------------------------------------------------------------
shs_subset<-as_factor(shs_subset)



## ----workers-------------------------------------------------------------------------------------------
shs_subset %>% 
  filter(., `Earned Income`> 0)-> earned_income



## ----nrow----------------------------------------------------------------------------------------------
nrow(earned_income)
nrow(shs_subset)


## ----range---------------------------------------------------------------------------------------------
shs_subset %>% 
  filter(`Total Income`> 10000& `Total Income` <15000)


## ----average-------------------------------------------------------------------------------------------
shs_subset %>% 
  filter(`Total Income` > mean(shs_subset$`Total Income`))


## ------------------------------------------------------------------------------------------------------
#| label: levels-factors
#| output.lines: 10 


#Check the levels
levels(shs_subset$`Household Size`)




## ------------------------------------------------------------------------------------------------------
#| label: print-household-size
#| output.lines: 25

  #Actually print the variable
shs_subset$`Household Size`


## ----show-error, error=TRUE----------------------------------------------------------------------------
try({
shs_subset %>% 
filter(., `Household Size`>1)
})


## ----equal---------------------------------------------------------------------------------------------
shs_subset %>% 
  filter(., `Household Size`=="4 or more")


## ----equal-or------------------------------------------------------------------------------------------
shs_subset %>% 
  filter(., `Household Size`=="4 or more" | `Household Size`==1)


## ------------------------------------------------------------------------------------------------------
#| label: make-missing-data
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


## ------------------------------------------------------------------------------------------------------
#| label: is-na
#| output.lines: 10
is.na(shs_subset$`Earned Income`)


## ----filter-is-na--------------------------------------------------------------------------------------
shs_subset %>% 
  filter(is.na(`Earned Income`)) 


## ----flip-filter-is-na---------------------------------------------------------------------------------
shs_subset %>% 
  filter(!is.na(`Earned Income`))->shs_subset_no_missing
  nrow(shs_subset)
  nrow(shs_subset_no_missing)


## ----filter-strings,output.lines: 20-------------------------------------------------------------------

shs_subset %>% 
  filter(str_detect(`Household Type`, "children"))



## ----filter-strings2-----------------------------------------------------------------------------------
#| output.lines: 20
shs_subset %>% 
  filter(str_detect(`Household Type`, "with children"))


## ----invert-filter-strings-----------------------------------------------------------------------------
#| output.lines: 20
shs_subset %>% 
  filter(!str_detect(`Household Type`, "with children"))



## ------------------------------------------------------------------------------------------------------
#| label: fig-regex
#| echo: false
include_graphics(path=here("images/regular_expressions.png"))


## ------------------------------------------------------------------------------------------------------
#| label: regex-caret
#| output.lines: 20
#Search the string children anywhere
shs_subset %>% 
  filter(str_detect(`Household Type`, "children"))



## ------------------------------------------------------------------------------------------------------
#| label: regex-no-caret
#| output.lines: 20
#Search the string children anywhere
shs_subset %>% 
  filter(str_detect(`Household Type`, "children"))



## ------------------------------------------------------------------------------------------------------
#| label: regex-question
#| output.lines: 20
#Search the string children anywhere
shs_subset %>% 
  filter(str_detect(`Household Type`, "children"))



## ------------------------------------------------------------------------------------------------------
#| label: regex-question-end
#| output.lines: 20
#Search for the pattern with children at the end 
shs_subset %>% 
  filter(str_detect(`Household Type`, "children?"))


## ------------------------------------------------------------------------------------------------------
#| label: regex-wildcard-space
#| output.lines: 20
#This will return 
shs_subset %>% 
  filter(str_detect(`Household Type`, ". children"))


## ------------------------------------------------------------------------------------------------------
#| label: regex-wildcard
#| output.lines: 20
#This will return 
shs_subset %>% 
  filter(str_detect(`Household Type`, "Couple.children"))


## ------------------------------------------------------------------------------------------------------
#| label: regex-wildcard-3
#| output.lines: 20
#This will return 
shs_subset %>% 
  filter(str_detect(`Household Type`, "Couple.+children"))



## ------------------------------------------------------------------------------------------------------
#| label: regex-pipe
#| output.lines: 20
#This will return 
shs_subset %>% 
  filter(str_detect(`Household Type`, "with|without"))



## ------------------------------------------------------------------------------------------------------
#| label: inspect-dwel-type
#| output.lines: 20 
shs_subset$DwelTyp


## ----slice---------------------------------------------------------------------------------------------
shs_subset %>% 
  slice(10:100)


## ------------------------------------------------------------------------------------------------------
#| label: glimpse-for-grouping
#glimpse
glimpse(shs_subset)


## ------------------------------------------------------------------------------------------------------
#| label: groups
shs_subset %>% 
  group_by(Prov) 


## ------------------------------------------------------------------------------------------------------
#| label: fig-groups
include_graphics("images/groups.png")


## ------------------------------------------------------------------------------------------------------
#| label: summary-groups
#| 
shs_subset %>% 
  group_by(Prov) %>% 
  summarise(mean(`Total Income`, na.rm=T))->income_prov
income_prov


## ------------------------------------------------------------------------------------------------------
#| label: set-summary-variable-name
shs_subset %>% 
  group_by(Prov) %>%
  #insert a variable name here. 
  summarise(average=mean(`Total Income`, na.rm=T))->income_prov
income_prov


## ------------------------------------------------------------------------------------------------------
#| label: multiple-summarise
shs_subset %>% 
  group_by(Prov) %>%
  #insert a variable name here. 
  summarise(mean=mean(`Total Income`, na.rm=T),
            median=median(`Total Income`, na.rm=T), 
            SD=sd(`Total Income`, na.rm=T),
            max=max(`Total Income`, na.rm=T))->income_prov
income_prov


## ------------------------------------------------------------------------------------------------------
#| label: group-prov-dwelling

shs_subset %>% 
  group_by(Prov, DwelTyp) %>% 
  summarise(avg=mean(`Total Income`, na.rm=T))



## ----load-car------------------------------------------------------------------------------------------
#Uncomment if you need to install
#install.packages('car')
library(car)


## ------------------------------------------------------------------------------------------------------
#| label: glimpse-mutate
glimpse(shs_subset)


## ----check-levels--------------------------------------------------------------------------------------
#Check levels
levels(shs_subset$Prov)

#Or use table()
table(shs_subset$Prov)


## ------------------------------------------------------------------------------------------------------
#| label: recode
shs_subset %>% 
mutate(Region=Recode(Prov, "'Alberta'='West' ; 
                     'Saskatchewan'='West'; 
                     'British Columbia'='West' ;
                     'Manitoba'='West';
                     'Territorial Capitals'='North'"))->shs_subset
table(shs_subset$Region)



## ----summary_income------------------------------------------------------------------------------------
summary(shs_subset$`Total Income`)


## ----income--------------------------------------------------------------------------------------------

shs_subset %>% 
  mutate(Income2=Recode(`Total Income`, "1:50000='Less than $50K';
                        50001:100000='$50K to $100K';
                        100001=150000='$101Kto $150K';
                        15100:571250='> $150K'", as.factor=T))->shs_subset




## ------------------------------------------------------------------------------------------------------
#| label: inc2-levels
#| output.lines: 30
glimpse(shs_subset)
levels(shs_subset$Income2)



## ----help-revel----------------------------------------------------------------------------------------
?fct_relevel


## ----show-levels---------------------------------------------------------------------------------------
levels(shs_subset$Income2)


## ----relevel-------------------------------------------------------------------------------------------

shs_subset %>% 
  mutate(Income3=fct_relevel(Income2, "Less than $50K", "$50K to $100K", "> $150K"))->shs_subset

levels(shs_subset$Income2)
levels(shs_subset$Income3)



## ----case-when-----------------------------------------------------------------------------------------
shs_subset %>% 
  mutate(Income4=case_when(
    `Total Income` < 50001 ~ "Less than $50K",
    `Total Income` >50000 ~ "More than $50K"
  ))


## ----check-levels-education----------------------------------------------------------------------------
levels(shs_subset$Education)


## ----case-when-two-variables---------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------
#| label: view-class
#| eval: false
# shs_subset %>%
#   select(Income3, Education, Class) %>%
#   View()


## ------------------------------------------------------------------------------------------------------
#| label: case-when-regex

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


