# tidyverse for lots of stuff
library(tidyverse)
# here for getting working directories
library(here)
# haven for importing datasets
library(haven)
#labelled to look through large datasets
library(labelled)
#crosstable package for crosstabs
library(crosstable)

#import data
shs<-read_sav(here("../course_data/shs.sav"))
lookfor(shs, "province")

shs %>% 
  #Select income, taxes, dwelling type and housing tenure
  select(Income=HH_TotInc, Taxes=TX001,Dwelling=DwelTyp, Tenure=Tenure, Province=Prov)->shs_subset
#glimpse
glimpse(shs_subset)

shs_subset<-as_factor(shs_subset)
#Compare now
glimpse(shs_subset)

#Numeric
cor(shs_subset$Income, shs_subset$Taxes)

#Producing Histogram
shs_subset %>% 
  pivot_longer(Income:Taxes) %>% 
  ggplot(., aes(x=value))+geom_histogram()+facet_grid(~name)

cor(shs_subset$Income, shs_subset$Taxes, method=c("spearman"))

#Categorical
#Crosstabs
table(shs_subset$Dwelling, shs_subset$Tenure)

with(shs_subset, table(Tenure, Dwelling))

#Table 1
#Save the table
tab1<-with(shs_subset, table(Tenure, Dwelling))
#print
tab1

#Chi-squared test
chisq.test(tab1)

#Cramer's V
#install.packages("rcompanion")
library(rcompanion)
cramerV(tab1)

#Exporting tables
#install.packages('janitor')
library(janitor)

?tabyl
tabyl(shs_subset, Tenure)
tabyl(shs_subset, Tenure) %>% 
  adorn_pct_formatting()

#install.package('flextable')
library(flextable)
#Save the flextable as crosstab1
tabyl(shs_subset, Tenure) %>% 
  adorn_pct_formatting() %>% 
  as_flextable()->tabyl1
#Save crosstab1 as an html file
save_as_html(tabyl1, path=here("table1.html"))
# save as word document 
save_as_docx(tabyl1, path="table1.docx")

#Two Variable Crosstab
tabyl(shs_subset, Dwelling, Tenure)

tabyl(shs_subset, Dwelling, Tenure) %>% 
  adorn_percentages()
tabyl1<-tabyl(shs_subset, Dwelling, Tenure) %>% 
  adorn_percentages(denominator="col")

#print
tabyl1

tabyl1<-tabyl(shs_subset, Dwelling, Tenure) %>% 
  #Add the column percentages
  adorn_percentages(denominator="col") %>% 
  #Format the percentages neatly
  adorn_pct_formatting %>% 
  #Add the ns back in 
  adorn_ns()
#print 
tabyl1

#Adding row and column totals
tabyl1<-tabyl(shs_subset, Dwelling, Tenure) %>% 
  #Add the totals
  adorn_totals("both") %>% 
  #Add the column percentages
  adorn_percentages(denominator="col") %>% 
  #Format the percentages neatly
  adorn_pct_formatting() %>% 
  #Add the ns back in 
  adorn_ns() 
#print
tabyl1

#Three-way Table
tabyl2<-shs_subset %>% 
  #Just filter the provinces of Ontario and Quebec
  filter(Province=="Ontario"|Province=="Quebec") %>% 
  # Make the tabyl with Dwelling, Tenure and province
  tabyl(., Dwelling, Tenure, Province) %>% 
  #Add the percentages
  adorn_percentages() %>% 
  #format the percentages
  adorn_pct_formatting %>%
  #add the ns
  adorn_ns()
tabyl2

#install.packages('flextable')
library(flextable)
tabyl1<-tabyl1 %>% 
  #Convert to flextable
  as_flextable()
#save as html
save_as_html(tabyl1, path=here("tabyl1.html"))
#save as png
save_as_image(tabyl1, path=here("tabyl1.png"))

#save as powerpoint
save_as_pptx(tabyl1, path=here("tabyl1.pptx"))
#save as docx
save_as_docx(tabyl1, path=here("tabyl1.docx"))

#Categorical- Numeric
table(shs_subset$Tenure)

library(car)
shs_subset %>% 
  mutate(Tenure2=Recode(Tenure, "'Owned with mortgage'='Owned'; 'Owned without mortgage'='Owned'", levels=c("Owned", "Rented")))->shs_subset
#Check
table(shs_subset$Tenure2)

#T Test
#Dependent left, Independent right
t.test(Income~Tenure2, data=shs_subset)


#Exercises
#1
library(tidyverse)
library(here)
library(haven)
library(labelled)
library(crosstable)
library(rcompanion)
library(car)
library(janitor)
library(flextable)

shs <- read_sav(here("../course_data/shs.sav"))
lookfor(shs, "income")
lookfor(shs, "sex")
lookfor(shs, "tenure")

#2
shs_small <- shs %>%
  select(
    respondent_income = HH_RespInc,
    spousal_income = HH_SpouseInc,
    sex = Sex,
    housing_tenure = Tenure
  )

#3
shs_small <- as_factor(shs_small)
glimpse(shs_small)

#4
cor(shs_small$respondent_income, shs_small$spousal_income, use = "complete.obs")

#5
table(shs_small$sex, shs_small$housing_tenure)

#6
tab_sex_tenure <- table(shs_small$sex, shs_small$housing_tenure)
chisq.test(tab_sex_tenure)

#7
levels(shs_small$sex)
t.test(respondent_income ~ sex, data = shs_small)
