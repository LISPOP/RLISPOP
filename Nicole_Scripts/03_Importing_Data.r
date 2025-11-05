#General
library(tidyverse)
#For file paths
library(here)
#For cleaning names 
library(janitor)
#For webscraping
library(rvest)

#For bulk csv and excel files
# If you don't have devtools installed yet, uncomment and run the line below
install.packages("devtools")
library(skpersonal)

library(here)
library(skpersonal)
#For reading excel files
library(readxl)
#For importing SPSS data
library(haven)
library(labelled)

#install.packages('here')
library(here)

list.files(here("../course_data/parking"))

df_csv <- read.csv(file=here("../course_data/parking/Parking_Tags_Data_2024_1.csv"))

#Check to see the files are there
combined_data <- bulk_csv(folder=here('../course_data/parking'))

#download excel file to subfolder
#read excel
df_xl <- read_excel("../course_data/StatisticsSummary.xls", sheet=1)
View(df_xl)

# the names() function lists the column names of the dataframe
names(df_xl)
head(df_xl$`Other Cases`)
#bulk_excel

#one excel workbook with multiple worksheets
dat<-read_excel(here("../course_data/cudo.xlsx"), sheet=3)
file_path<-here("data/cudo.xlsx")
sheet_names<-excel_sheets(file_path)
mydat<-map(sheet_names, read_excel, path=file_path)

mydat2<-map(sheet_names, read_excel, path=file_path, skip=6, n_max=26)
class(list)
18*26
mydat3<-bind_rows(mydat2)
nrow(mydat3)
?bind_rows
mydat3<-bind_rows(mydat2, .id="Year")

#SPSS Files
list.files(path=here('data'))
library(haven)
library(labelled)
shs<-read_sav(here('data/shs.sav'))
lookfor(shs, 'internet')
View(var_label(shs))

shs %>% 
  select(Prov, HHType6, HH_TotInc) %>% head()

#Printing first ten
shs$HHType6[1:10]
table(shs$Prov)
table(as_factor(shs$Prov))
#More permanent solution
table(as_factor(shs$HHType6))

library(car)
val_labels(shs$HHType6)
class(shs$HHType6)

library(haven)

shs %>% 
  mutate(household=case_when(
    as.numeric(as.character(HHType6))==1~'Single',
    as.numeric(as.character(HHType6)) >1~'Multiple'
  )) ->shs
table(shs$household)

#Scraping data from tables
news<-'https://en.wikipedia.org/wiki/List_of_newspapers_in_Canada_by_circulation'
#Read in the html
news_table<-read_html(news)
#Checking to see what you have
glimpse(news_table)
html_nodes(news_table, "table")
class(news_table)
html_nodes(news_table, "table") %>%
  .[1]
html_nodes(news_table, "table") %>% 
  .[3]
html_nodes(news_table, "table") %>% 
  .[3] %>% 
  html_table()->newspapers

#Turn to data.frame
newspapers<-data.frame(newspapers)
glimpse(newspapers)
