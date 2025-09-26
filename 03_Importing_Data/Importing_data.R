## ---------------------------------------------------------------------------------------------
#General
library(tidyverse)
#For file paths
library(here)
#For cleaning names 
library(janitor)
#For webscraping
library(rvest)
#For bulk csv and excel files
library(muckrakr)
#For reading excel files
library(readxl)
#For importing SPSS data
library(haven)
library(labelled)


## ----img1, echo = F, out.width="100%"---------------------------------------------------------
library(knitr)
include_graphics("images/csv1.png")


## ----img2, echo = F, out.width="100%"---------------------------------------------------------
include_graphics("images/csv2.png")


## ----img3, echo = F, out.width="100%"---------------------------------------------------------
include_graphics("images/rightclick.png")


## ----segment1---------------------------------------------------------------------------------
df_csv <- read.csv("https://data.ct.gov/api/views/iyru-82zq/rows.csv?accessType=DOWNLOAD")
head(df_csv, 10)


## ----getwd, echo=T----------------------------------------------------------------------------
getwd()


## ----install----------------------------------------------------------------------------------
#install.packages('here')
library(here)


## ----run-here---------------------------------------------------------------------------------
here()


## ----here-data--------------------------------------------------------------------------------
here("data")


## ----segment2---------------------------------------------------------------------------------
df_csv <- read.csv(here("data", "Admissions_to_DMHAS_Addiction_Treatment_by_Town__Year__and_Month.csv"), stringsAsFactors=F)


## ----img_excel, echo = F, out.width="100%"----------------------------------------------------
library(knitr)
include_graphics("images/excel1.png")


## ----first_sheet, eval=F----------------------------------------------------------------------
## df_xl <- read_excel("data/StatisticsSummary.xls", sheet=1)


## ----view1, eval=F----------------------------------------------------------------------------
## View(df_xl)


## ----img5, echo = F, out.width="100%"---------------------------------------------------------
include_graphics("images/excel_imported.png")


## ----skip-------------------------------------------------------------------------------------
df_xl <- read_excel("data/StatisticsSummary.xls", sheet=1, skip=2)


## ----view2, eval=F----------------------------------------------------------------------------
## View(df_xl)


## ----img6, echo = F, out.width="100%"---------------------------------------------------------
include_graphics("images/excel_imported2.png")


## ----df_xl------------------------------------------------------------------------------------
# the names() function lists the column names of the dataframe
names(df_xl)


## ----df_xl_col_fail, error=T------------------------------------------------------------------
head(df_xl$Other Cases)


## ----df_xl_col_pass, error=T------------------------------------------------------------------
head(df_xl$`Other Cases`)


## ----next-------------------------------------------------------------------------------------
library(janitor)
df_xl <-clean_names(df_xl)


## ----check-names------------------------------------------------------------------------------
names(df_xl)


## ----rename2, warning=F, message=F------------------------------------------------------------
## If you don't have dplyr installed yet, uncomment the line below and run it
# install.packages("dplyr")

library(dplyr)
df_xl <- rename(df_xl, Year=fiscal_year_7_1_6_30)


## ----names_again------------------------------------------------------------------------------
names(df_xl)


## ----rename4, warning=F, message=F------------------------------------------------------------

df_xl <- rename(df_xl, Examinations=4)


## ----check-names2-----------------------------------------------------------------------------
df_xl


## ----install_muckrakr, warning=F, message=F---------------------------------------------------
# If you don't have devtools installed yet, uncomment and run the line below
#install.packages("devtools")
#devtools::install_github("andrewbtran/muckrakr")
library(muckrakr)
library(here)


## ----make-url---------------------------------------------------------------------------------
news<-'https://en.wikipedia.org/wiki/List_of_newspapers_in_Canada_by_circulation'


## ----getnews----------------------------------------------------------------------------------
#Read in the html
news_table<-read_html(news)
#Check to see what you have
glimpse(news_table)


## ----newspapers-------------------------------------------------------------------------------

html_nodes(news_table, "table")

  



## ----check-class-nodeset----------------------------------------------------------------------
class(news_table)


## ----check-node1------------------------------------------------------------------------------

html_nodes(news_table, "table") %>% 
  .[1]


## ----check-node6------------------------------------------------------------------------------

html_nodes(news_table, "table") %>% 
  .[3]


## ----clean-html-------------------------------------------------------------------------------
html_nodes(news_table, "table") %>% 
  .[3] %>% 
  html_table()->newspapers


## ----data-frame-------------------------------------------------------------------------------
#Turn to data.frame
newspapers<-data.frame(newspapers)



## ----summary-newspapers-----------------------------------------------------------------------
glimpse(newspapers)



## ----household-spending-----------------------------------------------------------------------
list.files(path=here('data'))



## ----load-haven-------------------------------------------------------------------------------
library(haven)
library(labelled)
shs<-read_sav(here('data/shs.sav'))


## ----look-for---------------------------------------------------------------------------------
lookfor(shs, 'internet')
lookfor(shs, 'income')
lookfor(shs, 'rent')



## ----view, results='hide', eval=F-------------------------------------------------------------
## View(var_label(shs))
## 


## ----example, eval=F--------------------------------------------------------------------------
## bulk_csv(folder = "DEFAULTBULKCSV2017", export = "filenamedefaultbulkcsv2018.csv"")


## ----csvs, echo = F, out.width="100%"---------------------------------------------------------
library(knitr)
include_graphics("images/csvs.png")


## ----bulk_csv, message=FALSE, warning=FALSE---------------------------------------------------
library(here)
here()
list.files('data/csvs')
combined_data <- bulk_csv(folder=here('/data/csvs'))



## ----excel1, echo=F---------------------------------------------------------------------------
include_graphics(here("images/multiple_worksheets.png"))


## ----read-sheet-------------------------------------------------------------------------------

dat<-read_excel(here("data/cudo.xlsx"), sheet=3)


## ----make-path--------------------------------------------------------------------------------
file_path<-here("data/cudo.xlsx")


## ----sheet-names------------------------------------------------------------------------------
sheet_names<-excel_sheets(file_path)


## ----map--------------------------------------------------------------------------------------
mydat<-map(sheet_names, read_excel, path=file_path)


## ----excel-mydat, echo=F----------------------------------------------------------------------
include_graphics(here('images/mydat.png'))


## ----help-------------------------------------------------------------------------------------
?read_excel


## ----multiple-worksheets, echo=F--------------------------------------------------------------
include_graphics(here("images/multiple_worksheets.png")) 


## ----skip2------------------------------------------------------------------------------------
mydat2<-map(sheet_names, read_excel, path=file_path, skip=6, n_max=26)



## ----list-------------------------------------------------------------------------------------
class(list)


## ----math-------------------------------------------------------------------------------------
18*26


## ----bind-rows--------------------------------------------------------------------------------
mydat3<-bind_rows(mydat2)


## ----nrow-mydat-------------------------------------------------------------------------------
nrow(mydat3)


## ----help-bind-rows---------------------------------------------------------------------------
?bind_rows


## ----id-year----------------------------------------------------------------------------------
mydat3<-bind_rows(mydat2, .id="Year")



## ----run-summary------------------------------------------------------------------------------
summary(mydat3)



## ----percent----------------------------------------------------------------------------------
mydat3$`Retention Rate`


## ----get-tweets, eval=F-----------------------------------------------------------------------
## library(rtweet)
## laurier <- search_tweets("laurier", n = 100, include_rts = FALSE)
## laurier


## ----get-spottedn, eval=F---------------------------------------------------------------------
## #Get timelline of a twitter account
## spotted<-get_timeline(user="spottedlaurier")
## #Check
## glimpse(spotted)
## 


## ----read-text, eval=F------------------------------------------------------------------------
##   users <- search_users("r")
##   tweets_data(users)

