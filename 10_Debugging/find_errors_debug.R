# import file shs.sav in the data subfolder using `read_sav`
shs<-read_sav(file=here("data/shs.sav"))
#glimpse
glimpse(shs)
#convert any labelled variables to factor
shs<-as_factor(shs)
#glimpse()
glimpse(shs)

#make new variable called `West` which has 1 for western provinces, 0 for non-Western 
shs %>% 
  mutate(West=case_when(
    Prov=="Alberta"~1,
    Prov=="Saskatchewan"~1,
    Prov="Manitoba"~1,
    Prov=="British Columbia"~1,
    Prov=="Ontario"~0,
    Prov=="Quebec"~0,
    Prov=="Atlantic provinces"~0
  ))->shs
#The var_label() command from labelled prints the variable labels of a data frame.

shs %>% 
  map(., var_label)
#the lookfor command from labelled
# searches through variables for keywords
lookfor(shs, "Tenure")
#Select a subset of variables
shs %>% 
  select(HH_TotInc, RP_Sex, SH001,Tenure)->shs
#Rename variables for ease of use
shs %>% 
  rename(HH_TotInc=Income,Sex=RP_Sex, Shelter_Costs=SH001)
#Correlate two variables for analysis
with(shs, cor(Income,Sex))



