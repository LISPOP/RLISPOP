library(tidyverse)
library(here)
library(haven)

shs<-read_sav(file=here("../course_data/shs.sav"))

shs %>% 
  #Select income, taxes, dwelling type and housing tenure
  select(Income=HH_TotInc, Taxes=TX001,Dwelling=DwelTyp, Tenure=Tenure, Province=Prov)->shs_subset
#glimpse
glimpse(shs_subset)

shs_subset<-as_factor(shs_subset)
#Compare now
glimpse(shs_subset)

ggplot(data=dat, aes(x=x, y=y))
dat %>% 
  ggplot(., data=., aes(x=x, y=y))

dat %>% 
  ggplot(., aes(x=x, y=y))

dat %>% 
  ggplot(., aes(x=x, y=y))+geom_point()

#Histogram
ggplot(shs_subset, aes(x=Income))+geom_histogram()

shs_subset%>% 
  ggplot(., aes(x=Income))+geom_histogram()

#Save plots to modify later
shs_subset%>% 
  ggplot(., aes(x=Income))+geom_histogram()->histogram1

#Now print. 
histogram1

#Density Plot
shs_subset%>% 
  ggplot(., aes(x=Income))+geom_density()

#Check the names
names(shs_subset)
#Print the values of Board Type just to check
shs_subset$Dwelling

ggplot(shs_subset ,aes(x=Dwelling))+geom_bar()

shs_subset %>% 
  group_by(Dwelling) %>% 
  summarize(count=n())

#Bar graph
shs_subset %>% 
  group_by(Dwelling) %>% 
  summarize(count=n()) %>% 
  ggplot(., aes(x=Dwelling, y=count))+geom_col()

#Bar graph of percents
shs_subset %>% 
  group_by(Dwelling) %>% 
  summarize(count=n())
mutate(percent=count/sum(count))
ggplot(., aes(x=Dwelling, y=percent))+geom_col()

#Boxplot
ggplot(shs_subset, aes(x=Income))+geom_boxplot()

#y- Variables
ggplot(shs_subset, aes(x=Province, y=Income))+geom_boxplot()

#Grouping
shs_subset %>% 
  group_by(Province) %>% 
  summarize(Average_Income=mean(Income))->average_incomes
#print
average_incomes

average_incomes %>% 
  ggplot(., aes(x=Province, y=Average_Income))+geom_col()

#Scatterplots
shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes))+geom_point()

shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes))+geom_point()+geom_smooth(method="lm")

shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes))+geom_point()+geom_smooth(method="loess")

#Aesthetics
ggplot(shs_subset, aes(x=Income, fill=Dwelling))+geom_density()
ggplot(shs_subset, aes(x=Income, fill=Dwelling, alpha=Dwelling))+geom_density()

ggplot(shs_subset, aes(x=Income, fill=Dwelling, alpha=Dwelling))+geom_histogram()

ggplot(shs_subset, aes(x=Dwelling, fill=Tenure))+geom_bar(position="fill")

#Facets
ggplot(shs_subset, aes(x=Income))+geom_histogram()+facet_wrap(vars(Dwelling))
ggplot(shs_subset, aes(x=Income))+geom_histogram()+facet_grid(vars(Dwelling))

ggplot(shs_subset, aes(x=Income))+geom_histogram()+facet_wrap(vars(Dwelling), nrow=2)
ggplot(shs_subset, aes(x=Income))+geom_histogram()+facet_grid(rows=vars(Dwelling), cols=vars(`Tenure`), scales='free')

#Pivot with facets
names(shs_subset)

shs_subset %>% 
  pivot_longer(., cols=c(Income, Taxes), names_to=c('Measure'), values_to=c("Dollars")) %>% 
  glimpse()
shs_subset %>% 
  pivot_longer(., cols=c(Income, Taxes), names_to=c('Measure'), values_to=c("Dollars"))%>% 
  ggplot(., aes(x=Dollars))+geom_histogram()+facet_wrap(vars(Measure))

shs_subset %>% 
  pivot_longer(., cols=c(Income, Taxes), names_to=c('Measure'), values_to=c("Dollars")) %>% 
  ggplot(., aes(x=Dollars))+geom_histogram()+facet_wrap(vars(Measure, Province), nrow=4)

#Ridgeline or Joy Plots
#install.packages('ggridges')
library(ggridges)
shs_subset %>% 
  ggplot(., aes(x=Income, y=Province, fill=Dwelling ))+geom_density_ridges()

#Reducing Categories and Changing Text
average_incomes %>% 
  ggplot(., aes(x=Province, y=Average_Income))+geom_col()
average_incomes %>% 
  ggplot(., aes(y=Province, x=Average_Income))+geom_col()

library(car)
average_incomes %>% 
  mutate(Province=Recode(Province, "'Atlantic provinces'='Atlantic'"))->average_incomes
table(average_incomes$Province)
table(shs_subset$Province)

average_incomes %>% 
  filter(Province!="Territorial capitals") %>% 
  ggplot(., aes(y=Province, x=Average_Income))+geom_col()

shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes))+geom_point()

shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes))+geom_point(color="red")

shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes))+geom_point(color="red", size=5, shape=3)

shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes))+geom_point(color="red", fill="blue", shape=23)

shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes, col=Dwelling))+geom_point()
scale_col_discrete() scale_col_continuous() scale_fill_discrete() scale_fill_continuous scale_size_discrete() scale_size_continuous


shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes, col=Dwelling))+geom_point()+scale_color_discrete(type=c("darkgreen", "gold", "lightgreen"))
shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes, col=Dwelling))+geom_point()+geom_point()+scale_color_discrete(type=c("lightgreen", "gold" , "darkgreen"))

#Picking Palettes
#install if necessary
#install.packages('RColorBrewer')
library(RColorBrewer)
shs_subset %>% 
  ggplot(., aes(x=Income, y=Taxes, col=Dwelling))+geom_point()+scale_color_brewer(type="seq", palette="Greens")

#Factor Orders
average_incomes %>% 
  ggplot(., aes(y=Province, x=Average_Income))+geom_col()

average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()

average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income, .desc=T))+geom_col()

#Axis and Graph Labels
average_incomes %>% 
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")
average_incomes %>% 
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title=str_wrap("Average Total Household Income By Province", 30))

#Themes
Theme_bw()

average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme_bw()

average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme_light()

average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme_minimal()

#instal if necessary
#install.packages('ggthemes')
library(ggthemes)

average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme_economist()

average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme_wsj()

#Axis Text Font and Size
average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme(text=element_text(size=14, color="red", family="Times New Roman"))

average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme(text=element_text(family="Times New Roman"), plot.title=element_text(color="darkgreen"))
average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme(text=element_text(family="Times New Roman"), plot.title=element_text(color="darkgreen"), axis.text=element_text(size=5))
average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme(text=element_text(family="Times New Roman"), plot.title=element_text(color="darkgreen"), axis.text=element_text(size=5))
average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme(text=element_text(family="Times New Roman"), plot.title=element_text(color="darkgreen"), axis.text.y=element_text(size=5), axis.text.x=element_text(size=12, angle=45))
average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme(plot.title=element_text(color="darkgreen"), axis.text.y=element_text(size=5),panel.background=element_blank())
average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme(plot.title=element_text(color="darkgreen"), axis.text.y=element_text(size=5),panel.background=element_rect(fill='white'), panel.grid.major = element_line(colour="black"))
average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme(plot.title=element_text(color="darkgreen"), axis.text.y=element_text(size=5),panel.background=element_blank(), panel.grid.major.x = element_line(colour="darkgray", linetype=2, size=0.5), panel.ontop = T)

#Legend
average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  
#Customizing
  theme_mine<-function() {theme(panel.background=element_rect(fill="darkred"))}

average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme_mine()

theme_mine<-function() {
  theme(
    text=element_text(family="mono"),
    panel.background=element_blank(),
    panel.ontop = T,
    panel.grid.major.x = element_line(color="lightgray", linetype=2),
    plot.title=element_text(color="darkgreen"))}

average_incomes %>% 
  #fct_reorder reorders the levels ofthe factor variable by the values of the second numeric variable
  ggplot(., aes(y=fct_reorder(Province, Average_Income), x=Average_Income))+geom_col()+labs(x="Average Household Income", y="Province", title="Average Total Household Income By Province")+theme_mine()