library(tidyverse)
set.seed(123)
var1<-round(rnorm(10, mean=10, sd=4),0)
error<-round(rnorm(10, mean=10, sd=8),0)
var2<-round(2+2*var1+error,1)
df<-data.frame(var1, var2)
df

df %>% 
  ggplot(., aes(x=var1, y=var2))+geom_point()
