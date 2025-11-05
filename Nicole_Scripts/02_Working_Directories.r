#Set the working directory
setwd("~/Documents")
#Check to see if it worked
getwd()

install.packages("here")
library(here)
here()

data <- read.csv(here("data", "test.csv"))
head(data)