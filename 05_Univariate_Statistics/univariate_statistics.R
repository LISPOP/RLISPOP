## -------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------
#| label: load-libraries 

library(haven)
library(here)
library(tidyverse)



## -------------------------------------------------------------------------------------
#| label: data-import
library(haven)
shs<-read_sav(here('..','course_data', 'shs.sav'))




## -------------------------------------------------------------------------------------
shs %>% 
  select(Income=HH_TotInc,Dwelling=DwelTyp, Taxes=TX001)->shs_subset
#Convert labelled variables to factors

shs_subset<-as_factor(shs_subset)


## -------------------------------------------------------------------------------------
#| label: mean
mean(shs_subset$Income)


## -------------------------------------------------------------------------------------
#| label: mean-na-rm
mean(shs_subset$Income, na.rm=T)


## -------------------------------------------------------------------------------------
#| label: save-mean
mean_income<-mean(shs_subset$Income)
mean_income


## -------------------------------------------------------------------------------------
#| label: median


median(shs_subset$Income)



## -------------------------------------------------------------------------------------
#| label: table

table(shs_subset$Dwelling)


## -------------------------------------------------------------------------------------
#| label: sd
sd(shs_subset$Income)

