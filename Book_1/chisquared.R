# load packages
library(tidyverse)
library(foreign)

# importing data from a SPSS sav file: 'asthmagest.sav' (CB1, Activity 2.5)
df <- read.spss('O:/M249/SPSS/Data/Book1/asthmagest.sav', use.value.labels = TRUE, to.data.frame = TRUE)

# convert the dataframe to a contingency table
table <- xtabs(df$count ~ df$exposure + df$disease)

# do the chi-squared test
chisq.test(table)