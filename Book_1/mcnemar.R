# load packages
library(tidyverse)
library(foreign)
# library(DescTools)  # for the Tarone test (a modification of the Breslow-Day test)

# import data from SPSS (CB1, Activity 4.1)
df.sav <- read.spss('O:/M249/SPSS/Data/Book1/alzheimers.sav', use.value.labels = TRUE, to.data.frame = TRUE)

# Create contingency table
table.sav <- xtabs(df.sav$count ~ ., df.sav)

# McNemar's test
mcnemar.test(table.sav)