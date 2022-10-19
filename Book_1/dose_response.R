# load packages
library(tidyverse)
library(foreign)
library(DescTools)  # for the Tarone test (a modification of the Breslow-Day test) and CochranArmitageTest
library(vcdExtra)   # for CMHtest
library(coin)       # fot lbl_test

# import data from SPSS (CB1, Activity 4.1)
df.sav <- read.spss('O:/M249/SPSS/Data/Book1/smoking2.sav', use.value.labels = TRUE, to.data.frame = TRUE)
df.sav$dose <- ordered(factor(df.sav$dose))

# Create contingency table
table.sav <- xtabs(count ~ ., df.sav)             # use "table.sav <- " to asign to object

# Pearson chi-squared test of association
chisq.test(table.sav)

# tests of linear trend
CMHtest(table.sav, types = "cor")
lbl_test(table.sav)
CochranArmitageTest(table.sav)