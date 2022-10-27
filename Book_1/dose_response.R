# load packages
library(tidyverse)
library(foreign)
library(DescTools)  # for the Tarone test (a modification of the Breslow-Day test) and CochranArmitageTest
library(vcdExtra)   # for CMHtest
library(coin)       # for lbl_test (redundant if test if linear association with dose levels is valid)

# import data from SPSS (CB1, Activity 4.1)
df.sav <- read.spss('O:/M249/SPSS/Data/Book1/smoking2.sav', use.value.labels = TRUE, to.data.frame = TRUE)

# Create contingency table
table.sav <- xtabs(count ~ ., df.sav)             # use "table.sav <- " to asign to object
strata.df <- as.data.frame(table.sav, stringsAsFactors = FALSE)
strata <- unique(as.numeric(strata.df$dose))

# Pearson chi-squared test of association (Pearson Chi-Square in SPSS)
chi.stat <- unname(chisq.test(table.sav)$statistic)
chi.df <- unname(chisq.test(table.sav)$parameter)
chi.p <- unname(chisq.test(table.sav)$p.value)

# test of linear trend (Linear-byLinear Association in SPSS)
lin_ass.stat <- CMHtest(table.sav, rscores = strata, types = "cor")$table[1]
lin_ass.df <- CMHtest(table.sav, rscores = strata, types = "cor")$table[2]
lin_ass.p <- CMHtest(table.sav, rscores = strata, types = "cor")$table[3]

# lbl_test(table.sav)
# CochranArmitageTest(table.sav)
