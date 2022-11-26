# load packages
library(tidyverse)
library(foreign)
library(DescTools)  # for the Tarone test (a modification of the Breslow-Day test)

# import data from SPSS (CB1, Activity 4.1)
df <- read.spss('O:/M249/SPSS/Data/Book1/diabetes.sav', use.value.labels = TRUE, to.data.frame = TRUE)

# Create contingency table
table <- xtabs(df$count ~ ., df)

# aggregate data across strata
a <- sum(unname(table[1,1,]))
b <- sum(unname(table[1,2,]))
c <- sum(unname(table[2,1,]))
d <- sum(unname(table[2,2,]))

# calculate unadjusted odds ratio (ci)
or.crude <- (a * d)/(b * c)
cl <- 0.95
sigma <- sqrt((1/a)+(1/b)+(1/c)+(1/d))
z <- qnorm((1 - ((1 - cl)/2)), mean =  0, sd = 1)
or.crude.cilb <- or.crude * exp(-z * sigma)
or.crude.ciub <- or.crude * exp(z * sigma)

# Mantel-Haenszel common OR estimate
mh <- mantelhaen.test(table, correct = TRUE, conf.level = cl)

mh.chi2.stat <- unname(mh$statistic)
mh.chi2.p <- mh$p.value
mh.cor <- unname(mh$estimate)
mh.cor.cilb <- mh$conf.int[1]
mh.cor.ciub <- mh$conf.int[2]
mh.null <- unname(mh$null.value)

# Tarone test of homogeneity of OR for all strata:
tarone <- BreslowDayTest(table, correct = TRUE)     # "correct = TRUE" yields the Tarone correction of the BD test, as per M249
tarone.stat <- unname(tarone$statistic)
tarone.p <- tarone$p.value

