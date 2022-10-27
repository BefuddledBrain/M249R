# load packages
library(tidyverse)
library(foreign)
library(DescTools)  # for the Tarone test (a modification of the Breslow-Day test)

rm(list = ls())

# import data from SPSS (CB1, Activity 4.1)
df.sav <- read.spss('O:/M249/SPSS/Data/Book1/diabetes.sav', use.value.labels = TRUE, to.data.frame = TRUE)

# Create contingency table
table.sav <- xtabs(df.sav$count ~ ., df.sav)
print(table.sav)

# Mantel-Haenszel common OR estimate
cl <- 0.95
mh.or <- mantelhaen.test(table.sav, correct = FALSE, conf.level = cl)
mh.or.stat <- unname(mh.or$statistic)
mh.or.ci.lb <- mh.or$conf.int[1]
mh.or.ci.ub <- mh.or$conf.int[2]

print(paste("MHOR =", mh.or$estimate, "with", 100 * cl, "% CI: (", mh.or.ci.lb,",", mh.or.ci.ub,")"))

# Tarone test of homogeneity of OR for all strata:
tarone <- BreslowDayTest(table.sav, correct = TRUE)     # "correct = TRUE" yields the Tarone correction of the BD test, as per M249
tarone.stat <- unname(tarone$statistic)
tarone.p <- tarone$p.value
print(paste("Tarone test statistic =", tarone$statistic, "(p-value =", tarone$p.value, ")"))

# Mantel-Haenszel chi-squared test of conditional independence
mh.chi2 <- mantelhaen.test(table.sav, correct = TRUE, conf.level = 0.95)
mh.chi2.stat <- unname(mh.chi2$statistic)
mh.chi2.p <- mh.chi2$p.value
print(paste("The MH chi-squared test statistic =", mh.chi2$statistic))
print(paste("The MH chi-squared p-valuec =", mh.chi2$p.value))





# Ignore thissection if importing data from the SPSS sav file
#
# If creating dataframe from printed contingency table: enter "count" data column-wise
# e.g. data from Book1, Table 6.12 on page 51 (for CB1, Activity 4.1)
#
# df.man <- data.frame(
#   cbind(
#     count      = c(0, 1, 15, 129, 218, 104, 311, 124),
#     expand.grid(
#       exposure = c("Non-insulin dependent", "Insulin dependent"),
#       outcome  = c("Died", "Alive"),
#       stratum  = c("Patients aged 40 or younger", "Patients aged over 40")
#     )
#   )
# )
#
# Create contingency table
# table.man <- xtabs(df.man$count ~ ., df.man)

