# open SPSS data file 'asthmagest.sav' (CB1, Activity 2.5) using package 'haven'
library(haven)
 
# When importing a SPSS .sav file, only the first column is needed to
# create a contingency table in R. Convert that column into a matrix
# using row-wise entry:
spss.dat <- read_sav('O:/M249/SPSS/Data/Book1/asthmagest.sav')
r.dat <- matrix(spss.dat[[1]], byrow=TRUE, ncol=2)
colnames(r.dat) <- c("Hospitalised","Not hospitalised")
rownames(r.dat) <- c("Pre-term", "Term", "Post-term")

# when creating dataframe from table of data, enter column-wise:
# r.dat <- matrix(c(18, 402, 45, 266, 8565, 1100), ncol=2)
# (the default entry mode is byrow=FALSE)

print(r.dat)
chisq.test(r.dat)
fisher.test(r.dat)

# Access to the values returned by chisq.test() function
# The result of chisq.test() function is a list containing the following components:
#   
# statistic:  the value the chi-squared test statistic
# method:     statistical test used
# parameter:  the degrees of freedom
# p.value:    the p-value of the test
# observed:   the observed count
# expected:   the expected count

chisq.test(r.dat)$statistic
chisq.test(r.dat)$p.value
chisq.test(r.dat)$observed
chisq.test(r.dat)$expected

