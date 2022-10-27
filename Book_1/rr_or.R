# Enter data as c(a, b, c, d) as defined in Book 1
data <- c(26, 148, 8, 163)

# Confidence level
conf <- 95

data <- matrix(data, byrow=TRUE, nrow=2, ncol=2)
rownames(data) <- c('Exposure', 'No exposure')
colnames(data) <- c('Disease', 'No disease')

# Interpretation of RR depends on the magnitude of the risk involved (B1, P12)
rr.hat <- (data[1, 1]/sum(data[1,]))/(data[2, 1]/sum(data[2,]))

# For uncommon diseases, OR and RR are similar
# In case control studies, only OR is meaningful
or.hat <- (data[1, 1] * data[2, 2]) / (data[1, 2] * data[2, 1])

# Confidence interval for RR
sigma.hat.rr <- sqrt((1/data[1, 1])-(1/sum(data[1,]))+(1/data[2, 1])-(1/sum(data[2,])))  #std error of theta.hat
z <- qnorm((1-((100-conf)/200)), mean=0, sd=1)
rr.ci.lo <- rr.hat * exp(-z * sigma.hat.rr)
rr.ci.hi <- rr.hat * exp(z * sigma.hat.rr)
                  
# Confidence interval for OR
sigma.hat.or <- sqrt((1/data[1, 1])+(1/data[1, 2])+(1/data[2, 1])+(1/data[2, 2]))  #std error of theta.hat
z <- qnorm((1-((100-conf)/200)), mean=0, sd=1)
or.ci.lo <- or.hat * exp(-z * sigma.hat.or)
or.ci.hi <- or.hat * exp(z * sigma.hat.or)