# cohort & case control studies:
# ---------------------------------------------------------------------------------

# Enter data as c(a, b, c, d) as defined in Book 1
coh.dat <- c(62, 76, 5, 55)

# Confidence interval
ci <- 95

coh.m <- matrix(coh.dat, byrow=TRUE, nrow=2, ncol=2)
rownames(coh.m) <- c('Exposure', 'No exposure')
colnames(coh.m) <- c('Disease', 'No disease')

# Interpretation of RR depends on the magnitude of the riskd involved (B1, P12)
rr.hat <- (coh.m[1, 1]/sum(coh.m[1,]))/(coh.m[2, 1]/sum(coh.m[2,]))
rr.hat

# For uncommon diseases, OR and RR are similar
# In case control studies, only OR is meaningful
or.hat <- (coh.m[1, 1] * coh.m[2, 2]) / (coh.m[1, 2] * coh.m[2, 1])
or.hat

# Confidence interval for RR
sigma.hat.rr <- sqrt((1/coh.m[1, 1])-(1/sum(coh.m[1,]))+(1/coh.m[2, 1])-(1/sum(coh.m[2,])))  #std error of theta.hat
z <- qnorm((1-((100-ci)/200)), mean=0, sd=1)
rr.neg <- rr.hat * exp(-z * sigma.hat.rr)
rr.pos <- rr.hat * exp(z * sigma.hat.rr)
                  
# Confidence interval for OR
sigma.hat.or <- sqrt((1/coh.m[1, 1])+(1/coh.m[1, 2])+(1/coh.m[2, 1])+(1/coh.m[2, 2]))  #std error of theta.hat
z <- qnorm((1-((100-ci)/200)), mean=0, sd=1)
or.neg <- or.hat * exp(-z * sigma.hat.or)
or.pos <- or.hat * exp(z * sigma.hat.or)