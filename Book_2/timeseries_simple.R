# load packages
library(foreign)
library(forecast)

rm(list = ls())

# import data from SPSS ('chemical.sav' from Activity 4.3)
df <- read.spss('O:/M249/SPSS/Data/Book2/chemical.sav', use.value.labels = TRUE, to.data.frame = TRUE)
ts1 <- ts(df)

# plot time series
par(mar = c(5, 5, 2, 3))
plot(ts1, 
     type = "l", 
     col = "red3", 
     main = "Time series plot", 
     xlab = "Date", 
     ylab = "Temperature", 
     las = 1)

# There is no trend; no seasonality; constant random errors so simple exponential smoothing is appropriate
# To use HoltWinters() for simple exponential smoothing, we need to set the parameters:
#      beta = FALSE  (this is gamma in M249)
#      gamma = FALSE (this is delta in M249)
ts1.predict <- HoltWinters(ts1, beta=FALSE, gamma=FALSE)

# extract SSE and alpha; calculate RMSE
sse <- ts1.predict$SSE
rmse <- sqrt(ts1.predict$SSE / (length(ts1) - 1))
alpha <- ts1.predict$alpha

# plot original time series vs fitted values
par(mar = c(5, 5, 2, 3))
plot(ts1.predict, 
     las = 1)

# obtain forecast of values ahead of known values
ts1.forecast <- forecast(ts1.predict, h=5)        # ts1.forecast$mean gives forecast values
future.val <- ts1.forecast$mean[1]
par(mar = c(5, 5, 2, 3))
plot(ts1.forecast, 
     col = c("firebrick"), 
     main = "Forecast values", 
     las = 1
     )