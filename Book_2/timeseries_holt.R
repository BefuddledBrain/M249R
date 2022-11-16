# load packages
library(foreign)
library(dplyr)
library(forecast)

rm(list = ls())

# import data from SPSS ('airline5.sav' from Activity 5.1)
df <- read.spss('O:/M249/SPSS/Data/Book2/airline5.sav', use.value.labels = TRUE, to.data.frame = TRUE) %>% 
  rename(
    psngrs_rt4 = fpass
  )

# plot time series: the trend for 'passengers' is non-linear so not suitable for an additive model
par(mar = c(5, 5, 2, 3))
plot(df$passengers / 1e+06, 
     type = "l", 
     col = "red3", 
     main = "Time series plot", 
     xlab = "Date", 
     ylab = "Number (millions)", 
     las = 1)

# plot transformed time series: 4th root of 'passengers' is linear so suitable for an additive model
par(mar = c(5, 5, 2, 3))
plot(df$psngrs_rt4, 
     type = "l", 
     col = "red3", 
     main = "Time series plot", 
     xlab = "Date", 
     ylab = "Number (4th root)", 
     las = 1)

# There is a trend; no seasonality; constant random errors so Holt exponential smoothing is appropriate
# To use HoltWinters() for Holt exponential smoothing, we need to set the parameter:
#      gamma = FALSE (this is delta in M249)
ts1 <- ts(df$psngrs_rt4)
ts1.hw <- HoltWinters(ts1, gamma=FALSE)    # no seasonality ('delta' in M249 is called 'gamma' in R)

# extract SSE and alpha; calculate RMSE
sse <- ts1.hw$SSE
rmse <- sqrt(ts1.hw$SSE / (length(ts1) - 1))
alpha <- unname(ts1.hw$alpha)
gamma <- unname(ts1.hw$beta)    # the smoothing parameter 'beta' in R is called 'gamma' in M249

# plot original time series vs fitted values
par(mar = c(5, 5, 2, 3))
plot(ts1.hw, 
     las = 1)

# obtain forecast of values ahead of known values
# ts1.forecast <- forecast(ts1.hw, h=5)        # ts1.forecast$mean gives forecast values
# ts1.forecast <- predict(ts1.hw, n.ahead = 5)        # ts1.forecast$mean gives forecast values
ts1.forecast <- forecast:::forecast.HoltWinters(ts1.hw, h = 5)
future.val <- ts1.forecast$mean

# plot forecast values
par(mar = c(5, 5, 2, 3))
plot(1, 
     type = "n", 
     main = "Forecast values", 
     ylab = "Numbers", 
     xlim=c(0, 60), 
     ylim=c(35, 125), 
     las = 1)
lines(ts1.forecast$x,
     col = "dodgerblue")
lines(ts1.forecast$fitted,
      col = "dodgerblue", lty = 2)
lines(ts1.forecast$mean,
      col = "red")
