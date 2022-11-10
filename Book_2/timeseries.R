# load packages
library(tidyverse)
library(lubridate)
library(foreign)
library(latex2exp)

rm(list = ls())

# import data from SPSS ('ts4r.sav' is a copy of 'airline.sav')
df <- read.spss('O:/M249/SPSS/Data/Book2/ts4r.sav', use.value.labels = TRUE, to.data.frame = TRUE) %>% 
  rename(
    year = YEAR_,
    month = MONTH_
  ) %>%
  mutate(date = make_date(year, month), 
         passengers = (passengers**(1/4)))   # transformed by 4th root as raw data not suitable for additive model

# plot time series (or, as here, transformed time series) to assess suitability of additive model
par(mar = c(5, 5, 2, 3))
plot(x = df$date, 
     y = df$passengers, 
     type = "l", 
     col = "darkmagenta", 
     main = "Time series plot", 
     sub = "Assess suitability of additive model", 
     xlab = "Date", 
     ylab = "Passengers (4th root)")

# same plot as above but executed with ggplot2
# ggplot(df) +
#   aes(x = df$date, 
#       y = df$passengers) +
#   geom_line(colour = "firebrick")+ 
#   scale_x_date(limits = c(as.Date("1973-12-1"), as.Date("2000-1-1")), 
#                date_breaks = "2 years", 
#                date_labels = "%Y") +
#   labs(title = "GG: Time series plot", 
#        subtitle = "Transformed data", 
#        x = "Date", 
#        y = "Passengers (4th root)") +
#   theme_bw() +
#   theme(plot.title=element_text(hjust = 0.5), 
#         plot.subtitle=element_text(hjust = 0.5))

# create time series object using base R ts() function
tseries <- ts(df$passengers, 
              frequency=12, 
              start=c(1974, 1), 
              end=c(1999, 12)
              )

# decomposition of time series sing base R function decompose()
decomp <- decompose(tseries, type = "additive")

# plot (1) transformed time series and (2) trend, (3) seasonal, and (4) random components
# par(mfcol=c(4,1))
layout(matrix(c(rep(1, times = 7), 
           rep(2, times = 5), 
           rep(3, times = 5), 
           rep(4, times = 8), 
           0), 
         ncol=1)
)

par(mar = c(0, 5, 2, 3), 
    cex.main = 2.0, 
    cex.lab=1.5, 
    cex.axis=1.2, 
    font.lab = 2, 
    font.main = 2)
plot(decomp$x,    # passengers**(1/4) -- transformed passenger numbers
     type = "l", 
     col = "firebrick", 
     main = "Time series and components", 
     xlab = "", 
     ylab = TeX("\\textbf{Passengers (\\sqrt[4]{n})}"), 
     xaxt = "n")

par(mar = c(0, 5, 0, 3))
plot(decomp$trend, 
     type = "l", 
     col = "royalblue", 
     xlab = "", 
     ylab = "Trend", 
     xaxt = "n")

par(mar = c(0, 5, 0, 3))
plot(decomp$seasonal, 
     type = "l", 
     col = "royalblue", 
     xlab = "", 
     ylab = "Seasonal", 
     xaxt = "n")

par(mar = c(4, 5, 0, 3))
plot(decomp$random, 
     type = "l", 
     col = "royalblue", 
     xlab = "Date", 
     ylab = "Random")