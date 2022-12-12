library(fpp3)
library(foreign)

# import data from SPSS (CB2, activity 8.1 'ftse100.sav') and log transform and difference once
# the 'mutate' entries are aimed at achieving a variable whose time series plot represents 'white noise'
dt <- read.spss('O:/M249/SPSS/Data/Book2/ftse100.sav', use.value.labels = TRUE, to.data.frame = TRUE) %>% 
  rename(
    year = YEAR_,
    month = MONTH_
  ) %>%
  mutate(date = yearmonth(make_date(year, month)), 
         ftse.ln = log(ftse), 
         diff = difference(ftse.ln)) %>%
  select(date, ftse, ftse.ln, diff) %>% 
  as_tsibble(index = date)
  
# plot time series of FTSE data, log transformed FTSE, and first differences until 'white noise' is seen
ggplot(dt) +
  theme_bw() + 
  aes(x = date, 
      y = diff) +    # 'y' variable: trial and error with variables from 'mutate' until stationary ts
  geom_line() + 
  labs(x = "Year", 
       y = "", 
       title = "First difference of the logarithm of monthly \nclosing FTSE index value")

# plot ACF
acf <- dt %>% ACF(diff, lag_max = 20)
ac.y <- acf$acf
ac.x <- c(1:length(ac.y))

ggplot(acf) + 
  aes(y = ac.y, x = ac.x) + 
  ylim(c(-0.5, 0.5)) +
  geom_bar(stat="identity", 
           width= 0.6, 
           colour = "darkblue", 
           fill = NA) + 
  geom_hline(yintercept = 0, 
             colour = "black") + 
  geom_hline(yintercept = -(1.96 / sqrt(length(dt$diff))), 
             linetype = 2, 
             colour = "firebrick") + 
  geom_hline(yintercept = (1.96 / sqrt(length(dt$diff))), 
             linetype = 2, 
             colour = "firebrick") + 
  labs(subtitle = "ACF", 
       x = "Lag", 
       y = NULL) + 
  theme_bw()

# plot PACF
pacf <- dt %>% PACF(diff, lag_max = 20)
pac.y <- pacf$pacf
pac.x <- c(1:length(pac.y))

ggplot(pacf) + 
  aes(y = pac.y, x = pac.x) + 
  ylim(c(-0.5, 0.5)) +
  geom_bar(stat="identity", 
           width= 0.6, 
           colour = "darkblue", 
           fill = NA) + 
  geom_hline(yintercept = 0, 
             colour = "black") + 
  geom_hline(yintercept = -(1.96 / sqrt(length(dt$diff))), 
             linetype = 2, 
             colour = "firebrick") + 
  geom_hline(yintercept = (1.96 / sqrt(length(dt$diff))), 
             linetype = 2, 
             colour = "firebrick") + 
  labs(subtitle = "PACF", 
       x = "Lag", 
       y = NULL) + 
  theme_bw()

lb <- dt %>% 
  features(diff, ljung_box, lag = 20)

lb.stat <- lb$lb_stat
lb.pval <- lb$lb_pvalue

mdl <- dt %>%
  model(ARIMA(ftse))



# dt %>% 
#   PACF(diff, lag_max = 20) %>%
#   autoplot() + 
#   labs(subtitle = "PACF", 
#        y = NULL) + 
#   theme_bw()

