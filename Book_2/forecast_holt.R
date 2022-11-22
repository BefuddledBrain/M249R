library(foreign)
library(fpp3)

# get data
ts_data <- tsibbledata::global_economy %>%
  filter(Country == "Australia") %>% 
  select(Year, Population) %>% 
  as_tsibble(index = Year)

# plot to assess time series
ts_data %>% 
  ggplot(aes(x = Year, 
             y = Population)) +
  geom_line() +
  theme_bw() +
  labs(title = "Australian population has trend, but no seasonality.")

# create model
fit <- ts_data %>%
  model(Holt = ETS(Population ~ error("A") + trend("A") + season("N")))
        
fit.data <- fit %>% 
  tidy()

# create forecast
fc <- fit %>% forecast(h = 10)

# extract parameters
alpha <- fit.data$estimate[1]
beta <- fit.data$estimate[2]

ci_fc  <-  fc %>% 
  hilo() %>% 
  unpack_hilo(5:6) %>% 
  as.data.frame() %>% 
  select('80%_lower', '80%_upper', '95%_lower', '95%_upper') %>% 
  rename(lo80 = '80%_lower', 
         hi80 = '80%_upper', 
         lo95 = '95%_lower', 
         hi95 = '95%_upper')

# create and plot forecast model
fit %>%
  augment() %>% 
  ggplot(aes(x = Year)) +                       # level = NULL or c(80, 95)
  geom_line(aes(y = Population),                # time series data
            color = "firebrick") +
  geom_line(aes(y = .fitted),                   # smoothed data
            color = "goldenrod") +
  geom_line(data = fc, 
            aes(y = .mean), 
            color = "goldenrod") +
  geom_ribbon(data = fc, 
              aes(ymin = ci_fc$lo95, 
                  ymax = ci_fc$hi95),
              alpha = 0.2, 
              fill = "goldenrod") +
  geom_ribbon(data = fc, 
              aes(ymin = ci_fc$lo80, 
                  ymax = ci_fc$hi80),
              alpha = 0.2, 
              fill = "goldenrod") +
  theme_bw() + 
  labs(title = "Holt's Linear Method ETS(A, A, N)",
       subtitle = "Australian Population.") +
  theme(legend.position = "none")
