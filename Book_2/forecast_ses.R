library(foreign)
library(distributional)
library(fpp3)

# get data
ts_data <- tsibbledata::global_economy %>%
  filter(Country == "Algeria") %>% 
  select(Year, Exports) %>% 
  as_tsibble(index = Year)

# plot to assess time series
ts_data %>% 
  autoplot() + 
  theme_bw() +
  labs(title = "Algerian exports (% of GDP) show no trend or seasonality.")

# create model
fit <- ts_data %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
        
fit.data <- fit %>% 
  tidy()

# create forecast
fc <- fit %>% forecast(h = 5)

# extract parameters
alpha <- fit.data$estimate[1]

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
  geom_line(aes(y = Exports),                   # time series data
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
  labs(title = "Simple Exponential Smoothing ETS(A, N, N)",
       subtitle = "Algerian exports (% of GDP).") +
  theme(legend.position = "none")
