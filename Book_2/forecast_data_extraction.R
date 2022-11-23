# original: forecast_holt.R
library(fpp3)

# get data ('Year' is the time variable and 'Population' is the response variable in this example)
ts_data <- tsibbledata::global_economy %>%
  filter(Country == "Australia") %>% 
  select(Year, Population) %>% 
  as_tsibble(index = Year)

# create model ('fit' is my name for the model object)
fit <- ts_data %>%
  model(Holt = ETS(Population ~ error("A") + trend("A") + season("N")))
        
# create forecast ('fc' is my name for the forecast object)
fc <- fit %>% forecast(h = 10)

# available from model object 'fit':
# 'Holt' in 'fit$Holt[[1]]..'. is from the name that I have given the model. It is the '~ error("A") + trend("A") + season("N")'
# that tells the model() function to use the Holt agorthm. I could have named it 'Blah', in which case the following code
# would use 'fit$Blah[[1]]...', but it would still use the Holt algorithm. 'Blah' would be a mere label.
ft.alpha <- fit$Holt[[1]]$fit$par$estimate[1]         # for SES (Simple), Holt, and Holt-Winters exponential smoothing
ft.beta <- fit$Holt[[1]]$fit$par$estimate[2]          # for Holt, and Holt-Winters
# ft.gamma <- fit$Holt[[1]]$fit$par$estimate[3]         # for Holt-Winters

# fit$Holt[[1]]$fit$est$.fitted gives fitted values
# fit$Holt[[1]]$fit$est$.resid gives residual values
# fit$Holt[[1]]$fit$est$Year and # fit$Holt[[1]]$fit$est$Population give original time series variables

# fit$Holt[[1]]$fit$est gives data variables plus '.fitted' and '.resid'
# fit$Holt[[1]]$fit$fit gives (via $ prefix) sigma2 log_lik, AIC, AICc, BIC, MSE, AMSE, MAE
# and fit$Holt[[1]]$fit$fit[[n]] gives values of nth element

# fit$Holt[[1]]$fit$fit gives (sigma2, log_lik, AIC, AICc, BIC, AMSE, MAE)


# available from :
# get mu and sigma from forecast object 'fc' (a pair of values for each forecast step -- 'h' in forecast function)
fc.mu_sig <- as.list.data.frame(fc$Population)

# get separate values of 'mu' and 'sigma' for 'h' forecast steps
fc.mus <- sapply(fc.mu_sig, "[[", "mu")
fc.sigmas <- sapply(fc.mu_sig, "[[", "sigma")
fc.vals <- fc$.mean     # forecast value of response variable ('Population' in this time series)
