#Code File
library(fpp3)
library(tidyverse)

direct <- "Current Uni Stuff/Stats 326/Assignment 3/"
df <- read_csv(paste0(direct,"productivity.csv"))%>%
  as_tsibble(index = Year)


#Part 2
dfTrain <- df%>%
  filter(Year < 2017)


#### PROBLEM 2
dfTrain%>%
  features(`Productivity`, unitroot_kpss)
           
# 0.01, reject the null Data is not stationary

#difference 1

dfTrain%>%
  features(difference(Productivity, lag = 1), unitroot_kpss)

#It is stationary with difference 1
#d = 1

dfTrain2 <- dfTrain%>%
  mutate(diff = difference(Productivity, lag  = 1))

#ACF
dfTrain2%>%
  ACF(diff)%>%
  autoplot() + 
  labs(title = "ACF Plot for Differenced Data With Order 1, Lag 1",
       subtitle = "Labour Productivity for Primary Industries in New Zealand",
       y = "ACF")

#PACF
dfTrain2%>%
  PACF(diff)%>%
  autoplot()

#White Noise Series

fitARIMA <- dfTrain%>%
  model(stepwise = ARIMA(Productivity, stepwise = TRUE),
        search = ARIMA(Productivity, stepwise = FALSE),
        arima = ARIMA(Productivity ~ pdq(1,1,2)))

glance(fitARIMA)%>%
  arrange(AICc)%>%
  select(.model:BIC)

fitARIMA
dfTrain%>%
  autoplot(Productivity%>%difference(1))
