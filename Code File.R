#Code File
library(fpp3)
library(tidyverse)

# direct <- "Current Uni Stuff/Stats 326/Assignment 3/"
df <- read_csv("productivity.csv")%>%
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
        arima210 = ARIMA(Productivity ~ pdq(2,1,0)))

glance(fitARIMA)%>%
  arrange(AICc)%>%
  select(.model:BIC)

fitARIMA
dfTrain%>%
  autoplot(Productivity%>%difference(1))

#Need to S


model2 <- dfTrain%>%
  model(ARIMA(Productivity ~ pdq(2,1,0)))
#Part 3
bestARIMA <- dfTrain%>%
  model(ARIMA(Productivity ~ pdq(0,1,1)))


report(model2)
bestARIMA%>%
  gg_tsresiduals()


fc <- bestARIMA%>%
  forecast(h = 5)

fc

fc%>%
  autoplot(df, level = c(90, 99)) + 
  labs(title = "5 Year Forecast Using an ARIMA(0,1,1) with Drift",
       subtitle = "Labour Productivity for Primary Industries in New Zealand",
       y = "Index")




dfTrain%>%
  ACF(difference(Productivity, lag = 1))%>%
  autoplot()

df
