install.packages('Rcpp')
library(Rcpp)
library(prophet)
library(lubridate)

setwd("~/Documents/Research/Crypto/ChangePoint")
frax_df <- read.csv('frax.csv')
frax_df$frax_date <-as_datetime(frax_df$Timestamp)

df <- frax_df[,c('frax_date','Frax')]
colnames(df) <- c('ds','y')
m <- prophet(df)

future <- make_future_dataframe(m, periods = 30)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
