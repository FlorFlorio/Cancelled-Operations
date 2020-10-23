library(tidyverse)
library(zoo)
library(xts)
library(forecast)
library(seasonal) # seas()
library(tseries)

operations <- readRDS("data/processed/cancelled_operations_1994_to_2019.rds")

# convert to ts
x <- xts(operations[,"cancelled_operations_perc"], order.by = as.yearqtr(operations$year_quarter))
tt <- as.ts(as.zoo(x))

autoplot(tt)

# stationarity testing
print(adf.test(tt)) # Augmented Dickey−Fuller Test
print(kpss.test(tt)) # KPSS Test for Level Stationarity

# resulta no estacionaria entonces aplico diff (ojo que esto me hace perder info porque es un filtro)
tt_diff <- diff(tt)
autoplot(tt_diff)

print(adf.test(tt_diff))
print(kpss.test(tt_diff))

# otra manera en vez de diff es hacer detrending (en caso que observe una tendencia)
tt_lm <- lm(tt ~ index(tt))
print(summary(tt_lm))
tt_d <- tt - (tt_lm$coefficients[1] + tt_lm$coefficients[2] * index(tt))
plot(index(tt), tt_d, type = 'l')

print(adf.test(tt_lm))
print(kpss.test(tt_lm))

# Forecasting ----
# Plot some forecasts
autoplot(tt) +
  autolayer(meanf(tt, h=11), series="Mean", PI=FALSE) +
  autolayer(naive(tt, h=11), series="Naïve", PI=FALSE) +
  autolayer(snaive(tt, h=11), series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for quarterly cancelled operations") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

# non-seasonal forecast
autoplot(tt) +
  autolayer(meanf(tt, h=40), series="Mean", PI=FALSE) +
  autolayer(rwf(tt, h=40), series="Naïve", PI=FALSE) +
  autolayer(rwf(tt, drift=TRUE, h=40), series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

# Forecasting lineal
tslm_fit <- tslm(tt ~ index(tt))
f <- forecast(tslm_fit, h = 20, level = c(80, 95))
plot(f, ylab="x", xlab = "tiempo")
lines(fitted(tslm_fit), col = "blue")
summary(tslm_fit)

# Análisis a posteriori del forecast lineal
par(mfrow=c(1, 2))
res <- ts(resid(tslm_fit))
plot.ts( res , ylab = "res (x)")
abline(0, 0)
Acf(res)
# se ve que no es aleatoria la señal

# Auto ARIMA forecast
arima_fit <- auto.arima(tt, seasonal = TRUE)
print(arima_fit)
autoplot(forecast(arima_fit, h = 10)) +
  xlab("Año") + ylab("Porcentaje de cancelación")

res <- residuals(arima_fit)

autoplot(res) +
  xlab("Año") + ylab("Porcentaje de cancelación") +
  ggtitle("Residuos del forecast ARIMA")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")

checkresiduals(arima_fit)
