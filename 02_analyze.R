library(tidyverse)
library(zoo)
library(xts)
library(forecast)
library(seasonal) # seas()

df <- readRDS("data/processed/cancelled_operations_1994_to_2019.rds")

ggplot(df, aes(x = year_quarter, y = cancelled_operations)) +
  geom_line(group = 1) + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

autoplot(tt)

# calcular la transformada rápida de fourier
fft <- fft(df$cancelled_operations)
glimpse(fft)

ggplot() + geom_line(aes(x = seq_along(fft), y = Mod(fft)))

# convert to ts
x <- xts(df[,2], order.by = as.yearqtr(df$year_quarter))
tt <- as.ts(as.zoo(x))

# Decomposition ----
# moving average
autoplot(tt, series="Data") +
  autolayer(ma(tt, 4), series = "4-MA") +
  xlab("Year") + ylab("Cancelled operations") +
  ggtitle("Cancelled operations UK") +
  scale_colour_manual(values = c("Data" = "grey50", "4-MA" = "red"),
                      breaks = c("Data", "4-MA"))

# seasonal plot
ggseasonplot(tt, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Cancelled operations") +
  ggtitle("Seasonal plot: cancelled operations")

ggseasonplot(tt, polar = TRUE) +
  ylab("Cancelled operations") +
  ggtitle("Seasonal plot: cancelled operations")

ggsubseriesplot(tt) +
  ylab("Cancelled operations") +
  ggtitle("Seasonal subseries plot: cancelled operations")

autoplot(tt)
gglagplot(tt)
ggAcf(tt)

# classical decomposition
tt %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of cancelled operations index")

# X11 decomposition
fit <- tt %>% seas(x11 = "")
autoplot(fit) +
  ggtitle("X11 decomposition of electrical equipment index")

autoplot(tt, series="Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Cancelled operations UK") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")

# seats decomposition
tt %>% seas() %>%
  autoplot() +
  ggtitle("SEATS decomposition of cancelled operations index")

# stl decomposition
tt %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

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
