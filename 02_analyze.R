library(tidyverse)
library(zoo)
library(xts)
library(forecast)
library(seasonal) # seas()

df <- readRDS("data/processed/cancelled_operations_1994_to_2019.rds")

df %>% 
  pivot_longer(2:6, names_to = "serie", values_to = "valor") %>% 
  filter(serie %in% c("cancelled_operations",
                      "patients_not_treated_28_days")) %>% 
  ggplot(aes(x = year_quarter, y = valor, col = serie)) +
  geom_line(group = 1) + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

df %>% 
  pivot_longer(2:6, names_to = "serie", values_to = "valor") %>% 
  # filter(serie %in% c("cancelled_operations",
  #                     "patients_not_treated_28_days")) %>% 
  ggplot(aes(x = year_quarter, y = valor)) +
  geom_line(group = 1) + 
  facet_grid(vars(serie), scales = "free_y") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(df, aes(x = year_quarter, y = cancelled_operations)) +
  geom_line(group = 1) + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# convert to ts
x <- xts(df[,5], order.by = as.yearqtr(df$year_quarter))
tt <- as.ts(as.zoo(x))

autoplot(tt)

# calcular la transformada rápida de fourier
fft <- fft(df$cancelled_operations)
glimpse(fft)

ggplot() + geom_line(aes(x = seq_along(fft), y = Mod(fft)))

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

# Correlation ----
# day beds
day_beds_occupancy <- readRDS("data/processed/day_beds_occupancy_2010Q1_to_2020Q1.rds")

x2 <- xts(day_beds_occupancy[,2], order.by = as.yearqtr(day_beds_occupancy$year_quarter))
tt2 <- as.ts(as.zoo(x2))

autoplot(tt2)

df2 <- df %>%
  filter(year_quarter >= "2010 Q1") %>%
  select(cancelled_operations_perc) %>%
  as.numeric() %>%
  unlist()

bo <- day_beds_occupancy %>%
  filter(year_quarter <= "2019 Q3") %>%
  select(occupancy_all_types) %>%
  #as.numeric() %>%
  unlist()

# correlacion cruzada con convolve
ts_corr <- convolve(df2, bo, conj = TRUE) # se usa el conjugado
plot(1:length(ts_corr), ts_corr , type = 'l')

ccf(df2, bo, lag.max = 39)

# overnight beds
on_beds_occupancy <- readRDS("data/processed/overnight_beds_occupancy_2010Q1_to_2020Q1.rds")

x2 <- xts(on_beds_occupancy[,2], order.by = as.yearqtr(on_beds_occupancy$year_quarter))
tt2 <- as.ts(as.zoo(x2))

autoplot(tt2)

bo <- on_beds_occupancy %>%
  filter(year_quarter <= "2019 Q3") %>%
  select(occupancy_all_types) %>%
  #as.numeric() %>%
  unlist()

# correlacion cruzada con convolve
ts_corr <- convolve(df2, bo, conj = TRUE) # se usa el conjugado
plot(1:length(ts_corr), ts_corr , type = 'l')

ccf(df2, bo, lag.max = 39)