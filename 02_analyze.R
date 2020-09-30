library(tidyverse)
library(zoo)
library(xts)
library(forecast)
library(seasonal) # seas()

operations <- readRDS("data/processed/cancelled_operations_1994_to_2019.rds")

operations %>% 
  pivot_longer(2:6, names_to = "serie", values_to = "valor") %>% 
  filter(serie %in% c("cancelled_operations",
                      "patients_not_treated_28_days")) %>% 
  ggplot(aes(x = year_quarter, y = valor, col = serie)) +
  geom_line(group = 1) + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

operations %>% 
  pivot_longer(2:6, names_to = "serie", values_to = "valor") %>% 
  filter(serie %in% c("elective_admissions",
                      "cancelled_operations_perc",
                      "patients_not_treated_28_days_perc")) %>%
  ggplot(aes(x = year_quarter, y = valor)) +
  geom_line(group = 1) + 
  facet_grid(vars(serie), scales = "free_y") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(operations, aes(x = year_quarter, y = cancelled_operations)) +
  geom_line(group = 1) + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# convert to ts
x <- xts(operations[,"cancelled_operations_perc"], order.by = as.yearqtr(operations$year_quarter))
tt <- as.ts(as.zoo(x))

autoplot(tt)

# calcular la transformada rápida de fourier
fft <- fft(operations$cancelled_operations)
glimpse(fft)

# me quedo con la primer mitad (el vector completo está espejado a la mitad)
mod_fft <- Mod(fft)
mod_fft <- mod_fft[1:ceiling(length(mod_fft)/2)]

ggplot() + geom_line(aes(x = seq_along(mod_fft), y = mod_fft))

# dónde y cuáles son los picos de la fft?
picos_fft <- data.frame(valor = mod_fft[order(mod_fft, decreasing=TRUE)[1:3], drop=FALSE],
                        posicion = order(mod_fft, decreasing=TRUE)[1:3])
picos_fft

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

# ggseasonplot(tt, polar = TRUE) +
#   ylab("Cancelled operations") +
#   ggtitle("Seasonal plot: cancelled operations")

ggsubseriesplot(tt) +
  ylab("Cancelled operations") +
  ggtitle("Seasonal subseries plot: cancelled operations")

autoplot(tt)
#gglagplot(tt)
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

# filtro para que empiece desde el mismo punto que la serie de las camas
operations_2010_2019 <- operations %>%
  filter(year_quarter >= "2010 Q1") %>%
  select(cancelled_operations_perc) %>%
  unlist()

day_beds_occupancy_2010_2019 <- day_beds_occupancy %>%
  filter(year_quarter <= "2019 Q3") %>%
  select(occupancy_all_types) %>%
  unlist()

# correlacion cruzada con convolve
ts_corr <- convolve(operations_2010_2019,
                    day_beds_occupancy_2010_2019,
                    conj = TRUE) # se usa el conjugado
plot(1:length(ts_corr), ts_corr , type = 'l')

# correlación con ccf
ccf(operations_2010_2019, day_beds_occupancy_2010_2019, lag.max = 39)

# overnight beds
night_beds_occupancy <- readRDS("data/processed/overnight_beds_occupancy_2010Q1_to_2020Q1.rds")

x2 <- xts(night_beds_occupancy[,2], order.by = as.yearqtr(night_beds_occupancy$year_quarter))
tt2 <- as.ts(as.zoo(x2))

autoplot(tt2)

night_beds_occupancy_2010_2019 <- night_beds_occupancy %>%
  filter(year_quarter <= "2019 Q3") %>%
  select(occupancy_all_types) %>%
  unlist()

# correlacion cruzada con convolve
ts_corr <- convolve(operations_2010_2019,
                    night_beds_occupancy_2010_2019,
                    conj = TRUE) # se usa el conjugado
plot(1:length(ts_corr), ts_corr , type = 'l')

ccf(operations_2010_2019, night_beds_occupancy_2010_2019, lag.max = 39)
