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
gglagplot(tt)
ggAcf(tt)
ggtsdisplay(tt)

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

# classical decomposition
tt %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of cancelled operations index")

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
