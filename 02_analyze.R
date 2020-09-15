library(tidyverse)
library(zoo)
library(xts)
library(forecast)

df <- readRDS("data/processed/cancelled_operations_1994_to_2019.rds")

ggplot(df, aes(x = year_quarter, y = cancelled_operations)) +
  geom_line(group = 1) + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# calcular la transformada rápida de fourier
fft <- fft(df$cancelled_operations)
glimpse(fft)

ggplot() + geom_line(aes(x = seq_along(fft), y = Mod(fft)))

# seasonal plot
x <- xts(df[,2], order.by = as.yearqtr(df$year_quarter))
tt <- as.ts(as.zoo(x))

ggseasonplot(tt, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Cancelled operations") +
  ggtitle("Seasonal plot: cancelled operations")

