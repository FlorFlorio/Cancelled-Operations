library(tidyverse)
library(ggpubr)

operations <- readRDS("data/processed/cancelled_operations_1994_to_2019.rds")

# day beds ----
day_beds_occupancy <- readRDS("data/processed/day_beds_occupancy_2010Q1_to_2020Q1.rds")
day_beds_occupancy <- filter(day_beds_occupancy, year_quarter != "2020 Q1")

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

df_db <- data.frame(op_10_19 = operations_2010_2019,
                    db_10_19 = day_beds_occupancy_2010_2019)

qplot(db_10_19, op_10_19, data = df_db) +
  xlab("Day bed occupancy (%?)") + ylab("Cancelled operations (%)")

ggscatter(df_db, x = "db_10_19", y = "op_10_19", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Day bed occupancy (%?)", ylab = "Cancelled operations (%)")

# correlacion cruzada con convolve
ts_corr <- convolve(operations_2010_2019,
                    day_beds_occupancy_2010_2019,
                    conj = TRUE) # se usa el conjugado
plot(1:length(ts_corr), ts_corr , type = 'l')

# correlación con ccf
ccf(operations_2010_2019, day_beds_occupancy_2010_2019, lag.max = 39)

# overnight beds ----
night_beds_occupancy <- readRDS("data/processed/overnight_beds_occupancy_2010Q1_to_2020Q1.rds")

x2 <- xts(night_beds_occupancy[,2], order.by = as.yearqtr(night_beds_occupancy$year_quarter))
tt2 <- as.ts(as.zoo(x2))

autoplot(tt2)

night_beds_occupancy_2010_2019 <- night_beds_occupancy %>%
  filter(year_quarter <= "2019 Q3") %>%
  select(occupancy_all_types) %>%
  unlist()

df_nb <- data.frame(op_10_19 = operations_2010_2019,
                    nb_10_19 = night_beds_occupancy_2010_2019)

qplot(nb_10_19, op_10_19, data = df_nb) +
  xlab("Night bed occupancy (%?)") + ylab("Cancelled operations (%)")

ggscatter(df_nb, x = "nb_10_19", y = "op_10_19", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Night bed occupancy (%?)", ylab = "Cancelled operations (%)")

# correlacion cruzada con convolve
ts_corr <- convolve(operations_2010_2019,
                    night_beds_occupancy_2010_2019,
                    conj = TRUE) # se usa el conjugado
plot(1:length(ts_corr), ts_corr , type = 'l')

ccf(operations_2010_2019, night_beds_occupancy_2010_2019, lag.max = 39)
ggCcf(operations_2010_2019, night_beds_occupancy_2010_2019, lag.max = 39)


df <- data.frame(cancelled_operations = operations_2010_2019,
                 day = day_beds_occupancy_2010_2019,
                 night = night_beds_occupancy_2010_2019) %>% 
  pivot_longer(2:3, names_to = "bed_type", values_to = "bed_occupancy")

b <- ggplot(df, aes(x = bed_occupancy, y = cancelled_operations))
# Change color and shape by groups (bed_type)
b + geom_point(aes(color = bed_type, shape = bed_type))+
  geom_smooth(aes(color = bed_type, bed_type = bed_type), method = "lm") +
  geom_rug(aes(color = bed_type)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
# Remove confidence region (se = FALSE)
# Extend the regression lines: fullrange = TRUE
b + geom_point(aes(color = bed_type, shape = bed_type)) +
  geom_rug(aes(color = bed_type)) +
  geom_smooth(aes(color = bed_type), method = lm, 
              se = TRUE, fullrange = FALSE)+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  ggpubr::stat_cor(aes(color = bed_type))
