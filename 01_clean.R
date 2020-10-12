library(tidyverse)
library(readxl)

# cancelled operations ----
df <- read_excel("data/raw/QMCO-Timeseries-Q3-2019-20-4hiU8.xlsx",
                 sheet = 1, range = "B15:H118")

names(df) <- c("year", "quarter", "cancelled_operations",
               "patients_not_treated_28_days", "elective_admissions",
               "cancelled_operations_perc", "patients_not_treated_28_days_perc")

df %>%
  mutate(year = str_sub(year, 1, 4)) %>%
  fill(year) %>% 
  unite(year_quarter, year, quarter, sep = "-") %>% 
  mutate(year_quarter = as.yearqtr(year_quarter)) %>% 
  saveRDS("data/processed/cancelled_operations_1994_to_2019.rds")

# bed occupancy % (day only) ----
beds_day <- read_excel("data/raw/Beds-Timeseries-2010-11-onwards-DAY-ONLY-Q1-2020-21-ADJ-for-missings-QWEDC.xls",
                 sheet = "Open Day Only", range = "B14:U55") %>% select(1, 2, 16, 17, 19, 20)

names(beds_day) <- c("year", "quarter", "occupancy_all_types",
                     "occupancy_general_and_acute", "occupancy_maternity",
                     "occupancy_mental")

beds_day %>%
  mutate(year = str_sub(year, 1, 4)) %>%
  unite(year_quarter, year, quarter, sep = " ") %>% 
  mutate(year_quarter = as.yearqtr(year_quarter)) %>% 
  saveRDS("data/processed/day_beds_occupancy_2010Q1_to_2020Q1.rds")

# bed occupancy % (overnight) ----
beds_overnight <- read_excel("data/raw/Beds-Timeseries-2010-11-onwards-DAY-ONLY-Q1-2020-21-ADJ-for-missings-QWEDC.xls",
                       sheet = "Open Overnight", range = "B14:U55") %>% select(1, 2, 16, 17, 19, 20)

names(beds_overnight) <- c("year", "quarter", "occupancy_all_types",
                           "occupancy_general_and_acute", "occupancy_maternity",
                           "occupancy_mental")

beds_overnight %>%
  mutate(year = str_sub(year, 1, 4)) %>%
  unite(year_quarter, year, quarter, sep = " ") %>% 
  mutate(year_quarter = as.yearqtr(year_quarter)) %>% 
  saveRDS("data/processed/overnight_beds_occupancy_2010Q1_to_2020Q1.rds")
