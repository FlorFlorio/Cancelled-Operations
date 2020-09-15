library(tidyverse)
library(readxl)

df <- read_excel("data/raw/QMCO-Timeseries-Q3-2019-20-4hiU8.xlsx",
                 sheet = 1, range = "B15:H118")

names(df) <- c("year", "quarter", "cancelled_operations",
               "patients_not_treated_28_days", "elective_admissions",
               "cancelled_operations_perc", "patients_not_treated_28_days_perc")

df <- df %>%
  mutate(year = str_sub(year, 1, 4)) %>%
  fill(year) %>% 
  unite(year_quarter, year, quarter, sep = "-") %>% 
  mutate(year_quarter = as.yearqtr(year_quarter))

saveRDS(df, "data/processed/cancelled_operations_1994_to_2019.rds")