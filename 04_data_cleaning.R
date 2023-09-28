# Load Packages
library(tidyverse)
library(janitor)
library(readr)

set.seed(1968)

# Load Data Set
data <- read_csv(file = "data/raw/wieng_exit_slips_raw.csv")

# Cleaning
## Remove Second Row
data <- data[-2,]

## Make First Row the Variable Names
data <- janitor::row_to_names(data, 1, remove_rows_above = FALSE)
data <- data %>%
  janitor::clean_names() %>%
  select(date, topic, interesting, included, belong)

# Save Clean Data Set
write_csv(data, file = "data/processed/wieng_exit_slips_clean.csv")
data <- read_csv(file = "data/processed/wieng_exit_slips_clean.csv")
