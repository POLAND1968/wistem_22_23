# Load Packages
library(tidyverse)
library(janitor)
library(readr)

set.seed(1968)

# Load Data Set
data <- read_csv(file = "data/raw/wistem_attendance_raw.csv")

# Cleaning
## Remove Second Row
data <- data[-2,]

## Make First Row the Variable Names
data <- janitor::row_to_names(data, 1, remove_rows_above = FALSE)
data <- data %>%
  janitor::clean_names() %>%
  select(date, name, student_id, lunch)

## Change one date 
data$date <- recode(data$date, "Mar 24th" = "Feb 24th")

## Adding topic
exit_slip <- read_csv(file = "data/processed/wistem_exit_slips_clean.csv") %>%
  select(date, topic) %>%
  unique()
data <- full_join(data, exit_slip) %>%
  filter(date != "May 31st")

data <- data %>%
  mutate(topic = ifelse(date == "Apr 28th", "Out of School Event", topic))

# Save Clean Data Set
write_csv(data, file = "data/processed/wistem_attendance_clean.csv")
data <- read_csv(file = "data/processed/wistem_attendance_clean.csv")
