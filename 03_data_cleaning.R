# Load Packages
library(tidyverse)
library(janitor)
library(readr)

set.seed(1968)

# Load Data Set
data <- read_csv(file = "data/raw/wieng_attendance_raw.csv")

# Cleaning
## Remove Second Row
data <- data[-2,]

## Make First Row the Variable Names
data <- janitor::row_to_names(data, 1, remove_rows_above = FALSE)
data <- data %>%
  janitor::clean_names() %>%
  select(date, name, student_id)

## Adding topic
exit_slip <- read_csv(file = "data/processed/wieng_exit_slips_clean.csv") %>%
  select(date, topic) %>%
  unique()
data <- full_join(data, exit_slip) %>%
  filter(date != "Jan 13th", date != "Activity")

data <- data %>%
  mutate(topic = ifelse(date == "Feb 17th", "Activity", topic))

data <- data %>%
  mutate(topic = ifelse(date == "Mar 17th", "Activity", topic))

data <- data %>%
  mutate(topic = ifelse(date == "Apr 21st", "Outside of School Activity", topic))

data <- data %>%
  mutate(topic = ifelse(date == "May 9th", "Outside of School Activity", topic))

# Save Clean Data Set
write_csv(data, file = "data/processed/wieng_attendance_clean.csv")
data <- read_csv(file = "data/processed/wieng_attendance_clean.csv")
