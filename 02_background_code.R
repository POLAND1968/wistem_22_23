# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
data <- read_csv(file = "data/processed/wistem_exit_slips_clean.csv") 

# Background ----
## Number per meeting ----
date_order <- read_csv(file = "data/processed/wistem_exit_slips_clean.csv") %>%
  filter(!is.na(date))
date_order$date <- factor(date_order$date, levels = c("May 31st", "May 12th", "Apr 14th", "Mar 31st", "Mar 3rd", "Feb 24th", "Feb 10th", "Jan 27th", "Dec 16th", "Dec 2nd", "Nov 18th", "Oct 28th", "Oct 14th", "Sep 30th"))

## Graph ----
ggplot(data = date_order, mapping = aes(y = date)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.8)) +
  labs(title = "Number of Exit Slips Per Meeting")

## Table ----
date_order %>%
  filter(!is.na(date)) %>%
  count(date) %>%
  mutate(
    proportion = n / sum(n), 
    percent = (n / sum(n)) * 100 
  ) %>%
  kbl(caption = "Number of Exit Slips Per Meeting") %>%
  kable_styling()

# Completion Rate ----
attendance_data <- read_csv(file = "data/processed/wistem_attendance_clean.csv") %>%
  filter(!is.na(date))
attendance_data$date <- factor(attendance_data$date, levels = c("May 31st", "May 12th", "Apr 14th", "Mar 31st", "Mar 3rd", "Feb 24th", "Feb 10th", "Jan 27th", "Dec 16th", "Dec 2nd", "Nov 18th", "Oct 28th", "Oct 14th", "Sep 30th"))

attendance_data <- attendance_data %>%
  filter(!is.na(date))

## Table ----
completion_rate <- data.frame (
  date  = c("May 31st", "May 12th", "Apr 14th", "Mar 31st", "Mar 3rd", "Feb 24th", "Feb 10th", "Jan 27th", "Dec 16th", "Dec 2nd", "Nov 18th", "Oct 28th", "Oct 14th", "Sep 30th"),
  exit_slips = c(7, 43, 35, 40, 41, 39, 24, 39, 41, 43, 21, 18, 29, 64), 
  attendance = c(NA, 46, 40, 57, 51, 48, 51, 44, 54, 48, 51, 48, 35, 48)
) %>%
  mutate(
    completion_prop = exit_slips / attendance, 
    completion_percent = completion_prop * 100
  )

completion_rate %>%
  kbl(caption = "Exit Slip Completion Rate Per Meeting") %>%
  kable_styling()
