# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
data <- read_csv(file = "data/processed/wieng_exit_slips_clean.csv") 

# Background ----
## Number per meeting ----
date_order <- read_csv(file = "data/processed/wieng_exit_slips_clean.csv") %>%
  filter(!is.na(date))
date_order$date <- factor(date_order$date, levels = c("May 12th", "May 5th", "Apr 28th", "Apr 14th", "Mar 10th", "Mar 3rd", "Feb 24th", "Feb 10th", "Feb 3rd", "Jan 27th", "Jan 20th", "Jan 13th", "Dec 16th", "Dec 2nd", "Nov 18th", "Nov 4th", "Oct 28th", "Oct 21st", "Oct 14th"))

## Graph ----
ggplot(data = date_order, mapping = aes(y = date)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.9)) +
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
attendance_data <- read_csv(file = "data/processed/wieng_attendance_clean.csv") %>%
  filter(!is.na(date))
attendance_data$date <- factor(attendance_data$date, levels = c("May 12th", "May 9th", "May 5th", "Apr 28th", "Apr 21st", "Apr 14th", "Mar 17th", "Mar 10th", "Mar 3rd", "Feb 24th", "Feb 17th", "Feb 10th", "Feb 3rd", "Jan 27th", "Jan 20th", "Dec 16th", "Dec 2nd", "Nov 18th", "Nov 4th", "Oct 28th", "Oct 21st", "Oct 14th"))

attendance_data <- attendance_data %>%
  filter(!is.na(date))

## Table ----
completion_rate <- data.frame (
  date  = c("May 12th", "May 5th", "Apr 28th", "Apr 14th", "Mar 10th", "Mar 3rd", "Feb 24th", "Feb 10th", "Feb 3rd", "Jan 27th", "Jan 20th", "Jan 13th", "Dec 16th", "Dec 2nd", "Nov 18th", "Nov 4th", "Oct 28th", "Oct 21st", "Oct 14th"),
  exit_slips = c(6, 4, 9, 7, 9, 8, 10, 8, 13, 11, 6, 13, 9, 9, 7, 14, 4, 11, 11), 
  attendance = c(9, 8, 9, 11, 11, 9, 11, 13, 11, 15, 8, NA, 13, 10, 11, 16, 4, 11, 9)
) %>%
  mutate(
    completion_prop = exit_slips / attendance, 
    completion_percent = completion_prop * 100
  )

completion_rate %>%
  kbl(caption = "Exit Slip Completion Rate Per Meeting") %>%
  kable_styling()
