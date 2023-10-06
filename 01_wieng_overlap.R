# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
wieng_data <- read_csv(file = "data/processed/wieng_attendance_clean.csv")
wistem_data <- read_csv(file = "data/processed/wistem_attendance_clean.csv")

# WiENG 
wieng_names <- wieng_data %>% # 32 unique students
  select(name, student_id) %>%
  unique()

wieng_unique_students <- wieng_names %>%
  nrow()

# WiSTEM
wistem_names <- wistem_data %>% # 138 unique students
  select(name, student_id) %>%
  unique()

wistem_unique_students <- wistem_names %>%
  nrow()

# Overlapping
overalping_students <- dplyr::semi_join(wieng_names, wistem_names, "student_id")

num_overlap_student <- overalping_students %>%
  nrow()

## Percentage
wieng_overlap_percent <- (wieng_unique_students / num_overlap_student) * 100

wistem_overlap_percent <- (wistem_unique_students / num_overlap_student) * 100
