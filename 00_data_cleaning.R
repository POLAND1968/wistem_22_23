# Load Packages
library(tidyverse)
library(janitor)
library(readr)

set.seed(1968)

# Load Data Set
data <- read_csv(file = "data/raw/wistem_student_forms_raw.csv")

# Cleaning
## Remove Second Row
data <- data[-2,]

## Make First Row the Variable Names
data <- janitor::row_to_names(data, 1, remove_rows_above = FALSE)
data <- data %>%
  janitor::clean_names()

data$succesful <- recode(data$succesful, "Strongly Disagree" = "Strongly disagree") 
data$member <- recode(data$member, "Strongly Disagree" = "Strongly disagree") 
data$career <- recode(data$career, "Strongly Disagree" = "Strongly disagree") 
data$identity <- recode(data$identity, "Strongly Disagree" = "Strongly disagree") 

# Save Clean Data Set
write_csv(data, file = "data/processed/wistem_student_forms_clean.csv")
data <- read_csv(file = "data/processed/wistem_student_forms_clean.csv")

# Separate Data Set 
## Pre Data Set
pre_data <- data %>%
  filter(survey_type == "Pre")

write_csv(pre_data, file = "data/processed/wistem_pre_data.csv")
pre_data <- read_csv(file = "data/processed/wistem_pre_data.csv")

## Post Data Set 
post_data <- data %>%
  filter(survey_type == "Post")

write_csv(post_data, file = "data/processed/wistem_post_data.csv")
post_data <- read_csv(file = "data/processed/wistem_post_data.csv")
