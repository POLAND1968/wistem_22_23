# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
data <- read_csv(file = "data/processed/wistem_attendance_clean.csv")

# Student Demographic Data Cleaned ----
student_dem <- read_csv(file = "data/raw/student_dem.csv") %>%
  janitor::clean_names() %>%
  select(-gender) %>%
  rename(program_504 = x504)

student_dem$federal_race <- recode(student_dem$federal_race, "White" = "White")
student_dem$federal_race <- recode(student_dem$federal_race, "Hispanic / Latino" = "LatinX")
student_dem$federal_race <- recode(student_dem$federal_race, "Black or African American" = "Black")
student_dem$federal_race <- recode(student_dem$federal_race, "Two or More Races" = "T.M.O.")

student_dem <- student_dem %>%
  mutate(gender = "female")

write_csv(student_dem, file = "data/processed/student_dem.csv")
student_dem <- read_csv(file = "data/processed/student_dem.csv")

## Adding Characteristics to Overall Data ----
student_charac <- student_dem %>%
  select(student_id, graduation_year, federal_race, iep, program_504, esl, gender) %>%
  unique()

## Saving Data ----
data <- left_join(data, student_charac)
write_csv(data, file = "data/processed/wistem_attendance_clean.csv")
data <- read_csv(file = "data/processed/wistem_attendance_clean.csv")

## Checking Students Missing Characteristics ----
charac_miss <- data %>%
  filter(is.na(federal_race)) %>%
  select(date, name, student_id, graduation_year, federal_race, iep, program_504, esl) %>%
  kbl(caption = "Missing Student Information") %>%
  kable_styling() # recoded three student_id values manually in the original, raw, dataset

### Recoded Manually ----
# code didn't work for some reason
data$student_id <- recode(data$student_id, "999723" = "999725") # Rachael Rubin
data$student_id <- recode(data$student_id, "845370" = "845730") # Samantha Daly-Short
data$student_id <- recode(data$student_id, "973056" = "873056") # Abigael Owomoyela-Olusesi

# IEP ----
charac_distribution <- data %>%
  select(iep, program_504, esl, student_id, date, topic, num_meetings, graduation_year, federal_race) %>%
  mutate(
    graduation_year = as_factor(graduation_year), 
    num_meetings = as_factor(num_meetings), 
    federal_race = as_factor(federal_race)
  )

iep_dist <- charac_distribution %>%
  select(student_id, iep, num_meetings, graduation_year, federal_race) %>%
  unique() %>%
  count(iep) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

## Graph ----
ggplot(data = iep_dist, mapping = aes(x = iep, y = num_students, fill = iep)) + 
  geom_col() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.5)) + 
  labs(
    title = "Distribution of Students Having an IEP or Not",
    subtitle = "Unique Students",
    caption = "Source: WiSTEM 2022-2023",
    x = "If It Applies to a Student", 
    y = "Number of Students"
  ) + 
  theme_minimal()

## Table ----
iep_dist %>%
  kbl(caption = "Distribution of Students Having an IEP or Not (Unique)") %>%
  kable_styling()

# 504 Plan ----
program_504_dist <- charac_distribution %>%
  select(student_id, program_504, num_meetings, graduation_year, federal_race) %>%
  unique() %>%
  count(program_504) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

## Graph ----
ggplot(data = program_504_dist, mapping = aes(x = program_504, y = num_students, fill = program_504)) + 
  geom_col() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.5)) + 
  labs(
    title = "Distribution of Students Having a 504 Plan or Not",
    subtitle = "Unique Students",
    caption = "Source: WiSTEM 2022-2023",
    x = "If It Applies to a Student", 
    y = "Number of Students"
  ) + 
  theme_minimal()

## Table ----
program_504_dist %>%
  kbl(caption = "Distribution of Students Having an IEP or Not (Unique)") %>%
  kable_styling()

# ESL ----
esl_dist <- charac_distribution %>%
  select(student_id, esl, num_meetings, graduation_year, federal_race) %>%
  unique() %>%
  count(esl) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

## Graph ----
ggplot(data = esl_dist, mapping = aes(x = esl, y = num_students, fill = esl)) + 
  geom_col() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.5)) + 
  labs(
    title = "Distribution of Students Having ESL or Not",
    subtitle = "Unique Students",
    caption = "Source: WiSTEM 2022-2023",
    x = "If It Applies to a Student", 
    y = "Number of Students"
  ) + 
  theme_minimal()

## Table ----
esl_dist %>%
  kbl(caption = "Distribution of Students Having an IEP or Not (Unique)") %>%
  kable_styling()
