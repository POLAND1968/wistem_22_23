# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
data <- read_csv(file = "data/processed/wistem_attendance_clean.csv")

# Meeting Attendance Distribution ----
oct_14_data <- data %>%
  filter(date == "Oct 14th") %>%
  select(date, topic, num_meetings, graduation_year, federal_race, iep, program_504, esl) %>%
  mutate(
    num_meetings = as_factor(num_meetings), 
    graduation_year = as_factor(graduation_year), 
    federal_race = as_factor(federal_race)
  )

unique_colors <- c("lightpink2", "darkseagreen2", "lavender", "khaki", "lightcyan3")

## By Number of Days Attended ----
### Table ----
oct_14_data %>%
  select(date, num_meetings) %>%
  count(num_meetings) %>%
  rename(num_students = n) %>%
  kbl(caption = "Attendance Distribution By Number of Meetings Attended") %>%
  kable_styling()

### Graph ----
oct_14_num_meetings <- ggplot(data = oct_14_data, mapping = aes(x = num_meetings)) + 
  geom_bar(color = "azure4", fill = "lightcyan3") + 
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.8)) +  
  labs(title = "Attendance Distribution By Number of Meetings Attended", x = "Number of Meetings Attended in 2022-23", y = "Number of Students") + 
  theme_minimal()

## By Race ----
### Table ----
oct_14_data %>%
  select(date, federal_race) %>%
  count(federal_race) %>%
  rename(num_students = n) %>%
  kbl(caption = "Attendance Distribution By Federal Race") %>%
  kable_styling()

### Graph ----
oct_14_race <- ggplot(data = oct_14_data, mapping = aes(x = federal_race, fill = federal_race)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.8)) +  
  labs(title = "Attendance Distribution By Federal Race", x = "Federal Race", y = "Number of Students") + 
  theme_minimal()

## By Graduation Year ----
### Table ----
oct_14_data %>%
  select(date, graduation_year) %>%
  count(graduation_year) %>%
  rename(num_students = n) %>%
  kbl(caption = "Attendance Distribution By Federal Race") %>%
  kable_styling()

### Graph ----
oct_14_grad_year <- ggplot(data = oct_14_data, mapping = aes(x = graduation_year, fill = graduation_year)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.8)) +  
  labs(title = "Attendance Distribution By Federal Race", x = "Federal Race", y = "Number of Students") + 
  theme_minimal()

## By IEP ----
### Table ----
oct_14_data %>%
  select(date, iep) %>%
  count(iep) %>%
  rename(num_students = n) %>%
  kbl(caption = "Attendance Distribution by Having an IEP") %>%
  kable_styling()

### Graph ----
oct_14_iep <- ggplot(data = oct_14_data, mapping = aes(x = iep, fill = iep)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.6)) +  
  labs(title = "Attendance Distribution by Having an IEP", x = "IEP", y = "Number of Students") + 
  theme_minimal()

## By 504 ----
### Table ----
oct_14_data %>%
  select(date, program_504) %>%
  count(program_504) %>%
  rename(num_students = n) %>%
  kbl(caption = "Attendance Distribution by Having a 504 Plan") %>%
  kable_styling()

### Graph ----
oct_14_plan_504 <- ggplot(data = oct_14_data, mapping = aes(x = program_504, fill = program_504)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.6)) +  
  labs(title = "Attendance Distribution by Having a 504 Plan", x = "504 Plan", y = "Number of Students") + 
  theme_minimal()

## By ESL ----
### Table ----
oct_14_data %>%
  select(date, esl) %>%
  count(esl) %>%
  rename(num_students = n) %>%
  kbl(caption = "Attendance Distribution by Having ESL") %>%
  kable_styling()

### Graph ----
oct_14_esl <- ggplot(data = oct_14_data, mapping = aes(x = esl, fill = esl)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.6)) +  
  labs(title = "Attendance Distribution by Having ESL", x = "ESL", y = "Number of Students") + 
  theme_minimal()

#### Combined Table
oct_14_charac_table <- tibble(
  characteristic = c("IEP", "IEP", "504 Plan", "504 Plan", "ESL", "ESL"), 
  applicable = c("Yes", "No", "Yes", "No", "Yes", "No"), 
  num_students = c(2, 46, 6, 42, 1, 47)
)

oct_14_charac_table %>%
  kbl(caption = "Attendance Distribution by Additional Characteristics") %>%
  kable_styling()

#### Combined Graph
ggplot(data = oct_14_charac_table, mapping = aes(x = applicable, y = num_students, fill = characteristic)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = unique_colors) + 
  labs(title = "Extra Characteristics of Attending Students", x = "If It Applies to a Student", y = "Number of Students") + 
  theme_minimal()
