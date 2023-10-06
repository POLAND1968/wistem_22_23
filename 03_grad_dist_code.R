# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
data <- read_csv(file = "data/processed/wieng_attendance_clean.csv")

# Total Grade Distribution ----
grade_distribution <- data %>%
  select(student_id, date, topic, num_meetings, graduation_year, federal_race) %>%
  filter(!is.na(graduation_year)) %>%
  mutate(
    graduation_year = as_factor(graduation_year), 
    num_meetings = as_factor(num_meetings), 
    federal_race = as_factor(federal_race)
  )

unique_colors <- c("lightpink2", "darkseagreen2", "lavender", "khaki", "lightcyan3")

## Repeated ----
### Table ----
grade_repeat <- grade_distribution %>%
  count(graduation_year) %>%
  rename(num_students = n)

grade_repeat %>%
  kbl(caption = "Graduation Year of All Students (With Repeats)") %>%
  kable_styling()

### Graph ----
ggplot(data = grade_repeat, mapping = aes(x = graduation_year, y = num_students, fill = graduation_year)) + 
  geom_col() + 
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.8)) + 
  scale_fill_manual(values = unique_colors) + 
  labs(title = "Graduation Year of All Students (With Repeats)", x = "Graduation Year", y = "Number of Students") + 
  theme_minimal()

## Unique ----
### Table ----
grade_unique <- grade_distribution %>%
  select(student_id, num_meetings, graduation_year, federal_race) %>%
  unique() %>%
  count(graduation_year) %>%
  rename(num_students = n)

grade_unique %>%
  kbl(caption = "Graduation Year of All Students (Without Repeats)") %>%
  kable_styling()

### Graph ----
ggplot(data = grade_unique, mapping = aes(x = graduation_year, y = num_students, fill = graduation_year)) + 
  geom_col() + 
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.8)) + 
  scale_fill_manual(values = unique_colors) + 
  labs(title = "Graduation Year of All Students (Without Repeats)", x = "Graduation Year", y = "Number of Students") + 
  theme_minimal()

# Grade Level and Number of Meetings Attended ----
## Table ----
grade_num_meetings <- grade_distribution %>%
  select(student_id, num_meetings, graduation_year, federal_race) %>%
  unique() %>%
  select(-student_id)

grade_num_meetings_table <- grade_distribution %>%
  select(student_id, num_meetings, graduation_year, federal_race) %>%
  unique() %>%
  select(-student_id, -federal_race) %>%
  group_by(graduation_year, num_meetings) %>%
  summarize(num_students = n())

grade_num_meetings_table <- pivot_wider(grade_num_meetings_table, names_from = graduation_year, values_from = num_students)

grade_num_meetings_table %>%
  kbl(caption = "Graduation Year and Number of Meetings Attended of Unique Students") %>%
  kable_styling()

## Graph ----
### Part 1
ggplot(data = grade_num_meetings, mapping = aes(x = num_meetings, fill = graduation_year)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 3) +  
  labs(title = "Graduation Year and Number of Meetings Attended", x = "Number of Meetings Attended in 2022-23", y = "Number of Students") + 
  theme_minimal()

### Part 2
ggplot(data = grade_num_meetings, mapping = aes(x = num_meetings, fill = graduation_year)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  labs(title = "Graduation Year and Number of Meetings Attended", x = "Number of Meetings Attended in 2022-23", y = "Number of Students") + 
  theme_minimal()

# Grade Level and Federal Race ----
## Table ----
grade_race <- grade_distribution %>%
  select(student_id, num_meetings, graduation_year, federal_race) %>%
  unique() %>%
  select(-student_id)

grade_race_table <- grade_distribution %>%
  select(student_id, num_meetings, graduation_year, federal_race) %>%
  unique() %>%
  select(-student_id, -num_meetings) %>%
  group_by(graduation_year, federal_race) %>%
  summarize(num_students = n())

grade_race_table_wider <- pivot_wider(grade_race_table, names_from = federal_race, values_from = num_students) %>%
  select(graduation_year, White, Black, Asian, LatinX, T.M.O.)

grade_race_table_wider %>%
  kbl(caption = "Graduation Year and Federal Race of Unique Students") %>%
  kable_styling()

## Graph ----
### Part 1
ggplot(data = grade_race, mapping = aes(x = federal_race, fill = graduation_year)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 3) +  
  labs(title = "Graduation Year and Federal Race", x = "Federal Race", y = "Number of Students") + 
  theme_minimal()

### Part 2
ggplot(data = grade_race, mapping = aes(x = federal_race, fill = graduation_year)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  labs(title = "Graduation Year and Federal Race", x = "Federal Race", y = "Number of Students") + 
  theme_minimal()

### Part 3
ggplot(data = grade_race, mapping = aes(x = federal_race, fill = graduation_year)) + 
  geom_bar(position = "dodge") + 
  scale_fill_manual(values = unique_colors) + 
  labs(title = "Graduation Year and Federal Race", x = "Federal Race", y = "Number of Students") + 
  theme_minimal()

### Part 4
ggplot(data = grade_race, mapping = aes(x = federal_race, fill = graduation_year)) + 
  geom_bar(position = "dodge") + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Graduation Year and Federal Race", x = "Federal Race", y = "Number of Students") + 
  theme_minimal()

## Only Two Categories ----
grade_race_two <- grade_distribution %>%
  select(student_id, num_meetings, graduation_year, federal_race) %>%
  unique() %>%
  select(-student_id)

grade_race_two$federal_race <- recode(grade_race_two$federal_race, "Asian" = "Non-White")
grade_race_two$federal_race <- recode(grade_race_two$federal_race, "Black" = "Non-White")
grade_race_two$federal_race <- recode(grade_race_two$federal_race, "LatinX" = "Non-White")
grade_race_two$federal_race <- recode(grade_race_two$federal_race, "T.M.O." = "Non-White")

grade_race_table_two <- grade_distribution %>%
  select(student_id, num_meetings, graduation_year, federal_race)

grade_race_table_two$federal_race <- recode(grade_race_table_two$federal_race, "Asian" = "Non-White")
grade_race_table_two$federal_race <- recode(grade_race_table_two$federal_race, "Black" = "Non-White")
grade_race_table_two$federal_race <- recode(grade_race_table_two$federal_race, "LatinX" = "Non-White")
grade_race_table_two$federal_race <- recode(grade_race_table_two$federal_race, "T.M.O." = "Non-White")

grade_race_table_two <- grade_race_table_two %>%
  unique() %>%
  select(-student_id, -num_meetings) %>%
  group_by(graduation_year, federal_race) %>%
  summarize(num_students = n()) %>%
  mutate(num_students = num_students)

grade_race_table_two_wider <- pivot_wider(grade_race_table_two, names_from = federal_race, values_from = num_students)

grade_race_table_two_wider %>%
  kbl(caption = "Graduation Year and Federal Race (Unique)") %>%
  kable_styling()

### Graph ----
ggplot(data = grade_race_two, mapping = aes(x = federal_race, fill = graduation_year)) + 
  geom_bar(position = "dodge") + 
  scale_fill_manual(values = unique_colors) + 
  labs(title = "Graduation Year and Federal Race (Unique)", x = "Federal Race", y = "Number of Students") + 
  theme_minimal()

# Grade Level and Meeting Topic ----
grade_topic <- grade_distribution %>%
  group_by(topic, graduation_year) %>%
  summarize(num_students = n())

## Graph ----
ggplot(data = grade_topic, mapping = aes(x = topic, y = num_students, fill = graduation_year)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = unique_colors) + 
  labs(title = "Graduation Year and Topic (With Repeats)", x = "Federal Race", y = "Number of Students") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_minimal()

## Table ----
grade_topic_graph <- pivot_wider(grade_topic, names_from = topic, values_from = num_students)

grade_topic_graph %>%
  kbl(caption = "Graduation Year and Topic of All Students (With Repeats)") %>%
  kable_styling()

# Comparing to ETHS Grade Levels ----
eths_grade <- tibble(
  graduation_year = c(2026, 2025, 2024, 2023), 
  num_students = c(914, 995, 876, 905), 
  prop = num_students / sum(num_students), 
  percent = prop * 100
) %>%
  mutate(program = "eths") %>%
  select(program, graduation_year, num_students, prop, percent) %>%
  mutate(graduation_year = as_factor(graduation_year))

eths_grade %>%
  kbl(caption = "Grade Distribution at ETHS") %>%
  kable_styling()

wieng_grade <- data %>%
  select(name, student_id, graduation_year) %>%
  unique() %>%
  count(graduation_year) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  ) %>%
  mutate(program = "wieng") %>%
  select(program, graduation_year, num_students, prop, percent) %>%
  arrange(desc(graduation_year)) %>%
  mutate(graduation_year = as_factor(graduation_year))

wieng_grade %>%
  kbl(caption = "Grade Distribution at wieng") %>%
  kable_styling()

compare_grade_dist <- full_join(eths_grade, wieng_grade)

## Graph ---
### wieng ----
wieng_grade_dist <- ggplot(data = wieng_grade, mapping = aes(x = graduation_year, y = num_students, fill = graduation_year)) + 
  geom_col() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(aes(label = sprintf("%.2f%%", percent)), position = position_stack(vjust = 0.5)) + 
  labs(title = "ETHS Grade Distribution", x = "Graduation Year", y = "Number of Students") + 
  theme_minimal()

### ETHS ----
eths_grade_dist <- ggplot(data = eths_grade, mapping = aes(x = graduation_year, y = num_students, fill = graduation_year)) + 
  geom_col() + 
  scale_fill_manual(values = unique_colors) + 
  geom_text(aes(label = sprintf("%.2f%%", percent)), position = position_stack(vjust = 0.5)) + 
  labs(title = "ETHS Grade Distribution", x = "Graduation Year", y = "Number of Students") + 
  theme_minimal()

## Combining Table ----
compare_grade_dist %>%
  kbl(caption = "Comparing Grade Distributions") %>%
  kable_styling()
