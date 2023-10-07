# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
data <- read_csv(file = "data/processed/wistem_attendance_clean.csv")

# Race of Students Attending Only One Meeting ---- 
group_race_one_attend <- data %>%
  filter(num_meetings == 1) %>%
  select(date, num_meetings, federal_race) %>%
  group_by(date, federal_race) %>%
  summarize(num_students = n()) %>%
  filter(!is.na(federal_race))

race_meeting_one_attend <- data %>%
  filter(num_meetings == 1) %>%
  select(date, federal_race) %>%
  count(federal_race) %>%
  filter(!is.na(federal_race)) %>%
  rename(num_students = n)

## Table ----
race_meeting_one_attend %>%
  kbl(caption = "Federal Race of Students Who Only Attended Once") %>%
  kable_styling()

## Graph ----
### Part 1
ggplot(data = race_meeting_one_attend, mapping = aes(x = federal_race, y = num_students)) + 
  geom_col(color = "azure4", fill = "lightcyan3") + 
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.8)) + 
  labs(title = "Federal Race of Students Who Only Attended Once") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  theme_minimal()

### Part 2
unique_colors <- c("lightpink2", "darkseagreen2", "lavender", "khaki", "lightcyan3")

ggplot(data = group_race_one_attend, mapping = aes(x = date, y = num_students, fill = federal_race)) + 
  geom_col() + 
  labs(title = "Federal Race of Students Who Only Attended Once") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  scale_fill_manual(values = unique_colors) + 
  theme_minimal() + 
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.8), size = 3)

ggplot(data = group_race_one_attend, mapping = aes(x = date, y = num_students, fill = federal_race)) + 
  geom_col() + 
  labs(title = "Federal Race of Students Who Only Attended Once") + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  scale_fill_manual(values = unique_colors) + 
  theme_minimal()

# Race Distribution ----
race_dist_unique <- data %>%
  select(federal_race, student_id, num_meetings) %>%
  unique() %>%
  select(-student_id) %>%
  count(federal_race) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

## Graph ----
ggplot(data = race_dist_unique, mapping = aes(x = federal_race, y = num_students, fill = federal_race)) +
  geom_col() + 
  scale_fill_manual(values = unique_colors) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5)) + 
  labs(
    title = "Federal Race Distribution", 
    x = "Federal Race", 
    y = "Number of Students"
  ) + 
  theme_minimal()

## Table ----
race_dist_unique %>%
  kbl(caption = "Federal Race Distribution (Unique)") %>%
  kable_styling()

## Only Two Categories ----
race_dist_unique_two <- data %>%
  select(federal_race, student_id, num_meetings) %>%
  unique() %>%
  select(-student_id)

race_dist_unique_two$federal_race <- recode(race_dist_unique_two$federal_race, "Asian" = "Non-White")
race_dist_unique_two$federal_race <- recode(race_dist_unique_two$federal_race, "Black" = "Non-White")
race_dist_unique_two$federal_race <- recode(race_dist_unique_two$federal_race, "LatinX" = "Non-White")
race_dist_unique_two$federal_race <- recode(race_dist_unique_two$federal_race, "T.M.O." = "Non-White")

race_dist_unique_two <- race_dist_unique_two %>%
  count(federal_race) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

### Graph ----
ggplot(data = race_dist_unique_two, mapping = aes(x = federal_race, y = num_students, fill = federal_race)) +
  geom_col() + 
  scale_fill_manual(values = unique_colors) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5)) + 
  labs(
    title = "Federal Race Distribution (Unique)", 
    subtitle = "By White and Non-White Students",
    x = "Federal Race", 
    y = "Number of Students"
  ) + 
  theme_minimal()

### Table ----
race_dist_unique_two %>%
  kbl(caption = "Federal Race Distribution With Only White and Non-White (Unique)") %>%
  kable_styling()

# Race Distribution Without One Timers ----
race_without_one <- data %>%
  filter(num_meetings != 1) %>%
  select(federal_race, student_id, num_meetings) %>%
  unique() %>%
  select(-student_id) %>%
  count(federal_race) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

### Graph ----
ggplot(data = race_without_one, mapping = aes(x = federal_race, y = num_students, fill = federal_race)) +
  geom_col() + 
  scale_fill_manual(values = unique_colors) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5)) + 
  labs(
    title = "Federal Race Distribution (Without One-Timers)", 
    subtitle = "By All Federal Race Categories (Unique Students)",
    x = "Federal Race", 
    y = "Number of Students"
  ) + 
  theme_minimal()

### Table ----
race_without_one %>%
  kbl(caption = "Federal Race Distribution Without One-Timers (Unique)") %>%
  kable_styling()

## Only Two Categories ----
race_without_one_two <- data %>%
  filter(num_meetings != 1) %>%
  select(federal_race, student_id, num_meetings) %>%
  unique() %>%
  select(-student_id)

race_without_one_two$federal_race <- recode(race_without_one_two$federal_race, "Asian" = "Non-White")
race_without_one_two$federal_race <- recode(race_without_one_two$federal_race, "Black" = "Non-White")
race_without_one_two$federal_race <- recode(race_without_one_two$federal_race, "LatinX" = "Non-White")
race_without_one_two$federal_race <- recode(race_without_one_two$federal_race, "T.M.O." = "Non-White")

race_without_one_two <- race_without_one_two %>%
  count(federal_race) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

### Graph ----
ggplot(data = race_without_one_two, mapping = aes(x = federal_race, y = num_students, fill = federal_race)) +
  geom_col() + 
  scale_fill_manual(values = unique_colors) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5)) + 
  labs(
    title = "Federal Race Distribution (Without One-Timers)", 
    subtitle = "By White and Non-White Students (Unique Students)",
    x = "Federal Race", 
    y = "Number of Students"
  ) + 
  theme_minimal()

### Table ----
race_without_one_two %>%
  kbl(caption = "Federal Race Distribution Without One-Timers (Only White and Non-White; Unique)") %>%
  kable_styling()

# Comparing ETHS with WiSTEM ----
eths_race <- tibble(
  federal_race = c("White", "Asian", "Black", "LatinX", "T.M.O."),
  num_students = c(1640, 198, 902, 724, 154), 
  prop = num_students / sum(num_students), 
  percent = prop * 100
) %>%
  mutate(program = "eths") %>%
  select(program, federal_race, num_students, prop, percent)

wistem_race <- data %>%
  select(name, student_id, federal_race) %>%
  unique() %>%
  count(federal_race) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  ) %>%
  mutate(program = "wistem") %>%
  select(program, federal_race, num_students, prop, percent) %>%
  arrange(desc(federal_race))

compare_race_dist <- full_join(eths_race, wistem_race)

## Graphs ----
### WiSTEM ----
wistem_race_dist <- ggplot(data = wistem_race, mapping = aes(x = federal_race, y = num_students, fill = federal_race)) + 
  geom_col() + 
  scale_fill_manual(values = unique_colors) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(
    title = "WiSTEM Race Distribution", 
    subtitle = "By All Federal Race Categories (Unique Students)",
    caption = "Source: WiSTEM 2022-2023",
    x = "Federal Race", 
    y = "Number of Students"
  ) + 
  theme_minimal()

### ETHS ----
eths_race_dist <- ggplot(data = eths_race, mapping = aes(x = federal_race, y = num_students, fill = federal_race)) + 
  geom_col() + 
  scale_fill_manual(values = unique_colors) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = "ETHS Race Distribution", 
       subtitle = "By All Federal Race Categories (Unique Students)",
       caption = "Source: WiSTEM 2022-2023",
       x = "Federal Race", 
       y = "Number of Students"
  ) + 
  theme_minimal()

## Combining ----
wistem_race_dist + eths_race_dist

## Table ----
compare_race_dist %>%
  kbl(caption = "Comparing Race Distributions (All Federal Race Categories) (Unique)") %>%
  kable_styling()

## Only Two Categories ----
eths_race_two <- tibble(
  federal_race = c("White", "Non-White"),
  num_students = c(1640, 1978), 
  prop = num_students / sum(num_students), 
  percent = prop * 100
) %>%
  mutate(program = "eths") %>%
  select(program, federal_race, num_students, prop, percent)

wistem_race_two <- data %>%
  select(name, student_id, federal_race) %>%
  unique() 

wistem_race_two$federal_race <- recode(wistem_race_two$federal_race, "Asian" = "Non-White")
wistem_race_two$federal_race <- recode(wistem_race_two$federal_race, "Black" = "Non-White")
wistem_race_two$federal_race <- recode(wistem_race_two$federal_race, "LatinX" = "Non-White")
wistem_race_two$federal_race <- recode(wistem_race_two$federal_race, "T.M.O." = "Non-White")

wistem_race_two <- wistem_race_two %>%
  count(federal_race) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  ) %>%
  mutate(program = "wistem") %>%
  select(program, federal_race, num_students, prop, percent) %>%
  arrange(desc(federal_race))

compare_race_dist_two <- full_join(eths_race_two, wistem_race_two)

### Graphs ----
#### WiSTEM ----
wistem_race_dist_two <- ggplot(data = wistem_race_two, mapping = aes(x = federal_race, y = num_students, fill = federal_race)) + 
  geom_col() + 
  scale_fill_manual(values = unique_colors) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  geom_text(aes(label = sprintf("%.2f%%", percent)), position = position_stack(vjust = 0.5)) + 
  labs(
    title = "WiSTEM Race Distribution", 
    subtitle = "By White and Non-White Students (Unique Students)",
    caption = "Source: WiSTEM 2022-2023",
    x = "Federal Race", 
    y = "Number of Students"
  ) + 
  theme_minimal()

#### ETHS ----
eths_race_dist_two <- ggplot(data = eths_race_two, mapping = aes(x = federal_race, y = num_students, fill = federal_race)) + 
  geom_col() + 
  scale_fill_manual(values = unique_colors) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  geom_text(aes(label = sprintf("%.2f%%", percent)), position = position_stack(vjust = 0.5)) + 
  labs(title = "ETHS Race Distribution", 
       subtitle = "By White and Non-White Students (Unique Students)",
       caption = "Source: WiSTEM 2022-2023",
       x = "Federal Race", 
       y = "Number of Students"
  ) + 
  theme_minimal()

### Combining ----
wistem_race_dist_two / eths_race_dist_two

### Table ----
compare_race_dist_two %>%
  kbl(caption = "Comparing Race Distributions (White and Non-White) (Unique)") %>%
  kable_styling()

# Race and Number of Meetings ----
race_distribution <- data %>%
  select(student_id, date, topic, num_meetings, graduation_year, federal_race) %>%
  mutate(
    graduation_year = as_factor(graduation_year), 
    num_meetings = as_factor(num_meetings), 
    federal_race = as_factor(federal_race)
  )

race_num_meetings <- race_distribution %>%
  select(student_id, num_meetings, graduation_year, federal_race) %>%
  unique() %>%
  select(-student_id, -graduation_year)

race_num_meetings_table <- race_num_meetings %>%
  group_by(federal_race, num_meetings) %>%
  summarize(num_students = n())

## Graph ----
race_num_meetings_graph <- ggplot(data = race_num_meetings, mapping = aes(x = num_meetings, fill = federal_race)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  labs(
    title = "Federal and Number of Meetings",
    subtitle = "By All Federal Race Categories (Unique Students)",
    caption = "Source: WiSTEM 2022-2023",
    x = "Number of Meetings Attended", 
    y = "Number of Students"
  ) + 
  theme_minimal()

## Table ----
race_num_meetings_table %>%
  kbl(caption = "Race and Number of Meetings Attended (All Federal Race Categories) (Unique)") %>%
  kable_styling()

## Only Two Categories ----
race_num_meetings_two <- race_num_meetings

race_num_meetings_two$federal_race <- recode(race_num_meetings_two$federal_race, "Asian" = "Non-White")
race_num_meetings_two$federal_race <- recode(race_num_meetings_two$federal_race, "Black" = "Non-White")
race_num_meetings_two$federal_race <- recode(race_num_meetings_two$federal_race, "LatinX" = "Non-White")
race_num_meetings_two$federal_race <- recode(race_num_meetings_two$federal_race, "T.M.O." = "Non-White")

race_num_meetings_table_two <- race_num_meetings_two %>%
  group_by(federal_race, num_meetings) %>%
  summarize(num_students = n())

race_num_meetings_graph_two <- ggplot(data = race_num_meetings_two, mapping = aes(x = num_meetings, fill = federal_race)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  labs(
    title = "Federal and Number of Meetings",
    subtitle = "By White and Non-White (Unique Students)",
    caption = "Source: WiSTEM 2022-2023",
    x = "Number of Meetings Attended", 
    y = "Number of Students"
  ) + 
  theme_minimal()

race_num_meetings_graph
race_num_meetings_graph_two

### Table ----
race_num_meetings_table_two %>%
  kbl(caption = "Race and Number of Meetings Attended (White and Non-White) (Unique)") %>%
  kable_styling()

# Race and Topic ----
race_topic <- race_distribution %>%
  select(federal_race, topic)

race_topic_table <- race_topic %>%
  group_by(federal_race, topic) %>%
  summarize(num_students = n())

## Graph ----
race_topic_graph <- ggplot(data = race_topic, mapping = aes(x = topic, fill = federal_race)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(
    title = "Federal Race and Topic",
    subtitle = "By All Federal Race Categories (With Repeats)",
    caption = "Source: WiSTEM 2022-2023",
    x = "Topic", 
    y = "Number of Students"
  ) + 
  theme_minimal()

## Table ----
race_topic_table %>%
  kbl(caption = "Race and Topic (All Federal Race Categories) (Unique)") %>%
  kable_styling()

## Only Two Categories ----
race_topic_two <- race_topic

race_topic_two$federal_race <- recode(race_topic_two$federal_race, "Asian" = "Non-White")
race_topic_two$federal_race <- recode(race_topic_two$federal_race, "Black" = "Non-White")
race_topic_two$federal_race <- recode(race_topic_two$federal_race, "LatinX" = "Non-White")
race_topic_two$federal_race <- recode(race_topic_two$federal_race, "T.M.O." = "Non-White")

race_topic_table_two <- race_topic_two %>%
  group_by(federal_race, topic) %>%
  summarize(num_students = n())

### Graph ----
race_topic_graph_two <- ggplot(data = race_topic_two, mapping = aes(x = topic, fill = federal_race)) + 
  geom_bar() + 
  scale_fill_manual(values = unique_colors) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(
    title = "Federal Race and Topic",
    subtitle = "By White and Non-White (With Repeats)",
    caption = "Source: WiSTEM 2022-2023",
    x = "Topic", 
    y = "Number of Students"
  ) + 
  theme_minimal()

race_topic_graph
race_topic_graph_two

### Table ----
race_topic_table_two %>%
  kbl(caption = "Race and Topic (White and Non-White) (Unique)") %>%
  kable_styling()
