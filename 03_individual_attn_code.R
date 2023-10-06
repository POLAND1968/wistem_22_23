# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
data <- read_csv(file = "data/processed/wieng_attendance_clean.csv")

names <- data %>% # 32 unique students
  select(name, student_id) %>%
  unique()

# Background ----
## Number per meeting ----
data$date <- factor(data$date, levels = c("May 12th", "May 9th", "May 5th", "Apr 28th", "Apr 21st", "Apr 14th", "Mar 17th", "Mar 10th", "Mar 3rd", "Feb 24th", "Feb 17th", "Feb 10th", "Feb 3rd", "Jan 27th", "Jan 20th", "Dec 16th", "Dec 2nd", "Nov 18th", "Nov 4th", "Oct 28th", "Oct 21st", "Oct 14th"))

# Number of Meetings Attendance by Individual ----
indiv_atten <- data %>%
  count(name) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100
  ) %>% 
  arrange(desc(percent))

cat_indiv_atten <- indiv_atten %>%
  count(n) %>%
  rename(num_meetings = n, num_students = nn) %>%
  mutate(
    proportion = num_students / sum(num_students), 
    percent = proportion * 100
  ) %>%
  mutate(
    num_meetings = as_factor(num_meetings), 
    num_students = as_factor(num_students)
  )

## Table ----
cat_indiv_atten %>% 
  arrange(desc(percent)) %>%
  kbl(caption = "Number of Meetings Attended by Unique Students") %>%
  kable_styling()

## Graph ---
ggplot(data = cat_indiv_atten, mapping = aes(x = num_meetings, y = num_students)) + 
  geom_col(color = "azure4", fill = "lightcyan3") + 
  labs(title = "Number of Meetings Attended by Unique Students", x = "Number of Meetings", y = "Number of Students") + 
  theme_minimal()

## Adding Number of Meetings Attended to Overall Data ----
meeting_num_by_student <- indiv_atten %>%
  select(name, n) %>%
  rename(num_meetings = n)

data <- full_join(data, meeting_num_by_student)
write_csv(data, file = "data/processed/wieng_attendance_clean.csv")
data <- read_csv(file = "data/processed/wieng_attendance_clean.csv")

# Meetings Attended by Students With One Meeting ----
meeting_one_attend <- data %>%
  filter(num_meetings == 1) %>%
  select(date, topic, num_meetings) %>%
  count(date) %>%
  rename(num_students = n)

meeting_one_attend %>%
  summarize(sum(num_students))

## Table ----
meeting_one_attend %>%
  kbl(caption = "Number of Student Per Meetings Who Only Attended Once") %>%
  kable_styling()

## Graph ---
ggplot(data = meeting_one_attend, mapping = aes(x = date, y = num_students)) + 
  geom_col(color = "azure4", fill = "lightcyan3") + 
  labs(title = "Number of Student Per Meetings Who Only Attended Once") + 
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.8)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  theme_minimal()

# By Characteristic ----
## Federal Race ----
one_meeting_fed_race <- data %>%
  filter(num_meetings == 1) %>%
  select(num_meetings, federal_race) %>%
  mutate(num_meetings = as_factor(num_meetings)) %>%
  count(federal_race) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

ggplot(data = one_meeting_fed_race, mapping = aes(x = federal_race, y = num_students, fill = federal_race)) +
  geom_col() + 
  scale_fill_manual(values = unique_colors) +
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(
    title = "Number of Student Per Meetings Who Only Attended Once", 
    x = "Federal Race", 
    y = "Number of Students"
  ) + 
  theme_minimal()

one_meeting_fed_race %>%
  kbl(caption = "Federal Race and Students Who Attended One Meeting") %>%
  kable_styling()

### Only Two Categories ----
one_meeting_fed_race_two <- data %>%
  filter(num_meetings == 1) %>%
  select(num_meetings, federal_race) %>%
  mutate(num_meetings = as_factor(num_meetings))

one_meeting_fed_race_two$federal_race <- recode(one_meeting_fed_race_two$federal_race, "Asian" = "Non-White")
one_meeting_fed_race_two$federal_race <- recode(one_meeting_fed_race_two$federal_race, "Black" = "Non-White")
one_meeting_fed_race_two$federal_race <- recode(one_meeting_fed_race_two$federal_race, "LatinX" = "Non-White")
one_meeting_fed_race_two$federal_race <- recode(one_meeting_fed_race_two$federal_race, "T.M.O." = "Non-White")

one_meeting_fed_race_two <- one_meeting_fed_race_two %>%
  count(federal_race) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

ggplot(data = one_meeting_fed_race_two, mapping = aes(x = federal_race, y = num_students, fill = federal_race)) +
  geom_col() + 
  scale_fill_manual(values = unique_colors) +
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.5)) +
  labs(
    title = "Number of Student Per Meetings Who Only Attended Once", 
    subtitle = "By Federal Race",
    x = "Federal Race", 
    y = "Number of Students"
  ) + 
  theme_minimal()

## Graduation Year ----
one_meeting_grad_year <- data %>%
  filter(num_meetings == 1) %>%
  select(num_meetings, graduation_year) %>%
  mutate(graduation_year = as_factor(graduation_year)) %>%
  count(graduation_year) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

ggplot(data = one_meeting_grad_year, mapping = aes(x = graduation_year, y = num_students, fill = graduation_year)) +
  geom_col() + 
  scale_fill_manual(values = unique_colors) +
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.5)) +
  labs(
    title = "Number of Student Per Meetings Who Only Attended Once", 
    x = "Graduation Year", 
    y = "Number of Students"
  ) + 
  theme_minimal()

one_meeting_grad_year %>%
  kbl(caption = "Graduation Year and Students Who Attended One Meeting") %>%
  kable_styling()

## Topic ----
one_meeting_topic <- data %>%
  filter(num_meetings == 1) %>%
  select(num_meetings, topic) %>%
  mutate(topic = as_factor(topic)) %>%
  count(topic) %>%
  rename(num_students = n) %>%
  mutate(
    prop = num_students / sum(num_students), 
    percent = prop * 100
  )

ggplot(data = one_meeting_topic, mapping = aes(x = topic, y = num_students, fill = topic)) +
  geom_col() + 
  scale_fill_manual(values = unique_colors) +
  geom_text(aes(label = num_students), position = position_stack(vjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(
    title = "Number of Student Per Meetings Who Only Attended Once", 
    x = "Topic", 
    y = "Number of Students"
  ) + 
  theme_minimal()

one_meeting_grad_year %>%
  kbl(caption = "Topic and Students Who Attended One Meeting") %>%
  kable_styling()
