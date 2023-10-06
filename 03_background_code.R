# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
data <- read_csv(file = "data/processed/wieng_attendance_clean.csv")

# Background ----
## Number per meeting ----
data$date <- factor(data$date, levels = c("May 12th", "May 9th", "May 5th", "Apr 28th", "Apr 21st", "Apr 14th", "Mar 17th", "Mar 10th", "Mar 3rd", "Feb 24th", "Feb 17th", "Feb 10th", "Feb 3rd", "Jan 27th", "Jan 20th", "Dec 16th", "Dec 2nd", "Nov 18th", "Nov 4th", "Oct 28th", "Oct 21st", "Oct 14th")) 

### Graph ----
ggplot(data = data, mapping = aes(y = date)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.9)) +
  labs(title = "Number of Attendance Per Meeting") + 
  theme_minimal()

### Table ----
data %>%
  count(date) %>%
  mutate(
    proportion = n / sum(n), 
    percent = (n / sum(n)) * 100 
  ) %>%
  kbl(caption = "Number of Attendance Per Meeting") %>%
  kable_styling()

## Total Month Attendance ----
data_split_date <- data[c('month', 'day')] <- str_split_fixed(data$date, ' ', 2) %>%
  as_tibble() %>%
  rename(month = V1, day = V2) %>%
  mutate(day = substr(day, 1, 2))

data_split_date$month <- factor(data_split_date$month, levels = c("May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct", "Sep")) 

### Graph ----
ggplot(data = data_split_date, mapping = aes(y = month)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.9)) +
  labs(title = "Number of Attendance In Each Month") + 
  theme_minimal()

### Table ----
data_split_date %>%
  count(month) %>%
  mutate(
    proportion = n / sum(n), 
    percent = (n / sum(n)) * 100 
  ) %>%
  kbl(caption = "Number of Attedance In Each Month") %>%
  kable_styling()

## Total Attendance by Topic ----
### Graph ----
ggplot(data = data, mapping = aes(y = topic)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.6)) +
  labs(title = "Total Attendance Per Topic Type") + 
  theme_minimal()

### Table ----
topic_attn_total <- data %>%
  count(topic) %>%
  rename(total = n)

topic_attn_total %>%
  mutate(
    proportion = total / sum(total), 
    percent = (total / sum(total)) * 100 
  ) %>%
  kbl(caption = "Total Attendance Per Topic Type") %>%
  kable_styling()

## Average Attendance by Topic ----
topic_meeting_total <- data %>%
  select(date, topic) %>%
  unique() %>%
  count(topic) %>%
  rename(meeting_count = n)

avg_topic_data <- full_join(topic_meeting_total, topic_attn_total) %>%
  mutate(
    meeting_avg = total / meeting_count, 
    total_avg = 229 / 22
  )

### Graph ----
ggplot(data = avg_topic_data, mapping = aes(x = topic, y = meeting_avg)) + 
  geom_col(color = "azure4", fill = "lightcyan3") + 
  labs(title = "Average Attendance per Meeting Topic Compared to Overall Average") + 
  geom_hline(yintercept = 229 / 22) +
  geom_text(aes(label = meeting_avg), position = position_stack(vjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  theme_minimal()

### Table ----
mutate_better_than <- function(avg_topic_data) {
  avg_topic_data %>%
    mutate(benchmark = ifelse(meeting_avg > total_avg, "yes", "no"))
}

avg_topic_data <- mutate_better_than(avg_topic_data)

avg_topic_data %>%
  kbl(caption = "Average Attendance Per Meeting Topic") %>%
  kable_styling()
