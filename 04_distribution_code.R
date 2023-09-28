# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
data <- read_csv(file = "data/processed/wieng_exit_slips_clean.csv") 

# Overall opinion (483 total exit slips)
## interesting ----
### Graph ----
interesting_order <- read_csv(file = "data/processed/wieng_exit_slips_clean.csv") %>%
  filter(!is.na(interesting))
interesting_order$interesting <- factor(interesting_order$interesting, levels = c("Somewhat", "Fairly", "Very"))

percentage_data <- interesting_order %>%
  group_by(interesting) %>%
  summarize(percentage = n() / nrow(interesting_order) * 100)

ggplot(data = interesting_order, mapping = aes(x = interesting)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  geom_text(data = percentage_data, aes(x = interesting, y = percentage, label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Distribution for interesting")

### Table ----
data %>%
  filter(!is.na(interesting)) %>%
  count(interesting) %>%
  mutate(
    proportion = n / sum(n), 
    percent = (n / sum(n)) * 100 
  ) %>%
  arrange(desc(n)) %>%
  kbl(caption = "Distribution for interesting") %>%
  kable_styling()

## included ----
### Graph ----
included_order <- read_csv(file = "data/processed/wieng_exit_slips_clean.csv") %>%
  filter(!is.na(included))
included_order$included <- factor(included_order$included, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

percentage_data <- included_order %>%
  group_by(included) %>%
  summarize(percentage = n() / nrow(included_order) * 100)

ggplot(data = included_order, mapping = aes(x = included)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  geom_text(data = percentage_data, aes(x = included, y = percentage, label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) + 
  labs(title = "Distribution for included")

### Table ----
data %>%
  filter(!is.na(included)) %>%
  count(included) %>%
  mutate(
    proportion = n / sum(n), 
    percent = (n / sum(n)) * 100 
  ) %>%
  arrange(desc(n)) %>%
  kbl(caption = "Distribution for included") %>%
  kable_styling()

## belong ----
### Graph ----
belong_order <- read_csv(file = "data/processed/wieng_exit_slips_clean.csv") %>%
  filter(!is.na(belong))
belong_order$belong <- factor(belong_order$belong, levels = c("Strongly disagree", "Neutral", "Agree", "Strongly agree"))

percentage_data <- belong_order %>%
  group_by(belong) %>%
  summarize(percentage = n() / nrow(belong_order) * 100)

ggplot(data = belong_order, mapping = aes(x = belong)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  geom_text(data = percentage_data, aes(x = belong, y = percentage, label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) +  
  labs(title = "Distribution for belong")

### Table ----
data %>%
  filter(!is.na(belong)) %>%
  count(belong) %>%
  mutate(
    proportion = n / sum(n), 
    percent = (n / sum(n)) * 100 
  ) %>%
  arrange(desc(n)) %>%
  kbl(caption = "Distribution for belong") %>%
  kable_styling()
