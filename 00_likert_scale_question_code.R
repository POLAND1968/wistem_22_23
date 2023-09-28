# Load Packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(readr)
library(kableExtra)

set.seed(1968)

# Load Data Set ----
data <- read_csv(file = "data/processed/wistem_student_forms_clean.csv")
pre_data <- read_csv(file = "data/processed/wistem_pre_data.csv")
post_data <- read_csv(file = "data/processed/wistem_post_data.csv")

# General Distribution ----
data %>%
  count(survey_type) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100
  ) %>%
  kbl(caption = "Distribution of Pre and Post Forms") %>%
  kable_styling()

# work_hard ----
## Pre ----
pre_wh_order <- read_csv(file = "data/wistem_pre_data.csv") %>%
  filter(!is.na(work_hard))
pre_wh_order$work_hard <- factor(pre_wh_order$work_hard, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

pre_wh_plot <- ggplot(data = pre_wh_order, mapping = aes(x = work_hard)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Pre Test")

pre_wh_table <- pre_wh_order %>%
  count(work_hard) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  )

## Post ----
post_wh_order <- read_csv(file = "data/wistem_post_data.csv") %>%
  filter(!is.na(work_hard))
post_wh_order$work_hard <- factor(post_wh_order$work_hard, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

post_wh_plot <- ggplot(data = post_wh_order, mapping = aes(x = work_hard)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Post Test")

post_wh_table <- post_wh_order %>%
  count(work_hard) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  )

## Compare ----
### Plot
pre_wh_plot + post_wh_plot

### Table
wh_table_percent <- pre_wh_table %>%
  bind_rows(post_wh_table) %>%
  select(-n, -proportion) %>%
  pivot_wider(names_from = work_hard, values_from = percent) %>%
  kbl(caption = "Pre and Post Percent Distribution for work_hard") %>%
  kable_styling()

wh_table_prop <- pre_wh_table %>%
  bind_rows(post_wh_table) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = work_hard, values_from = proportion) %>%
  kbl(caption = "Pre and Post Proportional Distribution for work_hard") %>%
  kable_styling()

# knowledge ----
## Pre ----
pre_know_order <- read_csv(file = "data/wistem_pre_data.csv") %>%
  filter(!is.na(knowledge))
pre_know_order$knowledge <- factor(pre_know_order$knowledge, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

pre_know_plot <- ggplot(data = pre_know_order, mapping = aes(x = knowledge)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Pre Test")

pre_know_table <- pre_know_order %>%
  count(knowledge) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  )

## Post ----
post_know_order <- read_csv(file = "data/wistem_post_data.csv") %>%
  filter(!is.na(knowledge))

post_know_order %>%
  count(knowledge)
post_know_order$knowledge <- factor(post_know_order$knowledge, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

post_know_plot <- ggplot(data = post_know_order, mapping = aes(x = knowledge)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Post Test")

post_know_table <- post_know_order %>%
  count(knowledge) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  )

## Compare ----
### Plot
pre_know_plot + post_know_plot

### Table
know_table_percent <- pre_know_table %>%
  bind_rows(post_know_table) %>%
  select(-n, -proportion) %>%
  pivot_wider(names_from = knowledge, values_from = percent) %>%
  kbl(caption = "Pre and Post Percent Distribution for knowledge") %>%
  kable_styling()

know_table_prop <- pre_know_table %>%
  bind_rows(post_know_table) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = knowledge, values_from = proportion) %>%
  kbl(caption = "Pre and Post Proportional Distribution for knowledge") %>%
  kable_styling()

# courses ----
## Pre ----
pre_course_order <- read_csv(file = "data/wistem_pre_data.csv") %>%
  filter(!is.na(courses))
pre_course_order$courses <- factor(pre_course_order$courses, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

pre_course_plot <- ggplot(data = pre_course_order, mapping = aes(x = courses)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Pre Test")

pre_course_table <- pre_course_order %>%
  count(courses) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  )

## Post ----
post_course_order <- read_csv(file = "data/wistem_post_data.csv") %>%
  filter(!is.na(courses))
post_course_order$courses <- factor(post_course_order$courses, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

post_course_plot <- ggplot(data = post_course_order, mapping = aes(x = courses)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Post Test")

post_course_table <- post_course_order %>%
  count(courses) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  )

## Compare ----
### Plot
pre_course_plot + post_course_plot

### Table
course_table_percent <- pre_course_table %>%
  bind_rows(post_course_table) %>%
  select(-n, -proportion) %>%
  pivot_wider(names_from = courses, values_from = percent) %>%
  kbl(caption = "Pre and Post Percent Distribution for courses") %>%
  kable_styling()

course_table_prop <- pre_course_table %>%
  bind_rows(post_course_table) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = courses, values_from = proportion) %>%
  kbl(caption = "Pre and Post Proportional Distribution for courses") %>%
  kable_styling()

# succesful ----
## Pre ----
pre_succ_order <- read_csv(file = "data/wistem_pre_data.csv") %>%
  filter(!is.na(succesful))
pre_succ_order$succesful <- factor(pre_succ_order$succesful, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

pre_succ_plot <- ggplot(data = pre_succ_order, mapping = aes(x = succesful)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Pre Test")

pre_succ_table <- pre_succ_order %>%
  count(succesful) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  )

## Post ----
post_succ_order <- read_csv(file = "data/wistem_post_data.csv") %>%
  filter(!is.na(succesful))
post_succ_order$succesful <- factor(post_succ_order$succesful, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

post_succ_plot <- ggplot(data = post_succ_order, mapping = aes(x = succesful)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Post Test")

post_succ_table <- post_succ_order %>%
  count(succesful) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  )

## Compare ----
### Plot
pre_succ_plot + post_succ_plot

### Table
succ_table_percent <- pre_succ_table %>%
  bind_rows(post_succ_table) %>%
  select(-n, -proportion) %>%
  pivot_wider(names_from = succesful, values_from = percent) %>%
  kbl(caption = "Pre and Post Percent Distribution for succesful") %>%
  kable_styling()

succ_table_prop <- pre_succ_table %>%
  bind_rows(post_succ_table) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = succesful, values_from = proportion) %>%
  kbl(caption = "Pre and Post Proportional Distribution for succesful") %>%
  kable_styling()

# important ----
## Pre ----
pre_imp_order <- read_csv(file = "data/wistem_pre_data.csv") %>%
  filter(!is.na(important))
pre_imp_order$important <- factor(pre_imp_order$important, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

pre_imp_plot <- ggplot(data = pre_imp_order, mapping = aes(x = important)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Pre Test")

pre_imp_table <- pre_imp_order %>%
  count(important) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  )

## Post ----
post_imp_order <- read_csv(file = "data/wistem_post_data.csv") %>%
  filter(!is.na(important))
post_imp_order$important <- factor(post_imp_order$important, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

post_imp_plot <- ggplot(data = post_imp_order, mapping = aes(x = important)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Post Test")

post_imp_table <- post_imp_order %>%
  count(important) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  )

## Compare ----
### Plot
pre_imp_plot + post_imp_plot

### Table
imp_table_percent <- pre_imp_table %>%
  bind_rows(post_imp_table) %>%
  select(-n, -proportion) %>%
  pivot_wider(names_from = important, values_from = percent) %>%
  kbl(caption = "Pre and Post Percent Distribution for important") %>%
  kable_styling()

imp_table_prop <- pre_imp_table %>%
  bind_rows(post_imp_table) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = important, values_from = proportion) %>%
  kbl(caption = "Pre and Post Proportional Distribution for important") %>%
  kable_styling()

# member ----
## Pre ----
pre_mem_order <- read_csv(file = "data/wistem_pre_data.csv") %>%
  filter(!is.na(member))
pre_mem_order$member <- factor(pre_mem_order$member, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

pre_mem_plot <- ggplot(data = pre_mem_order, mapping = aes(x = member)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Pre Test")

pre_mem_table <- pre_mem_order %>%
  count(member) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  )

## Post ----
post_mem_order <- read_csv(file = "data/wistem_post_data.csv") %>%
  filter(!is.na(member))
post_mem_order$member <- factor(post_mem_order$member, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

post_mem_plot <- ggplot(data = post_mem_order, mapping = aes(x = member)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Post Test")

post_mem_table <- post_mem_order %>%
  count(member) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  )

## Compare ----
### Plot
pre_mem_plot + post_mem_plot

### Table
mem_table_percent <- pre_mem_table %>%
  bind_rows(post_mem_table) %>%
  select(-n, -proportion) %>%
  pivot_wider(names_from = member, values_from = percent) %>%
  kbl(caption = "Pre and Post Percent Distribution for member") %>%
  kable_styling()

mem_table_prop <- pre_mem_table %>%
  bind_rows(post_mem_table) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = member, values_from = proportion) %>%
  kbl(caption = "Pre and Post Proportional Distribution for member") %>%
  kable_styling()

# career ----
## Pre ----
pre_car_order <- read_csv(file = "data/wistem_pre_data.csv") %>%
  filter(!is.na(career))
pre_car_order$career <- factor(pre_car_order$career, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

pre_car_plot <- ggplot(data = pre_car_order, mapping = aes(x = career)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Pre Test")

pre_car_table <- pre_car_order %>%
  count(career) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  )

## Post ----
post_car_order <- read_csv(file = "data/wistem_post_data.csv") %>%
  filter(!is.na(career))
post_car_order$career <- factor(post_car_order$career, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

post_car_plot <- ggplot(data = post_car_order, mapping = aes(x = career)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Post Test")

post_car_table <- post_car_order %>%
  count(career) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  )

## Compare ----
### Plot
pre_car_plot + post_car_plot

### Table
car_table_percent <- pre_car_table %>%
  bind_rows(post_car_table) %>%
  select(-n, -proportion) %>%
  pivot_wider(names_from = career, values_from = percent) %>%
  kbl(caption = "Pre and Post Percent Distribution for career") %>%
  kable_styling()

car_table_prop <- pre_car_table %>%
  bind_rows(post_car_table) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = career, values_from = proportion) %>%
  kbl(caption = "Pre and Post Proportional Distribution for career") %>%
  kable_styling()

# identity ----
## Pre ----
pre_iden_order <- read_csv(file = "data/wistem_pre_data.csv") %>%
  filter(!is.na(identity))
pre_iden_order$identity <- factor(pre_iden_order$identity, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

pre_iden_plot <- ggplot(data = pre_iden_order, mapping = aes(x = identity)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Pre Test")

pre_iden_table <- pre_iden_order %>%
  count(identity) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  )

## Post ----
post_iden_order <- read_csv(file = "data/wistem_post_data.csv") %>%
  filter(!is.na(identity))
post_iden_order$identity <- factor(post_iden_order$identity, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

post_iden_plot <- ggplot(data = post_iden_order, mapping = aes(x = identity)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Post Test")

post_iden_table <- post_iden_order %>%
  count(identity) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  )

## Compare ----
### Plot
pre_iden_plot + post_iden_plot

### Table
iden_table_percent <- pre_iden_table %>%
  bind_rows(post_iden_table) %>%
  select(-n, -proportion) %>%
  pivot_wider(names_from = identity, values_from = percent) %>%
  kbl(caption = "Pre and Post Percent Distribution for identity") %>%
  kable_styling()

iden_table_prop <- pre_iden_table %>%
  bind_rows(post_iden_table) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = identity, values_from = proportion) %>%
  kbl(caption = "Pre and Post Proportional Distribution for identity") %>%
  kable_styling()

# meaningful ----
## Pre ----
pre_mean_order <- read_csv(file = "data/wistem_pre_data.csv") %>%
  filter(!is.na(meaningful))
pre_mean_order$meaningful <- factor(pre_mean_order$meaningful, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

pre_mean_plot <- ggplot(data = pre_mean_order, mapping = aes(x = meaningful)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Pre Test")

pre_mean_table <- pre_mean_order %>%
  count(meaningful) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  )

## Post ----
post_mean_order <- read_csv(file = "data/wistem_post_data.csv") %>%
  filter(!is.na(meaningful))
post_mean_order$meaningful <- factor(post_mean_order$meaningful, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

post_mean_plot <- ggplot(data = post_mean_order, mapping = aes(x = meaningful)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Post Test")

post_mean_table <- post_mean_order %>%
  count(meaningful) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  )

## Compare ----
### Plot
pre_mean_plot + post_mean_plot

### Table
mean_table_percent <- pre_mean_table %>%
  bind_rows(post_mean_table) %>%
  select(-n, -proportion) %>%
  pivot_wider(names_from = meaningful, values_from = percent) %>%
  kbl(caption = "Pre and Post Percent Distribution for meaningful") %>%
  kable_styling()

mean_table_prop <- pre_mean_table %>%
  bind_rows(post_mean_table) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = meaningful, values_from = proportion) %>%
  kbl(caption = "Pre and Post Proportional Distribution for meaningful") %>%
  kable_styling()

# everyday ----
## Pre ----
pre_every_order <- read_csv(file = "data/wistem_pre_data.csv") %>%
  filter(!is.na(everyday))
pre_every_order$everyday <- factor(pre_every_order$everyday, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

pre_every_plot <- ggplot(data = pre_every_order, mapping = aes(x = everyday)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Pre Test")

pre_every_table <- pre_every_order %>%
  count(everyday) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  )

## Post ----
post_every_order <- read_csv(file = "data/wistem_post_data.csv") %>%
  filter(!is.na(everyday))
post_every_order$everyday <- factor(post_every_order$everyday, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

post_every_plot <- ggplot(data = post_every_order, mapping = aes(x = everyday)) + 
  geom_bar(color = "azure4", fill = "gainsboro") + 
  labs(title = "Post Test")

post_every_table <- post_every_order %>%
  count(everyday) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  )

## Compare ----
### Plot
pre_every_plot + post_every_plot

### Table
every_table_percent <- pre_every_table %>%
  bind_rows(post_every_table) %>%
  select(-n, -proportion) %>%
  pivot_wider(names_from = everyday, values_from = percent) %>%
  kbl(caption = "Pre and Post Percent Distribution for everyday") %>%
  kable_styling()

every_table_percent <- pre_every_table %>%
  bind_rows(post_every_table) %>%
  select(-n, -percent) %>%
  pivot_wider(names_from = everyday, values_from = proportion) %>%
  kbl(caption = "Pre and Post Proportional Distribution for everyday") %>%
  kable_styling()
