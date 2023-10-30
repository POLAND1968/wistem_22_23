# Load Packages ----
library(tidyverse)
library(janitor)
library(readr)

set.seed(1968)

# Load Data Sets ----
data <- read_csv(file = "data/processed/wistem_student_forms_clean.csv")
pre_data <- read_csv(file = "data/processed/wistem_pre_data.csv")
post_data <- read_csv(file = "data/processed/wistem_post_data.csv")

# Re-coding Variables ----
data$work_hard <- recode(data$work_hard, "Strongly disagree" = "Disagree") 
data$work_hard <- recode(data$work_hard, "Strongly agree" = "Agree") 

data$knowledge <- recode(data$knowledge, "Strongly disagree" = "Disagree") 
data$knowledge <- recode(data$knowledge, "Strongly agree" = "Agree") 

data$courses <- recode(data$courses, "Strongly disagree" = "Disagree") 
data$courses <- recode(data$courses, "Strongly agree" = "Agree") 

data$succesful <- recode(data$succesful, "Strongly disagree" = "Disagree") 
data$succesful <- recode(data$succesful, "Strongly agree" = "Agree") 

data$important <- recode(data$important, "Strongly disagree" = "Disagree") 
data$important <- recode(data$important, "Strongly agree" = "Agree") 

data$member <- recode(data$member, "Strongly disagree" = "Disagree") 
data$member <- recode(data$member, "Strongly agree" = "Agree") 

data$career <- recode(data$career, "Strongly disagree" = "Disagree") 
data$career <- recode(data$career, "Strongly agree" = "Agree") 

data$identity <- recode(data$identity, "Strongly disagree" = "Disagree") 
data$identity <- recode(data$identity, "Strongly agree" = "Agree") 

data$meaningful <- recode(data$meaningful, "Strongly disagree" = "Disagree") 
data$meaningful <- recode(data$meaningful, "Strongly agree" = "Agree") 

data$everyday <- recode(data$everyday, "Strongly disagree" = "Disagree") 
data$everyday <- recode(data$everyday, "Strongly agree" = "Agree") 

# Arranging Variables ----
data$survey_type <- factor(data$survey_type, levels = c("Pre", "Post"))

data$work_hard <- factor(data$work_hard, levels = c("Disagree", "Neutral", "Agree"))
data$knowledge <- factor(data$knowledge, levels = c("Disagree", "Neutral", "Agree"))
data$courses <- factor(data$courses, levels = c("Disagree", "Neutral", "Agree"))
data$succesful <- factor(data$succesful, levels = c("Disagree", "Neutral", "Agree"))
data$important <- factor(data$important, levels = c("Disagree", "Neutral", "Agree"))
data$member <- factor(data$member, levels = c("Disagree", "Neutral", "Agree"))
data$career <- factor(data$career, levels = c("Disagree", "Neutral", "Agree"))
data$identity <- factor(data$identity, levels = c("Disagree", "Neutral", "Agree"))
data$meaningful <- factor(data$meaningful, levels = c("Disagree", "Neutral", "Agree"))
data$everyday <- factor(data$everyday, levels = c("Disagree", "Neutral", "Agree"))

# Graphs ----
colors <- c("#B477FF", "#7000B0")

## Work Hard ----
wh_data <- data %>%
  filter(!is.na(work_hard)) %>%
  group_by(survey_type, work_hard) %>%
  summarise(count = n()) %>%
  group_by(survey_type) %>%
  mutate(percentage = count / sum(count) * 100)

wh_plot <- ggplot(data = wh_data, mapping = aes(x = work_hard, y = percentage, fill = survey_type)) + 
  geom_col(position = 'dodge') + 
  geom_text(
    mapping = aes(x = work_hard, y = percentage, label = sprintf("%.2f%%", percentage)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    family = "Georgia", 
    fontface = "bold"
    ) + 
  scale_fill_manual(
    name = "",
    labels = c("Pre Test", "Post Test"), 
    values = colors
    ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    x = "Student Responses", 
    y = "Percentage", 
    title = "I'll Find the Answer If I Work Hard", 
    subtitle = "WiSTEM 2022 - 2023"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, family = "Georgia", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Georgia"),
    legend.position ="top", 
    legend.text = element_text(family = "Georgia", size = 12), 
    axis.title = element_text(family = "Georgia", size = 12), 
    axis.text = element_text(family = "Georgia", size = 10), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    )

## Knowledge ----
know_data <- data %>%
  filter(!is.na(knowledge)) %>%
  group_by(survey_type, knowledge) %>%
  summarise(count = n()) %>%
  group_by(survey_type) %>%
  mutate(percentage = count / sum(count) * 100)

know_plot <- ggplot(data = know_data, mapping = aes(x = knowledge, y = percentage, fill = survey_type)) + 
  geom_col(position = 'dodge') + 
  geom_text(
    mapping = aes(x = knowledge, y = percentage, label = sprintf("%.2f%%", percentage)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    family = "Georgia", 
    fontface = "bold"
  ) + 
  scale_fill_manual(
    name = "",
    labels = c("Pre Test", "Post Test"), 
    values = colors
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    x = "Student Responses", 
    y = "Percentage", 
    title = "I'm Confident In Applying Prior Knowledge To New Projects", 
    subtitle = "WiSTEM 2022 - 2023"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, family = "Georgia", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Georgia"),
    legend.position ="top", 
    legend.text = element_text(family = "Georgia", size = 12), 
    axis.title = element_text(family = "Georgia", size = 12), 
    axis.text = element_text(family = "Georgia", size = 10), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

## Courses ----
course_data <- data %>%
  filter(!is.na(courses)) %>%
  group_by(survey_type, courses) %>%
  summarise(count = n()) %>%
  group_by(survey_type) %>%
  mutate(percentage = count / sum(count) * 100)

course_plot <- ggplot(data = course_data, mapping = aes(x = courses, y = percentage, fill = survey_type)) + 
  geom_col(position = 'dodge') + 
  geom_text(
    mapping = aes(x = courses, y = percentage, label = sprintf("%.2f%%", percentage)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    family = "Georgia", 
    fontface = "bold"
  ) + 
  scale_fill_manual(
    name = "",
    labels = c("Pre Test", "Post Test"), 
    values = colors
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    x = "Student Responses", 
    y = "Percentage", 
    title = "I'm Confident In My Ability To Do Well In STE(A)M Courses", 
    subtitle = "WiSTEM 2022 - 2023"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, family = "Georgia", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Georgia"),
    legend.position ="top", 
    legend.text = element_text(family = "Georgia", size = 12), 
    axis.title = element_text(family = "Georgia", size = 12), 
    axis.text = element_text(family = "Georgia", size = 10), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

## Successful ----
succ_data <- data %>%
  filter(!is.na(succesful)) %>%
  group_by(survey_type, succesful) %>%
  summarise(count = n()) %>%
  group_by(survey_type) %>%
  mutate(percentage = count / sum(count) * 100)

succ_plot <- ggplot(data = succ_data, mapping = aes(x = succesful, y = percentage, fill = survey_type)) + 
  geom_col(position = 'dodge') + 
  geom_text(
    mapping = aes(x = succesful, y = percentage, label = sprintf("%.2f%%", percentage)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    family = "Georgia", 
    fontface = "bold"
  ) + 
  scale_fill_manual(
    name = "",
    labels = c("Pre Test", "Post Test"), 
    values = colors
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    x = "Student Responses", 
    y = "Percentage", 
    title = "I Think I Could Be Successful In A STE(A)M Career", 
    subtitle = "WiSTEM 2022 - 2023"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, family = "Georgia", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Georgia"),
    legend.position ="top", 
    legend.text = element_text(family = "Georgia", size = 12), 
    axis.title = element_text(family = "Georgia", size = 12), 
    axis.text = element_text(family = "Georgia", size = 10), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

## Important ----
imp_data <- data %>%
  filter(!is.na(important)) %>%
  group_by(survey_type, important) %>%
  summarise(count = n()) %>%
  group_by(survey_type) %>%
  mutate(percentage = count / sum(count) * 100)

imp_plot <- ggplot(data = imp_data, mapping = aes(x = important, y = percentage, fill = survey_type)) + 
  geom_col(position = 'dodge') + 
  geom_text(
    mapping = aes(x = important, y = percentage, label = sprintf("%.2f%%", percentage)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    family = "Georgia", 
    fontface = "bold"
  ) + 
  scale_fill_manual(
    name = "",
    labels = c("Pre Test", "Post Test"), 
    values = colors
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    x = "Student Responses", 
    y = "Percentage", 
    title = "People In The STE(A)M Field Can Solve Important Problems", 
    subtitle = "WiSTEM 2022 - 2023"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, family = "Georgia", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Georgia"),
    legend.position ="top", 
    legend.text = element_text(family = "Georgia", size = 12), 
    axis.title = element_text(family = "Georgia", size = 12), 
    axis.text = element_text(family = "Georgia", size = 10), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

## Member ----
mem_data <- data %>%
  filter(!is.na(member)) %>%
  group_by(survey_type, member) %>%
  summarise(count = n()) %>%
  group_by(survey_type) %>%
  mutate(percentage = count / sum(count) * 100)

mem_plot <- ggplot(data = mem_data, mapping = aes(x = member, y = percentage, fill = survey_type)) + 
  geom_col(position = 'dodge') + 
  geom_text(
    mapping = aes(x = member, y = percentage, label = sprintf("%.2f%%", percentage)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    family = "Georgia", 
    fontface = "bold"
  ) + 
  scale_fill_manual(
    name = "",
    labels = c("Pre Test", "Post Test"), 
    values = colors
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    x = "Student Responses", 
    y = "Percentage", 
    title = "I Am A Member Of A STE(A)M Community", 
    subtitle = "WiSTEM 2022 - 2023"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, family = "Georgia", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Georgia"),
    legend.position ="top", 
    legend.text = element_text(family = "Georgia", size = 12), 
    axis.title = element_text(family = "Georgia", size = 12), 
    axis.text = element_text(family = "Georgia", size = 10), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

## Career ----
career_data <- data %>%
  filter(!is.na(career)) %>%
  group_by(survey_type, career) %>%
  summarise(count = n()) %>%
  group_by(survey_type) %>%
  mutate(percentage = count / sum(count) * 100)

career_plot <- ggplot(data = career_data, mapping = aes(x = career, y = percentage, fill = survey_type)) + 
  geom_col(position = 'dodge') + 
  geom_text(
    mapping = aes(x = career, y = percentage, label = sprintf("%.2f%%", percentage)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    family = "Georgia", 
    fontface = "bold"
  ) + 
  scale_fill_manual(
    name = "",
    labels = c("Pre Test", "Post Test"), 
    values = colors
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    x = "Student Responses", 
    y = "Percentage", 
    title = "I Think A Career In STE(A)M May Be A Good Fit For Me", 
    subtitle = "WiSTEM 2022 - 2023"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, family = "Georgia", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Georgia"),
    legend.position ="top", 
    legend.text = element_text(family = "Georgia", size = 12), 
    axis.title = element_text(family = "Georgia", size = 12), 
    axis.text = element_text(family = "Georgia", size = 10), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

## Identity ----
iden_data <- data %>%
  filter(!is.na(identity)) %>%
  group_by(survey_type, identity) %>%
  summarise(count = n()) %>%
  group_by(survey_type) %>%
  mutate(percentage = count / sum(count) * 100)

iden_plot <- ggplot(data = iden_data, mapping = aes(x = identity, y = percentage, fill = survey_type)) + 
  geom_col(position = 'dodge') + 
  geom_text(
    mapping = aes(x = identity, y = percentage, label = sprintf("%.2f%%", percentage)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    family = "Georgia", 
    fontface = "bold"
  ) + 
  scale_fill_manual(
    name = "",
    labels = c("Pre Test", "Post Test"), 
    values = colors
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    x = "Student Responses", 
    y = "Percentage", 
    title = "'Future STE(A)M Professional' Is An Important Identity To Me", 
    subtitle = "WiSTEM 2022 - 2023"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, family = "Georgia", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Georgia"),
    legend.position ="top", 
    legend.text = element_text(family = "Georgia", size = 12), 
    axis.title = element_text(family = "Georgia", size = 12), 
    axis.text = element_text(family = "Georgia", size = 10), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

## Meaningful ----
mean_data <- data %>%
  filter(!is.na(meaningful)) %>%
  group_by(survey_type, meaningful) %>%
  summarise(count = n()) %>%
  group_by(survey_type) %>%
  mutate(percentage = count / sum(count) * 100)

mean_plot <- ggplot(data = mean_data, mapping = aes(x = meaningful, y = percentage, fill = survey_type)) + 
  geom_col(position = 'dodge') + 
  geom_text(
    mapping = aes(x = meaningful, y = percentage, label = sprintf("%.2f%%", percentage)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    family = "Georgia", 
    fontface = "bold"
  ) + 
  scale_fill_manual(
    name = "",
    labels = c("Pre Test", "Post Test"), 
    values = colors
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    x = "Student Responses", 
    y = "Percentage", 
    title = "I Could Make a Meaningful Contribution In A STE(A)M Career", 
    subtitle = "WiSTEM 2022 - 2023"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, family = "Georgia", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Georgia"),
    legend.position ="top", 
    legend.text = element_text(family = "Georgia", size = 12), 
    axis.title = element_text(family = "Georgia", size = 12), 
    axis.text = element_text(family = "Georgia", size = 10), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

## Everyday ----
every_data <- data %>%
  filter(!is.na(everyday)) %>%
  group_by(survey_type, everyday) %>%
  summarise(count = n()) %>%
  group_by(survey_type) %>%
  mutate(percentage = count / sum(count) * 100)

every_plot <- ggplot(data = every_data, mapping = aes(x = everyday, y = percentage, fill = survey_type)) + 
  geom_col(position = 'dodge') + 
  geom_text(
    mapping = aes(x = everyday, y = percentage, label = sprintf("%.2f%%", percentage)), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    family = "Georgia", 
    fontface = "bold"
  ) + 
  scale_fill_manual(
    name = "",
    labels = c("Pre Test", "Post Test"), 
    values = colors
  ) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    x = "Student Responses", 
    y = "Percentage", 
    title = "I Understand What Everyday STE(A)M Work Is Like", 
    subtitle = "WiSTEM 2022 - 2023"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, family = "Georgia", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "Georgia"),
    legend.position ="top", 
    legend.text = element_text(family = "Georgia", size = 12), 
    axis.title = element_text(family = "Georgia", size = 12), 
    axis.text = element_text(family = "Georgia", size = 10), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Saving
save(colors, wh_plot, know_plot, course_plot, succ_plot, imp_plot, mem_plot, career_plot, iden_plot, mean_plot, every_plot, file = "data/processed/pre_post_report_graphs.rda")
