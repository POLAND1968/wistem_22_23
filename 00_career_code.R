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

# Number of Careers ----
## Pre ----
pre_career_amount_plot <- ggplot(data = pre_data, mapping = aes(x = career_amount)) + 
  geom_density() + 
  geom_rug() + 
  theme_minimal()

pre_career_amount_table <- pre_data %>%
  count(career_amount) %>%
  mutate(
    mean = mean(career_amount), 
    max = max(career_amount), 
    min = min(career_amount), 
    std_dev = sd(career_amount),
    median = median(career_amount),
    test = "pre"
  ) %>%
  select(test, min, median, mean, std_dev, max) %>%
  head(1)

## Post ----
post_career_amount_plot <- ggplot(data = post_data, mapping = aes(x = career_amount)) + 
  geom_density() + 
  geom_rug() + 
  theme_minimal()

post_career_amount_table <- post_data %>%
  count(career_amount) %>%
  mutate(
    mean = mean(career_amount), 
    max = max(career_amount), 
    min = min(career_amount), 
    std_dev = sd(career_amount),
    median = median(career_amount),
    test = "post"
  ) %>%
  select(test, min, median, mean, std_dev, max) %>%
  head(1)

## Compare ----
### Plot
pre_career_amount_plot + post_career_amount_plot

### Table
career_amount_table <- pre_career_amount_table %>%
  bind_rows(post_career_amount_table)

# Career Type ----
## Pre ----
pre_data_longer <- pre_data %>%
  select(survey_type, career_one, career_two, career_three, career_four, career_five, career_six, career_seven, career_eight, career_nine, career_ten) %>% 
  pivot_longer(!survey_type, names_to = "career_num", values_to = "career") %>%
  select(-career_num) %>%
  filter(!is.na(career))

pre_career <- pre_data_longer %>%
  count(career) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "pre"
  ) %>%
  select(test, career, n, proportion, percent) %>%
  rename(count = n)

### Most ----
pre_most_career <- pre_career %>%
  arrange(desc(count)) %>%
  head(10) %>%
  kbl(caption = "Most Common Career Chosen in Pre Test") %>%
  kable_styling()

### Least ----
pre_only_one <- pre_career %>%
  filter(count == "1") %>%
  pull(career)

pre_two_and_three <- pre_career %>%
  filter(count == 2 | count == 3) %>%
  arrange(count) %>%
  pull(career)

pre_four_and_five <- pre_career %>%
  filter(count == 4 | count == 5) %>%
  arrange(count) %>%
  pull(career)

### Categories ----
pre_major_career <- data.frame (
  category  = c("Business", "Engineer", "Medical", "History", "Math", "Arts", "Physics", "Computer Science", "Legal Studies", "Earth Studies", "Design Studies", "Education", "Psych/Neuro", "Miscellaneous"),
  pre_unique_count = c(4, 13, 26, 2, 2, 1, 6, 6, 1, 2, 2, 2, 5, 5), 
  pre_total_count = c(8, 81, 143, 4, 28, 3, 31, 28, 1, 2, 2, 9, 18, 16)
) %>%
  arrange(desc(pre_unique_count))

pre_stem_cat_career <- data.frame (
  category  = c("Science", "Technology", "Engineering", "Arts", "Mathematics"),
  pre_unique_count = c(47, 6, 13, 3, 8), 
  pre_total_count = c(201, 28, 81, 5, 59)
) %>%
  arrange(desc(pre_unique_count))

# Business (4, 8): Accountant (5), Economist (1), Entrepreneur (1), Financial Risk Analyst  (1)
# Engineer (13, 81): Aerospace Engineer (6), Bioengineer (11), Biomedical Engineer (1), Chemical Engineer (5), Civil Engineer (5), Computer Engineer (1), Engineer (30), Environmental Engineer (5), Industrial Engineer (2), Materials Engineer (1), Mechanical Engineer (7), Software Engineer (6), Structural Engineer (1)
# Medical (26, 143): Anesthesiologist (1), Astrobiologist (1), Biochemist (5), Biologist (20), Biotechnician (1), Cardiologist (2), Cardiothoracic Surgeon (2), Cardiovascular Surgeon (1), Chemist (26), Dentist (3), Doctor (26), Gynecologist (1), Marine Biologist (5), Microbiologist (2), Nurse (8), OB-GYN (3), Orthopedic Surgeon (1), Pediatric Surgeon (1), Pediatrician (1), Pharmacist (1), Physical Therapist (4), Physician (1), Pulmonologist (1), Scientist (16), Surgeon (7), Vet (3)  
# History (2, 4): Anthropologist (1), Archeologist (3)
# Math (2, 28): Architect (13), Mathematician (15)
# Arts (1, 3): Artist (3)
# Physics (6, 31): Astronaut (2), Astronomer (5), Astrophysicist (5), Meteorologist (1), Physicist (17), Rocket Scientist (1)
# Computer Science (6, 28): Computer Technician (3), Cybersecurity (1), Data Analyst (1), Data Scientist (1), Programmer (14), Software Developer (8)
# Legal Studies (1, 1): Environmentalist (1)
# Earth Studies (2, 2): Geologist (1), Ornithologist (1)
# Design Studies (2, 2): Manufacturer (1), Product Designer (1)
# Education (2, 9): Math Teacher (8), Teacher (1)
# Psych/Neuro (5, 18): Neuroscientist (2), Nueroscientist (6), Nuerosurgeon (1), Psychiatrist (2), Psychologist (7)
# Miscellaneous (5, 16): Construction Worker (2), Mechanic (5), Metrologist (1), Researcher (7), Technician (1)

# Science: Business, Medical, History, Earth Studies, Legal Studies, Psych/Neuro, Education, Miscellaneous
# Technology: Computer Science
# Engineering: Engineer
# Arts: Arts, Design Studies
# Mathematics: Math, Physics

## Post ----
post_data_longer <- post_data %>%
  select(survey_type, career_one, career_two, career_three, career_four, career_five, career_six, career_seven, career_eight, career_nine, career_ten) %>% 
  pivot_longer(!survey_type, names_to = "career_num", values_to = "career") %>%
  select(-career_num) %>%
  filter(!is.na(career))

post_career <- post_data_longer %>%
  count(career) %>%
  mutate(
    proportion = n / sum(n), 
    percent = proportion * 100, 
    test = "post"
  ) %>%
  select(test, career, n, proportion, percent) %>%
  rename(count = n)

### Most ----
post_most_career <- post_career %>%
  arrange(desc(count)) %>%
  head(10) %>%
  kbl(caption = "Most Common Career Chosen in Post Test") %>%
  kable_styling()

### Least ----
post_only_one <- post_career %>%
  filter(count == "1") %>%
  pull(career)

post_two_and_three <- post_career %>%
  filter(count == 2 | count == 3) %>%
  arrange(count) %>%
  pull(career)

post_four_and_five <- post_career %>%
  filter(count == 4 | count == 5) %>%
  arrange(count) %>%
  pull(career)

### Categories ----
post_major_career <- data.frame (
  category  = c("Business", "Engineer", "Medical", "History", "Math", "Arts", "Physics", "Computer Science", "Legal Studies", "Earth Studies", "Design Studies", "Education", "Psych/Neuro", "Miscellaneous"),
  post_unique_count = c(1, 15, 20, 0, 3, 0, 3, 5, 0, 0, 0, 2, 3, 4), 
  post_total_count = c(1, 67, 68, 0, 12, 0, 13, 17, 0, 0, 0, 6, 7, 9)
) %>%
  arrange(desc(post_unique_count))

post_stem_cat_career <- data.frame (
  category  = c("Science", "Technology", "Engineering", "Arts", "Mathematics"),
  post_unique_count = c(30, 5, 15, 0, 6), 
  post_total_count = c(91, 17, 67, 0, 25)
) %>%
  arrange(desc(post_unique_count))

# Business (1, 1): Accountant (1)
# Engineer (15, 67): Aerospace Engineer (7), Biochemical Engineer (3), Bioengineer (2), Biomedical Engineer (7), Chemical Engineer (5), Civil Engineer (6), Computer Engineer (2), Electrical Engineer (2), Engineer (16), Environmental Engineer (1), Industrial Engineer (1), Mechanical Engineer (9), Nuclear Engineer (1), 	Software Engineer (1), Surgeon (4)
# Medical (20, 68): Anesthesiologist (1), Biochemist (1), Biologist (12), Chemist (8), Dentist (1), Dermatologist (1), Doctor (16), Epidemiologist (2), Geneticist (1), Marine Biologist (2), Microbiologist (1), Nurse (7), Nutritionist (1), Orthopedic Surgeon (2), Paramedic (1), Pediatrician (2), Pharmacist (2), Radiologist (1), Scientist (5), Vet (1)
# Math (3, 12): Architect (3), Mathematician (7), Statistician (2)
# Physics (3, 13): Astronomer (3), Astrophysicist (2), Physicist (8)
# Computer Science (5, 17): AI Developer (1), Computer Scientist (8), Data Analyst (1), Programmer (3), Software Developer (4)
# Education (2, 6): Math Teacher (2), Teacher (4)
# Psych/Neuro (3, 7): Neuroscientist (4), Psychiatrist (1), Psychologist (2)
# Miscellaneous (4, 9): Mechanic (1), Metrologist (1), Researcher (2), Technician (5)

# Science: Business, Medical, Psych/Neuro, Education, Miscellaneous
# Technology: Computer Science
# Engineering: Engineer
# Mathematics: Math, Physics

## Compare ----
### Table ----
major_career <- full_join(pre_major_career, post_major_career) %>%
  select(category, pre_unique_count, post_unique_count, pre_total_count, post_total_count) %>%
  kbl(caption = "Careers Grouped by Major Type") %>%
  kable_styling()

stem_cat_career <- full_join(pre_stem_cat_career, post_stem_cat_career) %>%
  select(category, pre_unique_count, post_unique_count, pre_total_count, post_total_count) %>%
  kbl(caption = "Careers Grouped by STEM Type") %>%
  kable_styling()

### Graph ----
pre_stem_cat_career$category <- factor(pre_stem_cat_career$category, levels = c("Mathematics", "Arts", "Engineering", "Technology", "Science"))
pre_stem_graph <- ggplot(data = pre_stem_cat_career, mapping = aes(y = category, x = pre_unique_count, fill = category)) + 
  geom_col() + 
  labs(title = "Pre Test Careers Grouped by STEM Type")

post_stem_cat_career$category <- factor(post_stem_cat_career$category, levels = c("Mathematics", "Arts", "Engineering", "Technology", "Science"))
post_stem_graph <- ggplot(data = post_stem_cat_career, mapping = aes(y = category, x = post_unique_count, fill = category)) + 
  geom_col() + 
  labs(title = "Post Test Careers Grouped by STEM Type")

pre_stem_graph + post_stem_graph

### Ind. Career Differences ----
post_new_careers <- post_career[!post_career$career %in% pre_career$career,] %>%
  pull(career)

pre_diff_careers <- pre_career[!pre_career$career %in% post_career$career,] %>%
  pull(career)

same_careers <- post_career[post_career$career %in% pre_career$career,] %>%
  pull(career)
