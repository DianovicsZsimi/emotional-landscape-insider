"Emotional landscape project"
install.packages("ggthemes")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rstatix)
library(ggthemes)
library(tidyr)

my_data <- read.csv("D:/metalabor/emotional landscape/raw_data.csv") 
View(my_data)


#completion distribution
my_data_clean <- my_data[-c(1, 2), ]

my_data_clean %>%
  mutate(Status = ifelse(Finished,
                         "Completed",
                         "Not Completed")) %>%
  count(Status) %>%
  ggplot(aes(x = Status, y = n)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n),
            vjust = -0.4,
            size = 5) +
  labs(
    title = "Questionnaire Completion",
    x = "",
    y = "Number of Participants"
  ) +
  theme_minimal()


#gender distribution
my_data_clean <- my_data[-c(1, 2), ]

df_Gender <- my_data_clean %>%
  mutate(
    gender = str_trim(gender),
    gender = ifelse(gender == "" | is.na(gender),
                    "Missing",
                    gender),
    gender = factor(
      gender,
      levels = c(
        "Man",
        "Woman",
        "Non-binary / third gender / gender non-conforming",
        "Prefer not to say",
        "Missing"
      )
    )
  ) %>%
  count(gender)

df_Gender

ggplot(df_Gender, aes(x = gender, y = n)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n),
            vjust = -0.4,
            size = 4) +
  labs(
    title = "Participant Gender Distribution",
    x = "",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#research area distribution
my_data_clean <- my_data[-c(1, 2), ]

unique(my_data_clean$area)

df_area <- my_data_clean %>%
  mutate(
    area = str_trim(area),
    area = ifelse(area == "" | is.na(area),
                  "Missing",
                  area)
  ) %>%
  count(area)

df_area

df_area <- df_area %>%
  arrange(desc(n)) %>%
  mutate(
    area = factor(
      area,
      levels = c(
        area[area != "Missing"],
        "Missing"
      )
    )
  )

ggplot(df_area, aes(x = area, y = n)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n),
            vjust = -0.4,
            size = 4) +
  labs(
    title = "Participants by Research Area",
    x = "",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#questionnaire dropout spikes
my_data_clean <- my_data[-c(1, 2), ]

df_dropout_exact <- my_data_clean %>%
  mutate(Progress = as.numeric(Progress)) %>%
  filter(!is.na(Progress), Progress < 100) %>%
  count(Progress) %>%
  arrange(Progress)

threshold <- 5

df_spikes <- df_dropout_exact %>%
  filter(n > threshold) %>%  
  mutate(is_max = n == max(n))

ymax <- max(df_spikes$n)


ggplot(df_spikes, aes(x = factor(Progress), y = n, fill = is_max)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = n), vjust = -0.35, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("FALSE" = "grey", "TRUE" = "black"), guide = "none") +
  scale_y_continuous(
    breaks = seq(0, ceiling(ymax/10)*10, by = 10),
    limits = c(0, ymax * 1.15),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    
    x = "Progress (%) at dropout",
    y = "Number of participants"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

mean_progress <- mean(df_dropout_exact$Progress, na.rm = TRUE)
sd_progress <- sd(df_dropout_exact$Progress, na.rm = TRUE)

mean_progress
sd_progress

#research stage distribution
my_data_clean <- my_data[-c(1, 2), ]

df_stage <- my_data_clean %>%
  mutate(
    research_stage = str_trim(research_stage),
    research_stage = ifelse(is.na(research_stage) | research_stage == "",
                            "Missing",
                           research_stage)
  ) %>%
  count(research_stage) %>%
  arrange(desc(n))

real_stages <- df_stage %>%
  filter(research_stage != "Missing")

missing_stage <- df_stage %>%
  filter(research_stage == "Missing")

letters_used <- LETTERS[1:nrow(real_stages)]

real_stages <- real_stages %>%
  mutate(stage_code = letters_used)

df_stage_final <- bind_rows(real_stages, missing_stage) %>%
  mutate(
    stage_code = ifelse(is.na(stage_code), "Missing", stage_code),
    stage_code = factor(stage_code,
                        levels = c(letters_used, "Missing"))
  )

stage_legend <- df_stage_final %>%
  select(stage_code, research_stage)

print(stage_legend)

library(ggplot2)

ggplot(df_stage_final, aes(x = stage_code, y = n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n),
            vjust = -0.4,
            size = 5,
            fontface = "bold") +
  labs(
    title = "Distribution of Participants by Research Stage",
    x = "Research Stage (see legend in console)",
    y = "Number of Participants"
  ) +
  theme_minimal(base_size = 15)



#position distribution
my_data_clean <- my_data[-c(1, 2), ]

unique(my_data_clean$postion)

df_position <- my_data_clean %>%
  mutate(
    position = str_trim(position),
    position = ifelse(position == "" | is.na(position),
                  "Missing",
                  position)
  ) %>%
  count(position)

df_position

df_position <- df_position %>%
  arrange(desc(n)) %>%
  mutate(
    position = factor(
      position,
      levels = c(
        position[position != "Missing"],
        "Missing"
      )
    )
  )

ggplot(df_position, aes(x = position, y = n)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n),
            vjust = -0.4,
            size = 4) +
  labs(
    title = "Participants by Position",
    x = "",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#work distribution
my_data_clean <- my_data[-c(1, 2), ]

unique(my_data_clean$category_of_work)

df_category_of_work <- my_data_clean %>%
  mutate(
    category_of_work = str_trim(category_of_work),
    category_of_work = ifelse(category_of_work == "" | is.na(category_of_work),
                      "Missing",
                      category_of_work)
  ) %>%
  count(category_of_work)

df_category_of_work

df_category_of_work <- df_category_of_work %>%
  arrange(desc(n)) %>%
  mutate(
    category_of_work = factor(
      category_of_work,
      levels = c(
        category_of_work[category_of_work != "Missing"],
        "Missing"
      )
    )
  )

ggplot(df_category_of_work, aes(x = category_of_work, y = n)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n),
            vjust = -0.4,
            size = 4) +
  labs(
    title = "Participants by category of work",
    x = "",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#age distribution
library(dplyr)
library(ggplot2)

my_data_clean <- my_data[-c(1, 2), ]

df_age <- my_data_clean %>%
  mutate(
    age = as.numeric(age)
  ) %>%
  filter(!is.na(age))

unique(my_data_clean$age)

ggplot(df_age, aes(x = age)) +
  geom_histogram(binwidth = 1, color = "white", fill = "grey") +
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  labs(
    title = "Age Distribution of Participants",
    x = "Age",
    y = "Number of Participants"
  ) +
  theme_minimal(base_size = 15)

mean_age <- mean(df_age$age)
sd_age <- sd(df_age$age)
median_age <- median(df_age$age, na.rm = TRUE)
min_age <- min(df_age$age, na.rm = TRUE)
max_age <- max(df_age$age, na.rm = TRUE)

mean_age
sd_age
median_age
min_age
max_age
