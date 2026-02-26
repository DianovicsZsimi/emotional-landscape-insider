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

my_data$Status <- ifelse(my_data$Finished,
                         "Completed",
                         "Not Completed")


#Categorical Distribution Plot Function
plot_distribution_categorical <- function(data,
                                          variable_name,
                                          title,
                                          custom_levels = NULL,
                                          remove_parentheses = FALSE,
                                          angle = 30) {
  
  library(dplyr)
  library(ggplot2)
  
  data_clean <- data[-c(1, 2), ]
  
  df_counts <- data_clean %>%
    mutate(
      var = as.character(.data[[variable_name]]),
      var = ifelse(var == "" | is.na(var), NA, var)
    ) %>%
    filter(!is.na(var)) %>%
    count(var)
  
  if (!is.null(custom_levels)) {
    df_counts <- df_counts %>%
      mutate(var = factor(var, levels = custom_levels)) %>%
      filter(!is.na(var))
  } else {
    df_counts <- df_counts %>%
      arrange(desc(n)) %>%
      mutate(var = factor(var, levels = var))
  }
  
  p <- ggplot(df_counts, aes(x = var, y = n)) +
    geom_col(width = 0.4) +
    geom_text(aes(label = n),
              vjust = -0.4,
              size = 4) +
    labs(
      title = title,
      x = "",
      y = "Number of Participants"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(),
      axis.text.x = element_text(size = 12, angle = angle, hjust = 1)
    )
  
  if (remove_parentheses) {
    p <- p + scale_x_discrete(
      labels = function(x) sub("\\s*\\(.*\\)", "", x)
    )
  }
  
  return(p)
}

#completion distribution
plot_distribution_categorical(my_data, "Status", "Questionnaire Completion")

#gender distribution
plot_distribution_categorical(
  my_data,
  "gender",
  "Participant Gender Distribution",
  custom_levels = c(
    "Man",
    "Woman",
    "Non-binary / third gender / gender non-conforming",
    "Prefer not to say"
  )
)

#research area distribution
plot_distribution_categorical(my_data, "area", "Participants by Research Area")

#research stage distribution
plot_distribution_categorical(
  my_data,
  "research_stage",
  "Participants by Research Stage",
  remove_parentheses = TRUE
)

#position distribution
plot_distribution_categorical(my_data, "position", "Participants by Position")

#work distribution
plot_distribution_categorical(my_data, "category_of_work", "Participants by Category of Work")


#Other distributions
#age distribution
my_data_clean <- my_data[-c(1, 2), ]

df_age <- my_data_clean %>%
  mutate(
    age = as.numeric(age)
  ) %>%
  filter(!is.na(age))

ggplot(df_age, aes(x = age)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  labs(
    title = "Age Distribution of Participants",
    x = "Age",
    y = "Number of Participants"
  ) +
  theme_minimal (base_size = 14)

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


#questionnaire dropout spikes
my_data_clean <- my_data[-c(1, 2), ]

df_dropout <- my_data_clean %>%
  mutate(Progress = as.numeric(Progress)) %>%
  filter(!is.na(Progress), Progress < 100)

df_dropout_counts <- df_dropout %>%
  count(Progress) %>%
  arrange(Progress) %>%
  mutate(
    Progress = factor(Progress, levels = Progress)
  )

mean_progress <- mean(df_dropout$Progress, na.rm = TRUE)
sd_progress   <- sd(df_dropout$Progress, na.rm = TRUE)

mean_progress
sd_progress

ggplot(df_dropout_counts, aes(x = Progress, y = n)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n),
            vjust = -0.4,
            size = 4) +
  labs(
    title = "Dropout Points Across Questionnaire",
    x = "Progress (%) at Dropout",
    y = "Number of Participants"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(),
    axis.title = element_text(),
    axis.text.x = element_text()
  )


#country distribution
install.packages("maps")
library(dplyr)
library(ggplot2)
library(maps)

my_data_clean <- my_data[-c(1, 2), ]

df_country <- my_data_clean %>%
  mutate(country = ifelse(country == "" | is.na(country), NA, country)) %>%
  filter(!is.na(country)) %>%
  count(country) %>%
  mutate(country = tolower(country))

world_map <- map_data("world") %>%
  mutate(region = tolower(region))

world_data <- left_join(world_map, df_country, by = c("region" = "country"))

ggplot(world_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = n), color = "white", linewidth = 0.1) +
  scale_fill_gradient(
    low = "#deebf7",
    high = "#08519c",
    na.value = "grey95"
  ) +
  labs(
    title = "Participants by Country",
    fill = "Participants"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

df_country_ranked <- df_country %>%
  arrange(desc(n))

df_country_ranked