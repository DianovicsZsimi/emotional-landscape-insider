"Emotional landscape project"
install.packages("ggthemes")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rstatix)
library(ggthemes)
library(tidyr)

raw_data = read.csv("C:/Emotional landscape/raw_data.csv")


data_recoded = raw_data[-1:-2, ]

data_recoded = data_recoded %>% 
  rename(feeling_interest = feeling_stage_1,
         feeling_amusement = feeling_stage_2,
         feeling_pride = feeling_stage_3,
         feeling_joy = feeling_stage_4,
         feeling_pleasure = feeling_stage_5,
         feeling_contentment = feeling_stage_6,
         feeling_love = feeling_stage_7, 
         feeling_admiration = feeling_stage_8, 
         feeling_relief = feeling_stage_9, 
         feeling_compassion = feeling_stage_10,
         feeling_sadness = feeling_stage_11, 
         feeling_guilt = feeling_stage_12,
         feeling_regret = feeling_stage_13, 
         feeling_shame = feeling_stage_14, 
         feeling_disappointment = feeling_stage_15, 
         feeling_fear = feeling_stage_16, 
         feeling_disgust = feeling_stage_17, 
         feeling_contempt = feeling_stage_18,
         feeling_hate = feeling_stage_19, 
         feeling_anger = feeling_stage_20)

data_recoded = data_recoded %>% 
  mutate(research_stage = factor(research_stage)) %>% 
  mutate(area = factor(area)) %>% 
  mutate(gender = factor(gender)) %>% 
  mutate(position = factor(position)) %>% 
  mutate(category_of_work = factor(category_of_work)) %>% 
  mutate(research_stage = recode(research_stage,	
                                 "Post-project stage (the project has received a decision (accepted or rejected). At this point, it is either concluded or being revised and prepared for submission to another venue)"
                                 = "Post-project stage", 
                                 "Preparations (IRB permit, preregistration, experiment preparations)" = 
                                   "Preparations",
                                 "Reality check (e.g., literature search, consultation with colleagues)" =
                                   "Reality check"
                                 
  )) %>% 
  mutate(across(starts_with("feeling_"),
                ~ as.numeric(as.character(.x))))
  
levels(data_recoded$research_stage)

research_stage_count = data_recoded %>% 
  count(research_stage) %>% 
  slice(-1) 
 

area_count = data_recoded %>% 
  count(area) %>% 
  slice(-1)

gender_count = data_recoded %>% 
  count(gender) %>% 
  slice(-1)

position_count = data_recoded %>% 
  count(position) %>% 
  slice(-1)

category_of_work_count = data_recoded %>% 
  count(category_of_work) %>% 
  slice(-1)

plot_resarch_stage_count = research_stage_count %>%
  arrange(n) %>%
  mutate(research_stage = reorder(research_stage, n)) %>%
  ggplot(aes(x = research_stage, y = n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n),
            hjust = -0.1,
            size = 4) +
  coord_flip() +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
  labs(x = NULL,
       y = "Count (n)",
       title = "Research Stage") +
  expand_limits(y = max(research_stage_count$n) * 1.1)
plot_resarch_stage_count

plot_area_count = area_count %>%
  arrange(n) %>%
  mutate(area = reorder(area, n)) %>%
  ggplot(aes(x = area, y = n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n),
            hjust = -0.1,
            size = 4) +
  coord_flip() +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(x = NULL,
       y = "Count (n)",
       title = "Area") +
  expand_limits(y = max(area_count$n) * 1.1)
plot_area_count




plot_gender_count = gender_count %>%
  arrange(n) %>%
  mutate(gender = reorder(gender, n)) %>%
  ggplot(aes(x = gender, y = n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n),
            hjust = -0.1,
            size = 4) +
  coord_flip() +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(x = NULL,
       y = "Count (n)",
       title = "Gender") +
  expand_limits(y = max(gender_count$n) * 1.1)

plot_gender_count

plot_position_count = position_count %>%
  arrange(n) %>%
  mutate(position = reorder(position, n)) %>%
  ggplot(aes(x = position, y = n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n),
            hjust = -0.1,
            size = 4) +
  coord_flip() +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(x = NULL,
       y = "Count (n)",
       title = "Position") +
  expand_limits(y = max(position_count$n) * 1.1)

plot_position_count

plot_category_of_work_count = category_of_work_count %>%
  arrange(n) %>%
  mutate(category_of_work = reorder(category_of_work, n)) %>%
  ggplot(aes(x = category_of_work, y = n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n),
            hjust = -0.1,
            size = 4) +
  coord_flip() +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(x = NULL,
       y = "Count (n)",
       title = "Category of Work") +
  expand_limits(y = max(category_of_work_count$n) * 1.1)

plot_category_of_work_count



#overall emotions visualization by gender

gender_emotions = data_recoded %>% 
  select(gender, starts_with("feeling_") )
gender_emotions = gender_emotions %>%  
  mutate(gender = if_else(gender == "Non-binary / third gender / gender non-conforming", "non-binary", gender)) %>% 
  mutate(gender = factor(gender))

gender_emotions = gender_emotions %>% 
  select(-22)
gender_emotions = gender_emotions %>%   
  mutate(across(12:21, ~ 8 - .x)) %>% 
  drop_na() %>% 
  group_by(gender) %>% 
  summarise(
    across(
      .cols = starts_with("feeling_"),
      .fns  = ~ {
        x <- .x
        x <- x[!is.na(x) & x != 0]
        if (length(x) == 0) NA_real_ else mean(x)},.names = "mean_{.col}"))

gender_emotions_mean = gender_emotions %>% 
  mutate(mean_emotion = rowMeans(across(c(starts_with("mean_"))))) %>% 
  select(gender, mean_emotion) %>% 
  arrange(mean_emotion) 

gender_emotions_mean %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(mean_emotion) %>% 
  ggplot(aes(y = reorder(gender, mean_emotion),
             x = dev,
             fill = mean_emotion)) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(-3, 3),
    breaks = -3:3,
    labels = 1:7
  ) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(title = "Overall Emotional Landscape per Gender",
       x = "",
       y = "") +
  theme(
    axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )


#overall emotions visualization by research stage

research_stage_emotions = data_recoded %>% 
  select(research_stage, starts_with("feeling_"))

research_stage_emotions = research_stage_emotions %>% 
  mutate(
    across(2:21, ~ na_if(.x, 0))
  )

summary(research_stage_emotions)
research_stage_emptions = research_stage_emotions

research_stage_emotions = research_stage_emotions %>% 
  select(-22)
research_stage_emotions = research_stage_emotions %>%   
  mutate(across(12:21, ~ 8 - .x)) %>% 
  drop_na() %>% 
  group_by(research_stage) %>% 
  summarise(
    across(.cols = starts_with("feeling_"),
      .fns  = ~ { x <- .x
        x <- x[!is.na(x) & x != 0]
        if (length(x) == 0) NA_real_ else mean(x)},.names = "mean_{.col}"))

research_stage_emotions_mean = research_stage_emotions %>% 
  mutate(mean_emotion = rowMeans(across(c(starts_with("mean_"))))) %>% 
  select(research_stage, mean_emotion) %>% 
  arrange(mean_emotion)
 
research_stage_emotions_mean %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(mean_emotion) %>% 
  ggplot(aes(y = reorder(research_stage, mean_emotion),
             x = dev,
             fill = mean_emotion)) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(-3, 3),
    breaks = -3:3,
    labels = 1:7
  ) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(title = "Overall emotional landscape per research_stage", x = "",
       y = "") +
  theme(
    axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

  

#overall emotion visualization area
area_emotions = data_recoded %>% 
  select(area, starts_with("feeling_") )


area_emotions = area_emotions %>% 
  select(-22)
area_emotions = area_emotions %>%   
  mutate(across(starts_with("feeling_"),
                ~ as.numeric(as.character(.x)))) %>% 
  mutate(across(12:21, ~ 8 - .x)) %>% 
  drop_na() %>% 
  group_by(area) %>%
  summarise(across(starts_with("feeling_"),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"))
area_emotions = area_emotions[-1,]

area_emotions_mean = area_emotions %>% 
  mutate(mean_emotion = rowMeans(across(c(starts_with("mean_"))))) %>% 
  select(area, mean_emotion) %>% 
  arrange(mean_emotion)

area_emotions_mean %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(mean_emotion) %>% 
  ggplot(aes(y = reorder(area, mean_emotion),
             x = dev,
             fill = mean_emotion)) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(-3, 3),
    breaks = -3:3,
    labels = 1:7
  ) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(title = "Overall emotional landscape per field",
       x = "",
       y = "") +
  theme(
    axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )


#overall emotions position-wise visualization
position_emotions = data_recoded %>% 
  select(position, starts_with("feeling_") )

position_emotions = position_emotions %>% 
  select(-22)
position_emotions = position_emotions %>%   
  mutate(across(starts_with("feeling_"),
                ~ as.numeric(as.character(.x)))) %>% 
  mutate(across(12:21,~ 8 - .x)) %>% 
  drop_na() %>% 
  group_by(position) %>%
  summarise(across(starts_with("feeling_"),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"))
position_emotions = position_emotions[-1,]

position_emotions_mean = position_emotions %>% 
  mutate(mean_emotion = rowMeans(across(c(starts_with("mean_"))))) %>% 
  select(position, mean_emotion) %>% 
  arrange(mean_emotion)

position_emotions_mean %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(mean_emotion) %>% 
  ggplot(aes( y = reorder(position, mean_emotion), x = dev, fill = mean_emotion)) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(-3, 3),
    breaks = -3:3,
    labels = 1:7) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(title = "Overall emotional landscape per position", x = "", y = "") +
  theme( axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none" )


combined_emotions <- bind_rows(
  area_emotions_mean %>% 
    mutate(group = "Area",
           category = area),
  
  position_emotions_mean %>% 
    mutate(group = "Position",
           category = position), 
  
  gender_emotions_mean %>% 
    mutate(group = "Gender", 
           category = gender), 
  
  research_stage_emotions_mean %>% 
    mutate(group = "Research Stage",
           category = research_stage)
) %>% 
  select(group, category, mean_emotion)

#overall emotion visualization --> country
country_emotions = data_recoded %>%  
  select(ResponseId, country, country_other, starts_with("feeling_")) %>% 
mutate(
  mutate(across(starts_with("feeling_"),
                ~ as.numeric(as.character(.x))))) %>% 
    mutate(across(14:23,~ 8 - .x)) %>% 
  mutate(mean_feeling = rowMeans(across(feeling_interest:feeling_anger)))

country_emotions = country_emotions %>% 
  select(-24) %>% 
  pivot_longer(cols = c(country, country_other), 
               names_to = "group", 
               values_to = "country") %>%
  drop_na()
country_emotions = country_emotions %>% 
filter(!(country == "")) %>% 
  mutate(country = if_else(country == "N Macedonia", "North Macedonia", country)) %>% 
  filter(!(ResponseId == "R_7YyTHrAj3Gc1fHP")) 
target_id <- "R_7gbaE2EDWdQ3wOo"
target_text <- "Also Germany Australia and recently China"


split_row <- country_emotions %>%
  filter(ResponseId == target_id, country == target_text) %>%
  mutate(
    country = str_replace_all(country, "(?<=[a-z])(?=[A-Z])", " "),
    country = str_remove(country, "^Also\\s+"),
    country = str_remove(country, "\\s+and recently\\s+"),
    country = str_squish(country),
    country = str_replace_all(country, "\\s+", ",")
  ) %>%
  separate_rows(country, sep = ",")

rest <- country_emotions %>%
  filter(!(ResponseId == target_id & country == target_text))

country_emotions_clean <- bind_rows(split_row, rest)

country_emotions_clean = country_emotions_clean %>% 
  mutate(country = if_else(country == "Did my PhD in the US graduated in August and moved to Australia for postdoc", "United States of America", country)) %>% 
  mutate(country = if_else(country == "i work from home from italy", "Italy", country)) %>% 
  mutate(country = if_else(country == "Israel I am doing a collaboration in italy", "Israel", country)) %>% 
  mutate(country = if_else(country == "Actually Im from Palestine unfortunatley there is no mention for state of Palestine", "Palestine", country)) %>% 
  mutate(country = if_else(country == "China ", "China", country)) %>% 
  mutate(country = if_else(country == "USA", "United States of America", country)) %>% 
  mutate(country = if_else(country == "UK", "United Kingdom of Great Britain and Northern Ireland", country)) %>% 
  mutate(country = if_else(country == "Makedonija", "Macedonia", country)) %>% 
  mutate(country = if_else(country == "I am from Canada and have done research there", "Canada", country)) %>% 
  mutate(country = if_else(country == "south africa", "South Africa", country)) %>% 
  mutate(country = if_else(country == "England", "United Kingdom of Great Britain and Northern Ireland", country)) %>% 
  mutate(country = if_else(country == "Macedonia", "North Macedonia", country)) %>%
  filter(!(country == "Africa")) %>% 
  mutate(country = if_else(country == "United Kingdom of Great Britain and Northern Ireland", "UK", country)) %>% 
  mutate(country = if_else(country == "United States of America", "USA", country))

country_emotions_clean <- bind_rows(
  country_emotions_clean %>%
    filter(ResponseId == "R_2GQ90ZxUHZ6JyFB", country == 	
             "Burkina Faso USA") %>%
    mutate(country = str_replace(country, "\\s+USA\\b", ",USA")) %>%  # insert delimiter before USA
    separate_rows(country, sep = ","),
  country_emotions_clean%>%
    filter(!(ResponseId == "R_2GQ90ZxUHZ6JyFB"  & country == "Burkina Faso USA")))
country_emotions_clean = bind_rows(
  country_emotions_clean %>% 
    filter(ResponseId == "R_7gbaE2EDWdQ3wOo" & country == "AustraliaChina") %>% 
    mutate(country = str_replace(country, "China\\b", ",China ")) %>% 
    separate_rows(country, sep = ","), 
  country_emotions_clean %>% 
    filter(!(ResponseId == "R_7gbaE2EDWdQ3wOo" & country == "AustraliaChina")))



country_emotions_clean <- country_emotions_clean %>%
  mutate(
    continent = case_when(
      country %in% c("Algeria", "Burkina Faso", "Egypt", 
                     "Mali", "South Africa", "Sudan") ~ "Africa",
      country %in% c("Argentina", "Brazil", "Chile", "Costa Rica", 
                     "Colombia") ~ "South America/ Central America",
      country %in% c("Canada", "Mexico", 
                     "USA") ~ "North America",
      country %in% c("Austria", "Belgium", "Croatia", "Czech Republic",
                     "Denmark", "Finland", "France", "Germany", "Greece",
                     "Hungary", "Iceland", "Ireland", "Italy", "Lithuania",
                     "Netherlands", "North Macedonia", "Norway", "Poland",
                     "Portugal", "Romania", "Serbia", "Slovenia", "Spain",
                     "Sweden", "Switzerland",
                     "UK") ~ "Europe",
      country %in% c("China", "Hong Kong (S.A.R.)", "India", "Iran", "Iraq",
                     "Israel", "Japan", "Lebanon", "Malaysia", "Pakistan",
                     "Palestine", "Philippines", "Saudi Arabia", "Singapore",
                     "South Korea", "Sri Lanka", "Taiwan", "Thailand") ~ "Asia",
      country == "Turkey" ~ "Europe", 
      country %in% c("Australia", "New Zealand") ~ "Oceania", TRUE ~ NA_character_))


country_emotions_clean = country_emotions_clean %>% 
  mutate(country = factor(country))
levels(country_emotions_clean$country)

#Europe
europe_emotions_mean <- country_emotions_clean %>% 
  filter(continent == "Europe") %>% 
  group_by(country) %>% 
  summarise(
    mean_emotion = mean(mean_feeling, na.rm = TRUE),
    .groups = "drop"
  )
europe_emotions_mean %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(mean_emotion) %>% 
  ggplot(aes(y = reorder(country, mean_emotion), x = dev,fill = mean_emotion )) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-3, 3), breaks = -3:3, labels = 1:7) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(title = "Overall emotional landscape – Europe", x = "", y = "") +
  theme(axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none")

#Asia
asia_emotions_mean = country_emotions_clean %>% 
  filter(continent == "Asia") %>% 
  group_by(country) %>% 
  summarise(
    mean_emotion = mean(mean_feeling, na.rm = TRUE),
    .groups = "drop"
  )
asia_emotions_mean %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(mean_emotion) %>% 
  ggplot(aes(y = reorder(country, mean_emotion), x = dev,fill = mean_emotion )) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-3, 3), breaks = -3:3, labels = 1:7) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(title = "Overall emotional landscape – Asia", x = "", y = "") +
  theme(axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
        plot.title  = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")+
  theme(axis.ticks.y = element_blank())

#Africa
africa_emotions_mean = country_emotions_clean %>% 
  filter(continent == "Africa") %>% 
  group_by(country) %>% 
  summarise(
    mean_emotion = mean(mean_feeling, na.rm = TRUE),
    .groups = "drop"
  )
africa_emotions_mean %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(mean_emotion) %>% 
  ggplot(aes(y = reorder(country, mean_emotion), x = dev,fill = mean_emotion )) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-3, 3), breaks = -3:3, labels = 1:7) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(title = "Overall emotional landscape – Africa", x = "", y = "") +
  theme(axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
        plot.title  = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")+
  theme(axis.ticks.y = element_blank())

#North America
NorthAmerica_emotions_mean = country_emotions_clean %>% 
  filter(continent == "North America") %>% 
  group_by(country) %>% 
  summarise(
    mean_emotion = mean(mean_feeling, na.rm = TRUE),
    .groups = "drop"
  )
NorthAmerica_emotions_mean %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(mean_emotion) %>% 
  ggplot(aes(y = reorder(country, mean_emotion), x = dev,fill = mean_emotion )) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-3, 3), breaks = -3:3, labels = 1:7) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(title = "Overall emotional landscape – North America", x = "", y = "") +
  theme(axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
        plot.title  = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")+
  theme(axis.ticks.y = element_blank())


#South America/ Central America
SouthcentralAmerica_emotions_mean = country_emotions_clean %>% 
  filter(continent == "South America/ Central America") %>% 
  group_by(country) %>% 
  summarise(
    mean_emotion = mean(mean_feeling, na.rm = TRUE),
    .groups = "drop"
  )
SouthcentralAmerica_emotions_mean %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(mean_emotion) %>% 
  ggplot(aes(y = reorder(country, mean_emotion), x = dev,fill = mean_emotion )) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-3, 3), breaks = -3:3, labels = 1:7) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(title = "Overall emotional landscape – South America/ Central America", x = "", y = "") +
  theme(axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
        plot.title  = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")+
  theme(axis.ticks.y = element_blank())

#Oceania
oceania_emotions_mean = country_emotions_clean %>% 
  filter(continent == "Oceania") %>% 
  group_by(country) %>% 
  summarise(
    mean_emotion = mean(mean_feeling, na.rm = TRUE),
    .groups = "drop"
  )
oceania_emotions_mean %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(mean_emotion) %>% 
  ggplot(aes(y = reorder(country, mean_emotion), x = dev,fill = mean_emotion )) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-3, 3), breaks = -3:3, labels = 1:7) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(title = "Overall emotional landscape – Oceania", x = "", y = "") +
  theme(axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
        plot.title  = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")+
  theme(axis.ticks.y = element_blank())

#Based on Frequency

top10_countries <- country_emotions_clean %>% 
  count(country, sort = TRUE) %>% 
  slice_head(n = 10) %>% 
  pull(country)

country_11_groups <- country_emotions_clean %>% 
  mutate(
    country_group = if_else(
      country %in% top10_countries,
      country,
      "Other countries")) %>% 
  group_by(country_group) %>% 
  summarise(
    mean_emotion = mean(mean_feeling, na.rm = TRUE),
    n = n(),   # optional, but nice to keep
    .groups = "drop")

country_11_groups %>% 
  mutate(dev = mean_emotion - 4) %>% 
  arrange(desc(n)) %>%   # arrange by frequency
  ggplot(aes(
    y = reorder(country_group, ifelse(country_group == "Other countries", -Inf, n)),
    x = dev,
    fill = mean_emotion)) +
  geom_col(width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(-3, 3),
    breaks = -3:3,
    labels = 1:7) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_tufte() +
  labs(
    title = "Overall emotional landscape – top 10 countries vs others", x = "", y = "" ) +
  theme(
    axis.text.y = element_text(size = 11, angle = 30, vjust = -3),
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none")+
  theme(axis.ticks.y = element_blank())



#important sidenote --> if you want to flip the reverse-coded items, multiplying by -1 is not a great approach
"it is a 7 point likert scale --> the proper way to do that is not to center the mean around 0, but to center
the mean around the middle of the scale, so 4 --> correct way to do that: if someone has 1 on happy and 7 on sad --> if 7 is flipped by 8-x, so 8-7, then sadness will be worth 1 at the end of the analysis, so it will pull the mean to 0.5 --> and that is correct methodologically 
Because: if the mean would be centered around 0 with with a negative flip, the scale would be stretched artificially"
data_recoded_mean_feeling = data_recoded %>% 
  mutate(across(starts_with("feeling_"),
                ~ as.numeric(as.character(.x)))) %>% 
  mutate(
    across(c(feeling_sadness, feeling_anger), ~ 8 - .)
  ) %>% 
  mutate(mean_feeling = rowMeans(across(feeling_interest:feeling_anger)))

position_emotions = position_emotions %>% 
  select(-22)
position_emotions = position_emotions %>%   
  mutate(across(starts_with("feeling_"),
                ~ as.numeric(as.character(.x)))) %>% 
  mutate(across(12:21,~ 8 - .x)) %>% 
  drop_na() %>% 
  group_by(position) %>%
  summarise(across(starts_with("feeling_"),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"))
position_emotions = position_emotions[-1,]



#Analysis - valszeg ANOVA-t nem tudunk csinálni, mivel túl sok a missing datapoint --> csak na-k kizárásával lehetne
mod1 = lm(mean_feeling ~ area, data = data_recoded_mean_feeling)
mod1  
summary(mod1)

mod2 = lm(mean_feeling ~ research_stage+position, data = data_recoded_mean_feeling)
summary(mod2)

mod3 = lm(mean_feeling ~ country, data = data_recoded_mean_feeling)
summary(mod3)

#Content Analysis of the responses of participants to specific feelings
feeling_cols <- c(
  "feeling_interest","feeling_amusement","feeling_pride","feeling_joy","feeling_pleasure",
  "feeling_contentment","feeling_love","feeling_admiration","feeling_relief","feeling_compassion",
  "feeling_sadness","feeling_guilt","feeling_regret","feeling_shame","feeling_disappointment",
  "feeling_fear","feeling_disgust","feeling_contempt","feeling_hate","feeling_anger"
)



positive_feelings <- c(
  "feeling_interest","feeling_amusement","feeling_pride","feeling_joy","feeling_pleasure",
  "feeling_contentment","feeling_love","feeling_admiration","feeling_relief","feeling_compassion"
)
negative_feelings <- setdiff(feeling_cols, positive_feelings)
neg_pattern <- regex(
  "\\bfear\\b|\\bunfair\\w*\\b|unfairness|\\bafraid\\b|\\bangry\\b|\\bdisappointment\\b|\\bdisappointed\\b|\\blonely\\b|\\bfrustrat\\w*\\b",
  ignore_case = TRUE
)


feelings_resolved <- data_recoded %>%
  mutate(neg_cue = str_detect(cause_stage, neg_pattern)) %>%
  pivot_longer(
    cols = all_of(feeling_cols),
    names_to = "feeling",
    values_to = "rating"
  ) %>%
  mutate(
    emotion_valence = if_else(feeling %in% positive_feelings, "Positive", "Negative")
  ) %>%
  group_by(ResponseId) %>%
  mutate(
    top = max(rating, na.rm = TRUE),
    n_top = sum(rating == top, na.rm = TRUE)   # how many feelings share the max?
  ) %>%
  filter(n_top == 1, rating == top) %>%        # <-- drops exact ties
  select(ResponseId, feeling, cause_stage)   

  
overall_counts <- df_long_feelings_excluded %>%
  count(attribution_category) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(prop)

ggplot(overall_counts,
       aes(x = reorder(attribution_category, prop),
           y = prop)) +
  geom_col(width = 0.6) +
  coord_flip() +
  labs(title = "Overall Attribution of Research-Related Emotions",
       x = "",
       y = "Proportion of Responses") +
  theme_minimal(base_size = 13)

response_max_feeling <- feelings_resolved %>%
  pivot_wider(
    id_cols = ResponseId,
    names_from = feeling,
    values_from = cause_stage
  )

response_max_feeling_clean <- response_max_feeling %>%
  filter(if_any(-ResponseId, ~ !is.na(.) & . != ""))


df_long_feelings_included <- response_max_feeling_clean %>%
  pivot_longer(
    -ResponseId,
    names_to = "feeling",
    values_to = "text"
  ) %>%
  filter(!is.na(text)) %>%
  mutate(
    valence = if_else(feeling %in% positive_feelings, "Positive", "Negative")
  )

df_long_feelings_excluded = df_long_feelings_included %>% 
  select(-feeling)


#Manual recoding of the inadequately rated valence values
"Those answers that deliberately stated uncertainty, lack of emotions or were uncategorizable were excluded from this part of the  analysis."
"In the case of elaborating on both specifically indicated negative and positive emotions, the one that was expressed more intensely, or was put a 
bigger emphasis on was kept in the analysis."
pattern_override = str_detect(df_long_feelings_excluded$text, regex("pain|fear|lonel|frustrat|anger|angry|sad|depress|lonliness", ignore_case = TRUE))

df_long_feelings_excluded = df_long_feelings_excluded %>% 
  mutate(valence = if_else
         (str_detect(df_long_feelings_excluded$text,
                     regex("pain|fear|lonel|frustrat|anger|angry|sad|depress|lonliness|disappointing|deadline|afraid|unfair", 
                           ignore_case = TRUE)), "Negative", valence))

valence_text_clean = df_long_feelings_excluded %>% 
  mutate(valence = if_else(ResponseId == "R_2DM0KradsyH5SJx", "Negative", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_8HkJOSSkP2z3qFZ", "Negative", valence )) %>% 
  mutate(valence = if_else(ResponseId == "R_8qOcYz4JunPebzb", "Negative", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_2pn6yH4WpwQ2ETL", "Negative", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_1t15oqA7lh9mK9H", "Negative", valence)) %>% 
  filter(!(ResponseId %in% c("R_4hcm4C4cWNxSP6H", "R_2Ywag3d3HukQMI9",
                             "R_8ASwTJePw7kl8TX", "R_81nV70oChoOIjhg",
                             "R_9m7QBevWcdUYLJu", "R_6gYptx9fug6DzGX",
                             "R_8QhMs5GXced14Gt", "R_4s7yO8dtN3GQ20m",
                             "R_8WOnkN8gvu3yRJ7", "R_8tAmJkkGR9OFqlo"))) %>% 
  mutate(valence = if_else(ResponseId == "R_8mfSuJuZ9YhxtE5", "Negative", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_9B9zAmE5ITzL2xZ", "Negative", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_2sbIPrGISoflj96", "Negative", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_8zeSEXel9NQrjgg", "Negative", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_1BW378WmfwSyet6", "Negative", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_2aDjXvAnllacTEL", "Positive", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_2EYu6IBp4onL6ge", "Positive", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_2sbhVBjiBhtL9bv", "Negative", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_41id7PR9MQPFQaf", "Negative", valence)) %>% 
  mutate(valence = if_else(ResponseId == "R_2lRDhzFCrSjaNJl", "Negative", valence))

  
#Attributional analysis (?)

only_text = valence_text_clean %>% 
  select(text)

saveRDS(only_text, "onlytext.rds")


"The attributional categories were selected after manually reading through the first 100 responses of 754 items. Further refinements to the selection criteria were added after manual checks"
valence_text_clean <- valence_text_clean %>%
  mutate(
    attribution_category = case_when(
      
      str_detect(text, regex("reviewer|editor|revision|reject|publication|journal", ignore_case = TRUE)) ~ "Peer Review",
      
      str_detect(text, regex("data|analysis|results|insight|experiment|finding|discover", ignore_case = TRUE)) ~ "Data/Results",
      
      str_detect(text, regex("project|employ|paper|\\bwork\\b", ignore_case = TRUE)) ~ "Project/Work-related",
      
      str_detect(text, regex("ethic|bureaucr|paperwork|funding|NIH|permit|legal|funded|funding|job|employ", ignore_case = TRUE)) ~ "Institutional",
      
      str_detect(text, regex("co-investigator|collaborat|team|mentor|colleague|supervisor|co-author|boss|\\bPI\\b|help", ignore_case = TRUE)) ~ "Collaboration",
      
      str_detect(text, regex("deadline|timeline|pressure|workload|slow|delay", ignore_case = TRUE)) ~ "Time Pressure",
      
      str_detect(text, regex("guilt|fear|self|my fault|competence|fail|underperform", ignore_case = TRUE)) ~ "Self-Evaluation",
      
      str_detect(text, regex("interest|fascinat|passion|exciting|love research|idea|pleasure|creative", ignore_case = TRUE)) ~ "Intrinsic Interest",
      
      str_detect(text, regex("sexism|bias|underfund|peripheral|mobing|institution|racism|female|woman", ignore_case = TRUE)) ~ "Structural Inequality",
      
      str_detect(text, regex("lonely|isolat", ignore_case = TRUE)) ~ "Isolation",
      
      TRUE ~ "Other/Unclear"
    ))


#Visualization
attribution_counts <- valence_text_clean %>%
  count(attribution_category) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(prop)

ggplot(attribution_counts,
       aes(x = reorder(attribution_category, prop),
           y = prop)) +
  geom_col(width = 0.6) +
  coord_flip() +
  labs(title = "Overall Attribution of Research-Related Emotions",
       x = "",
       y = "Proportion of Responses") +
  theme_minimal(base_size = 13)

valence_counts <- valence_text_clean %>%
  count(valence, attribution_category) %>%
  group_by(valence) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(valence_counts,
       aes(x = reorder(attribution_category, prop),
           y = prop,
           fill = valence)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ valence) +
  scale_fill_manual(values = c(
    "Positive" = "red",
    "Negative" = "blue"
  )) +
  labs(
    title = "Attributional Factors by Emotional Valence",
    x = "",
    y = "Proportion within Valence"
  ) +
  theme_minimal(base_size = 13)


#Content analysis of the participants' responses about their general feeling 
df_feeling_general = data_recoded %>% 
  drop_na() %>% 
  filter(!(cause_general %in% c("", "-", "na", "Na", "NA", "I don't know", "I dont know"))) %>% 
  filter(!(feeling_general_1 == 0)) %>% 
  select(ResponseId,feeling_general_1, cause_general ) %>% 
  mutate(valence = if_else(feeling_general_1 > 0, "positive", "negative"))

df_feeling_general<- df_feeling_general %>%
  mutate( attribution_category = case_when(
      
      str_detect(cause_general, regex("reviewer|editor|revision|reject|publication|journal", ignore_case = TRUE)) ~ "Peer Review",
      
      str_detect(cause_general, regex("data|analysis|results|insight|experiment|finding|discover", ignore_case = TRUE)) ~ "Data/Results",
      
      str_detect(cause_general, regex("project|employ|paper|\\bwork\\b", ignore_case = TRUE)) ~ "Project/Work-related",
      
      str_detect(cause_general, regex("ethic|bureaucr|paperwork|funding|NIH|permit|legal|funded|funding|job|employ", ignore_case = TRUE)) ~ "Institutional",
      
      str_detect(cause_general, regex("co-investigator|collaborat|team|mentor|colleague|supervisor|co-author|boss|\\bPI\\b|help", ignore_case = TRUE)) ~ "Collaboration",
      
      str_detect(cause_general, regex("deadline|timeline|pressure|workload|slow|delay", ignore_case = TRUE)) ~ "Time Pressure",
      
      str_detect(cause_general, regex("guilt|fear|self|my fault|competence|fail|underperform", ignore_case = TRUE)) ~ "Self-Evaluation",
      
      str_detect(cause_general, regex("interest|fascinat|passion|exciting|love research|idea|pleasure|creative", ignore_case = TRUE)) ~ "Intrinsic Interest",
      
      str_detect(cause_general, regex("sexism|bias|underfund|peripheral|mobing|institution|racism|female|woman", ignore_case = TRUE)) ~ "Structural Inequality",
      
      str_detect(cause_general, regex("lonely|isolat", ignore_case = TRUE)) ~ "Isolation",
      
      TRUE ~ "Other/Unclear"
    ))

#Visualization
attribution_counts_general <- df_feeling_general %>%
  count(attribution_category) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(prop)

ggplot(attribution_counts_general,
       aes(x = reorder(attribution_category, prop),
           y = prop)) +
  geom_col(width = 0.6) +
  coord_flip() +
  labs(title = "Overall Attribution of Research-Related Emotions",
       x = "",
       y = "Proportion of Responses") +
  theme_minimal(base_size = 13)

valence_counts_general <- df_feeling_general %>%
  count(valence, attribution_category) %>%
  group_by(valence) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(valence_counts_general,
       aes(x = reorder(attribution_category, prop),
           y = prop,
           fill = valence)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ valence) +
  scale_fill_manual(values = c(
    "positive" = "red",
    "negative" = "blue"
  )) +
  labs(
    title = "Attributional Factors by Emotional Valence",
    x = "",
    y = "Proportion within Valence"
  ) +
  theme_minimal(base_size = 13)







