"Emotional landscape project"
install.packages("ggthemes")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rstatix)
library(ggthemes)
raw_data = read.csv("D:/Emotional landscape/raw_data.csv")


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
                                 
  ))
  
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



#overall emotions visualization by research stage

research_stage_emotions = data_recoded %>% 
  select(research_stage, starts_with("feeling_") )

research_stage_emotions = research_stage_emotions %>% 
  select(-22)
research_stage_emotions = research_stage_emotions %>%   
  mutate(across(starts_with("feeling_"),
                ~ as.numeric(as.character(.x)))) %>% 
  mutate(across(12:21, ~ .x * -1)) %>% 
  drop_na() %>% 
  group_by(research_stage) %>%
  summarise(across(starts_with("feeling_"),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"))

research_stage_emotions_mean = research_stage_emotions %>% 
  mutate(mean_emotion = rowMeans(across(c(starts_with("mean_"))))) %>% 
  select(research_stage, mean_emotion) %>% 
  arrange(mean_emotion)
 
research_stage_emotions_mean %>% 
  arrange(mean_emotion) %>% 
  ggplot()+
  aes(y = reorder(research_stage, mean_emotion), x = mean_emotion, fill = mean_emotion)+ 
  geom_col(width = 0.3)+
  scale_fill_gradient(low = "orange", high = "red")+
  theme_tufte()+
  labs(title = "Overal emotional landscape per stage", x = "", y = "")+
  theme(axis.text.y = element_text(size = 11, angle = 45, vjust = -3))+
  theme(plot.title = element_text(angle = 0, hjust = 0.5, face = "bold"))
  

#overall emotion visualization area
area_emotions = data_recoded %>% 
  select(area, starts_with("feeling_") )


area_emotions = area_emotions %>% 
  select(-22)
area_emotions = area_emotions %>%   
  mutate(across(starts_with("feeling_"),
                ~ as.numeric(as.character(.x)))) %>% 
  mutate(across(12:21, ~ .x * -1)) %>% 
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
  arrange(area_emotions_mean) %>% 
  ggplot()+
  aes(y = reorder(area, mean_emotion ), x = mean_emotion, fill = mean_emotion)+ 
  geom_col(width = 0.3)+
  scale_fill_gradient(low = "orange", high = "red")+
  theme_tufte()+
  labs(title = "Overall emotional landcape per field", x = "", y = "")+
  theme(axis.text.y = element_text(size = 11, angle = 45, vjust = -3))+
  theme(plot.title = element_text(angle = 0, hjust = 0.5, face = "bold"))

#overall emotions position-wise visualization
position_emotions = data_recoded %>% 
  select(position, starts_with("feeling_") )

position_emotions = position_emotions %>% 
  select(-22)
position_emotions = position_emotions %>%   
  mutate(across(starts_with("feeling_"),
                ~ as.numeric(as.character(.x)))) %>% 
  mutate(across(12:21, ~ .x * -1)) %>% 
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
  arrange(mean_emotion) %>% 
  ggplot()+
  aes(y = reorder(position, mean_emotion), x = mean_emotion, fill = mean_emotion)+ 
  geom_col(width = 0.3)+
  scale_fill_gradient(low = "orange", high = "red")+
  theme_tufte()+
  labs(title = "Overal emotional landscape per position", x = "", y = "")+
  theme(axis.text.y = element_text(size = 11, angle = 45, vjust = -3))+
  theme(plot.title = element_text(angle = 0, hjust = 0.5, face = "bold"))

