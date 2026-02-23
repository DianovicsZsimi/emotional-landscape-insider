"Emotional landscape project"
install.packages("ggthemes")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rstatix)
library(ggthemes)
library(tidyr)
library(patchwork)

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

droplevels(research_stage_count$research_stage)
droplevels(area_count$area)
droplevels(gender_count$gender)
droplevels(position_count$position)
droplevels(category_of_work_count$category_of_work)

descriptive_plot_function <- function(data, category_var, count_var, title_text) {
  
  max_n <- max(pull(data, {{ count_var }}), na.rm = TRUE)
  
  data %>%
    arrange({{ count_var }}) %>%
    mutate({{ category_var }} := reorder({{ category_var }}, {{ count_var }})) %>%
    ggplot(aes(x = {{ category_var }}, y = {{ count_var }})) +
    geom_col(width = 0.7) +
    geom_text(aes(label = {{ count_var }}),
              hjust = -0.1,
              size = 4) +
    coord_flip() +
    theme_tufte() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    labs(x = NULL,
         y = "Count (n)",
         title = title_text) +
    expand_limits(y = max_n * 1.1)}



descriptive_plot_function(research_stage_count, research_stage, n, "Research Stage")
descriptive_plot_function(gender_count, gender, n, "Gender")
descriptive_plot_function(area_count, area, n, "Area")
descriptive_plot_function(position_count, position, n, "Position")
descriptive_plot_function(category_of_work_count, category_of_work, n, "Category of Work")


#Functions and setup
emotion_function = function(data, catvar) {
  data %>%   
    group_by({{catvar}}) %>%
    summarise(across(starts_with("feeling_"),
                     ~ mean(.x, na.rm = TRUE),
                     .names = "mean_{.col}"))
}

mean_function = function(data, catvar) {
  data %>% 
    mutate(mean_emotion = rowMeans(across(c(starts_with("mean_"))))) %>% 
    select({{catvar}}, mean_emotion) %>% 
    arrange(mean_emotion)
}

overall_emotion_plot <- function(data, group_var, mean_var,
                                 title_text = "Overall Emotional Landscape") {
  
  data %>%
    mutate(dev = {{ mean_var }} - 4) %>%
    arrange({{ mean_var }}) %>%
    ggplot(aes(
      y    = reorder({{ group_var }}, {{ mean_var }}),
      x    = dev,
      fill = {{ mean_var }})) +
    geom_col(width = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(
      limits = c(-3, 3),
      breaks = -3:3,
      labels = 1:7) +
    scale_fill_gradient(low = "orange", high = "red") +
    theme_tufte() +
    labs(
      title = title_text,
      x = "",
      y = ""
    ) +
    theme(
      axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none"
    )
}

data_recoded_clean = data_recoded %>% 
  select(gender, position, research_stage, category_of_work,
         country, country_other, area, starts_with("feeling_")) %>% 
  mutate(across(8:27, ~ na_if(.x, 0))) %>% 
  mutate(NAcount = rowSums(is.na(across(7:26)))) %>%
  mutate(across(18:27, ~ 8 - .x)) %>% 
  select(-28) %>% 
  filter(!if_all(starts_with("feeling_"), is.na))

positive_feelings <- c(
  "feeling_interest","feeling_amusement","feeling_pride","feeling_joy","feeling_pleasure",
  "feeling_contentment","feeling_love","feeling_admiration","feeling_relief","feeling_compassion"
)
negative_feelings <- c("feeling_sadness", "feeling_guilt", "feeling_regret" ,"feeling_shame", 
                       "feeling_disappointment" , "feeling_fear", "feeling_disgust", "feeling_contempt" , "feeling_hate", "feeling_anger")

overall_emotion_plot_valence <- function(data, group_var, mean_var, color_low = "orange", color_high = "red",
                            title_text = "Overall Emotional Landscape")
  {
  data %>%
    arrange({{ mean_var }}) %>%
    ggplot(aes(
      y    = reorder({{ group_var }}, {{ mean_var }}),
      x    = {{ mean_var }}, 
      fill = {{mean_var}})) +
    geom_col(width = 0.3) +
    scale_x_continuous(
      limits = c(0, 7),
      breaks = 0:7,
      labels = 0:7) +
    scale_fill_gradient(low = {{color_low}}, high = {{color_high}}) +
    theme_tufte()+
    labs(
      title = title_text,
      x = "",
      y = "") +
    theme(
      axis.text.y   = element_text(size = 11, angle = 45, vjust = -3),
      plot.title    = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none")
}



#overall emotions visualization by gender
gender_emotions = data_recoded_clean %>% 
  select(gender, starts_with("feeling_")) %>% 
  filter(!(gender == ""))
gender_emotions = gender_emotions %>%  
  mutate(gender = if_else(gender == "Non-binary / third gender / gender non-conforming", "non-binary", gender)) %>% 
  mutate(gender = factor(gender))
gender_emotions_positive = gender_emotions %>% 
  select(gender, all_of(positive_feelings))
gender_emotions_negative = gender_emotions %>% 
  select(gender, all_of(negative_feelings))
  

gender_emotions = emotion_function(gender_emotions, gender)

gender_emotions_mean = mean_function(gender_emotions, gender)

gender_emotions_positive = emotion_function(gender_emotions_positive, gender)

gender_emotions_mean_positive = mean_function(gender_emotions_positive, gender)

gender_emotions_negative = emotion_function(gender_emotions_negative, gender)

gender_emotions_mean_negative = mean_function(gender_emotions_negative, gender)


gender_pos = overall_emotion_plot_valence(gender_emotions_mean_positive, gender, mean_emotion, color_low = "orange", color_high = "red",
                             "Positive Emotions")

gender_neg = overall_emotion_plot_valence(gender_emotions_mean_negative, gender, mean_emotion, color_low = "lightblue", 
                     color_high = "blue", "Negative Emotions")

overall_emotion_plot(gender_emotions_mean, gender, mean_emotion,
                             "Overall Emotional Landscape by Gender")
combined_gender = gender_pos+gender_neg

combined_gender
#overall emotions visualization by research stage

research_stage_emotions = data_recoded_clean %>% 
  select(research_stage, starts_with("feeling_")) %>% 
  filter(!(research_stage == ""))
research_stage_emotions_positive = research_stage_emotions %>% 
  select(research_stage, all_of(positive_feelings))
research_stage_emotions_negative = research_stage_emotions %>% 
  select(research_stage, all_of(negative_feelings))


"OR"


research_stage_emotions = data_recoded_clean %>% 
  filter(!(NAcount > 10)) %>% 
  select(research_stage, starts_with("feeling_")) %>% 
  filter(!(research_stage == ""))

research_stage_emotions = emotion_function(research_stage_emotions, research_stage)

research_stage_emotions_mean = mean_function(research_stage_emotions, research_stage)

research_stage_emotions_positive = emotion_function(research_stage_emotions_positive, research_stage)

research_stage_emotions_mean_positive = mean_function(research_stage_emotions_positive, research_stage)

research_stage_emotions_negative = emotion_function(research_stage_emotions_negative, research_stage)

research_stage_emotions_mean_negative = mean_function(research_stage_emotions_negative, research_stage)


research_stage_pos = overall_emotion_plot_valence(
  research_stage_emotions_mean_positive,
  research_stage,
  mean_emotion,
  color_low = "orange",
  color_high = "red",
  "Positive Emotions"
)

research_stage_neg = overall_emotion_plot_valence(
  research_stage_emotions_mean_negative,
  research_stage,
  mean_emotion,
  color_low = "lightblue",
  color_high = "blue",
  "Negative Emotions"
)

overall_emotion_plot(
  research_stage_emotions_mean,
  research_stage,
  mean_emotion,
  "Overall Emotional Landscape by Research Stage")

combined_research_stage = research_stage_pos + research_stage_neg
research_stage_neg
research_stage_pos
combined_research_stage

#overall emotion visualization area
area_emotions = data_recoded_clean %>% 
  select(area, starts_with("feeling_")) %>% 
  filter(!(area == ""))
area_emotions_positive = area_emotions %>%
select(area, all_of(positive_feelings))
area_emotions_negative = area_emotions %>% 
  select(area, all_of(negative_feelings))

area_emotions = emotion_function(area_emotions, area)

area_emotions_mean = mean_function(area_emotions, area)

area_emotions_positive = emotion_function(area_emotions_positive, area)

area_emotions_mean_positive = mean_function(area_emotions_positive, area)

area_emotions_negative = emotion_function(area_emotions_negative, area)

area_emotions_mean_negative = mean_function(area_emotions_negative, area)


area_pos = overall_emotion_plot_valence(
  area_emotions_mean_positive,
  area,
  mean_emotion,
  color_low = "orange",
  color_high = "red",
  "Positive Emotions"
)

area_neg = overall_emotion_plot_valence(
  area_emotions_mean_negative,
  area,
  mean_emotion,
  color_low = "lightblue",
  color_high = "blue",
  "Negative Emotions"
)

overall_emotion_plot(
  area_emotions_mean,
  area,
  mean_emotion,
  "Overall Emotional Landscape by Area"
)

combined_area = area_pos + area_neg

combined_area

#overall emotions position-wise visualization
position_emotions = data_recoded_clean %>% 
  filter(!(position == "")) %>% 
  select(position, starts_with("feeling_"))
position_emotions_positive = position_emotions %>%
  select(position, all_of(positive_feelings))
position_emotions_negative = position_emotions %>% 
  select(position, all_of(negative_feelings))


position_emotions = emotion_function(position_emotions, position)

position_emotions_mean = mean_function(position_emotions, position)

position_emotions_positive = emotion_function(position_emotions_positive, position)

position_emotions_mean_positive = mean_function(position_emotions_positive, position)

position_emotions_negative = emotion_function(position_emotions_negative, position)

position_emotions_mean_negative = mean_function(position_emotions_negative, position)


position_pos = overall_emotion_plot_valence(
  position_emotions_mean_positive,
  position,
  mean_emotion,
  color_low = "orange",
  color_high = "red",
  "Positive Emotions"
)

position_neg = overall_emotion_plot_valence(
  position_emotions_mean_negative,
  position,
  mean_emotion,
  color_low = "lightblue",
  color_high = "blue",
  "Negative Emotions"
)

overall_emotion_plot(
  position_emotions_mean,
  position,
  mean_emotion,
  "Overall Emotional Landscape by Position"
)

combined_position = position_pos + position_neg

combined_position

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


continent_plot_function = function(data, title_text){
 data %>% 
   mutate(dev = mean_emotion - 4) %>% 
    arrange(mean_emotion) %>% 
    ggplot(aes(y = reorder(country, mean_emotion), x = dev,fill = mean_emotion )) +
    geom_col(width = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(-3, 3), breaks = -3:3, labels = 1:7) +
    scale_fill_gradient(low = "orange", high = "red") +
    theme_tufte() +
    labs(title = {{title_text}}, x = "", y = "") +
    theme(axis.text.y = element_text(size = 11, angle = 45, vjust = -3),
          plot.title  = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none")
  
}

continent_emotions_mean_function <- function(data, continent_name) {
  data %>%
    filter(continent == continent_name) %>%
    group_by(country) %>%
    summarise(
      mean_emotion = mean(mean_feeling, na.rm = TRUE),
      .groups = "drop"
    )
}

#Europe
europe_emotions_mean = continent_emotions_mean_function(country_emotions_clean, "Europe")
continent_plot_function(europe_emotions_mean, "Overall Emotional Landscape - Europe")

#Asia
asia_emotions_mean = continent_emotions_mean_function(country_emotions_clean, "Asia")
continent_plot_function(asia_emotions_mean, "Overall Emotional Landscape - Asia")

#Africa
africa_emotions_mean = continent_emotions_mean_function(country_emotions_clean, "Africa")
continent_plot_function(africa_emotions_mean, "Overall Emotional Landscape - Africa")
#North America
NorthAmerica_emotions_mean = continent_emotions_mean_function(country_emotions_clean, "North America")
continent_plot_function(NorthAmerica_emotions_mean, "Overall Emotional Landscape - North America")

#South America/ Central America
SouthAmerica_emotions_mean = continent_emotions_mean_function(country_emotions_clean, "South America/ Central America")
continent_plot_function(SouthAmerica_emotions_mean, "Overall Emotional Landscape - South America/Central America")

#Oceania
oceania_emotions_mean = continent_emotions_mean_function(country_emotions_clean, "Oceania")
continent_plot_function(oceania_emotions_mean, "Overall Emotional Landscape - Oceania")

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







