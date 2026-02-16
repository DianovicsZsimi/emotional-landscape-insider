"SA analysis"

install.packages("tidytext")
install.packages("Hmisc")
install.packages("sentimentr")
install.packages("zoo")
install.packages("flextable")

library(dplyr)
library(flextable)
library(ggplot2)
library(Hmisc)
library(sentimentr)
library(stringr)
library(tibble)
library(tidyr)
library(tidytext)
library(zoo)
library(here)

my_data = read.csv("C:/Emotional landscape/raw_data.csv")

data_recoded = my_data[-1:-2, ]

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
levels(data_recoded$position)


text_assistant_prof = data_recoded %>% 
  select(cause_stage, position) %>% 
  drop_na() %>% 
  filter(!(cause_stage == "")) %>% 
  filter(position == "Assistant professor")

text_associate_professor = data_recoded %>% 
  select(cause_stage, position) %>% 
  drop_na() %>% 
  filter(!(cause_stage == "")) %>% 
  filter(position == "Associate professor")

text_full_professor = data_recoded %>% 
  select(cause_stage, position) %>% 
  drop_na() %>% 
  filter(!(cause_stage == "")) %>% 
  filter(position == "Full professor")

text_nonacademic_researcher = data_recoded %>% 
  select(cause_stage, position) %>% 
  drop_na() %>% 
  filter(!(cause_stage == "")) %>% 
  filter(position == "Non-academic researcher")

text_other = data_recoded %>% 
  select(cause_stage, position) %>% 
  drop_na() %>% 
  filter(!(cause_stage == "")) %>% 
  filter(position == "Other")

text_PhD = data_recoded %>% 
  select(cause_stage, position) %>% 
  drop_na() %>% 
  filter(!(cause_stage == "")) %>% 
  filter(position == "PhD student")

text_Postdoc = data_recoded %>% 
  select(cause_stage, position) %>% 
  drop_na() %>% 
  filter(!(cause_stage == "")) %>% 
  filter(position == "Postdoc")

text_research_assistant = data_recoded %>% 
  select(cause_stage, position) %>% 
  drop_na() %>% 
  filter(!(cause_stage == "")) %>% 
  filter(position == "Research assistant")



readr::write_csv(my_data_only_text, "my_data_only_text.csv")
df2 <- readr::read_csv("my_data_only_text.csv", show_col_types = FALSE)


txtclean <- function(x, position) {
  require(dplyr)
  require(stringr)
  require(tibble)
  x <- x %>%
    iconv(to = "UTF-8") %>%
    base::tolower() %>%
    paste0(collapse = " ") %>%
    stringr::str_squish() %>%
    stringr::str_split(" ") %>%
    unlist() %>%
    tibble::tibble() %>%
    dplyr::select(word = 1, everything()) %>%
    dplyr::mutate(position = position) %>%
    dplyr::anti_join(stop_words) %>%
    dplyr::mutate(word = str_remove_all(word, "\\W")) %>%
    dplyr::filter(word != "")
}

researchassistant_clean = txtclean(text_research_assistant, "researchassistant")

assistantprof_clean = txtclean(text_assistant_prof, "assistantprof")


associateprof_clean = txtclean(text_associate_professor, "associateprof")


fullprof_clean <- txtclean(text_full_professor, "fullprofessor")
nonacademic_clean <- txtclean(text_nonacademic_researcher, "nonacademic")
other_clean <- txtclean(text_other, "other")
PhD_clean = txtclean(text_PhD, "PhD")
Postdoc_clean = txtclean(text_Postdoc, "Postdoc")


nrc <- readRDS(here::here("C:/Emotional landscape/nrc.rda"))

positions <- rbind(assistantprof_clean, associateprof_clean,
                   fullprof_clean, nonacademic_clean, other_clean, PhD_clean,
                   Postdoc_clean, researchassistant_clean) %>%
  group_by(position) %>%
  mutate(words = n()) %>%
 left_join(nrc) %>%
  mutate(
    position = factor(position),
    sentiment = factor(sentiment)
  )

positions_percentages = positions %>% 
  mutate(position = factor(position, levels = c("other", "nonacademic",
                                                "researchassistant", "PhD", "Postdoc",
                                                "assistantprof", "associateprof","fullprofessor"
  ))) %>% 
  group_by(position) %>% 
  group_by(position, sentiment) %>% 
  summarise(sentiment = unique(sentiment),
             sentiment_freq = n(), 
             words = unique(words)) %>% 
  filter(is.na(sentiment) == FALSE) %>% 
  mutate(percentage = round(sentiment_freq/words * 100, 1))

positions_percentages %>%
  filter(
    sentiment != "positive",
    sentiment != "negative"
  ) %>%
  ggplot(aes(sentiment, percentage, fill = position)) +
  geom_bar(
    stat = "identity",
    position = position_dodge()
  ) +
  scale_fill_manual(name = "", values = c("grey80", "gray40", "grey20","red", "red4", "orange", "orange4", "yellow3")) +
  theme_bw() +
  theme(legend.position = "top")

positions_percentages %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  select(-percentage, -words) %>%
  group_by(position) %>% 
  mutate(
    sentiment_sum = sum(sentiment_freq),
    positive = sentiment_sum - sentiment_freq
  ) %>%
  ungroup() %>% 
  filter(sentiment != "positive") %>%
  rename(negative = sentiment_freq) %>%
  select(position, positive, negative) %>%
  group_by(position) %>%
  summarise(polarity = positive / negative) %>%
  ggplot(aes(reorder(position, polarity, mean), polarity, fill = position)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(y = polarity - 0.1, label = round(polarity, 2)),
            color = "white", size = 4
  ) +
  theme_bw() +
  labs(
    y = "Polarity\n(ration of positive to negative emitives)",
    x = ""
  ) +
  coord_cartesian(y = c(0, 2)) +
  scale_y_continuous(
    breaks = seq(0, 2, 1),
    labels = c("more negative", "neutral", "more positive")
  ) +
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(angle = 30))+
  coord_flip()+
  geom_hline(yintercept = 2, linetype = "dashed")


#research stage SA

research_stage_levels <- c(
  "Project genesis/ first thoughts/ idea creation",
  "Reality check",
  "Preparations",
  "Data collection",
  "Data analysis",
  "Report writing",
  "Revisions",
  "Waiting for journal's response and revisions",
  "Post-project stage"
)


text_by_stage <- data_recoded %>%
  select(cause_stage, research_stage) %>%
  drop_na() %>%
  filter(cause_stage != "") %>%
  mutate(
    research_stage = factor(research_stage,
                            levels = research_stage_levels)
  )

stage_clean <- text_by_stage %>%
  group_by(research_stage) %>%
  summarise(text = paste(cause_stage, collapse = " "), .groups = "drop") %>%
  rowwise() %>%
  mutate(cleaned = list(txtclean(text, research_stage))) %>%
  unnest(cleaned)


nrc <- readRDS(here::here("C:/Emotional landscape/nrc.rda"))


stages <- stage_clean %>%
  group_by(research_stage) %>%
  mutate(words = n()) %>%
  left_join(nrc) %>%
  mutate(
    research_stage = factor(research_stage,
                            levels = research_stage_levels),
    sentiment = factor(sentiment)
  )


stages_percentages <- stages %>%
  group_by(research_stage, sentiment) %>%
  summarise(
    sentiment_freq = n(),
    words = unique(words),
    .groups = "drop"
  ) %>%
  filter(!is.na(sentiment)) %>%
  mutate(
    percentage = round(sentiment_freq / words * 100, 1),
    research_stage = factor(research_stage,
                            levels = research_stage_levels)
  )

stages_percentages %>%
  filter(
    sentiment != "positive",
    sentiment != "negative"
  ) %>%
  ggplot(aes(sentiment, percentage, fill = research_stage)) +
  geom_bar(
    stat = "identity",
    position = position_dodge()
  ) +
  scale_fill_manual(name = "", values = c("grey80", "gray40", "grey20","red", "red4", "orange", "orange4", "yellow3",
                                          "pink3", "pink1")) +
  theme_bw() +
  theme(legend.position = "bottom")


stages_percentages %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  select(-percentage, -words) %>%
  group_by(research_stage) %>% 
  mutate(
    sentiment_sum = sum(sentiment_freq),
    positive = sentiment_sum - sentiment_freq
  ) %>%
  ungroup() %>% 
  filter(sentiment != "positive") %>%
  rename(negative = sentiment_freq) %>%
  select(research_stage, positive, negative) %>%
  group_by(research_stage) %>%
  summarise(polarity = positive / negative) %>%
  ggplot(aes(reorder(research_stage, polarity, mean), polarity, fill = research_stage)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(y = polarity - 0.1, label = round(polarity, 2)),
            color = "white", size = 4
  ) +
  theme_bw() +
  labs(
    y = "Polarity\n(ration of positive to negative emitives)",
    x = ""
  ) +
  coord_cartesian(y = c(0, 2)) +
  scale_y_continuous(
    breaks = seq(0, 2, 1),
    labels = c("more negative", "neutral", "more positive")
  ) +
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(angle = 30))+
  coord_flip()+
  geom_hline(yintercept = 2, linetype = "dashed")
