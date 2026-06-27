# 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rstatix)
library(ggthemes)
library(tidyr)
library(patchwork)
library(scales)
library(magrittr)
library(apyramid)

# descriptive plotting function 
descriptive_plots <- function(data,
                                          variable_name,
                                          title,
                                          custom_levels = NULL,
                                          remove_parentheses = FALSE,
                                          angle = 30) {
  
  df_counts <- data %>%
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
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3)))
  
  if (remove_parentheses) {
    p <- p + scale_x_discrete(
      labels = function(x) sub("\\s*\\(.*\\)", "", x)
    )
  }
  
  return(p)
}

# creates SD column
sd_data <- function(data) {
  sd_flang <- data %>% 
    select(
      -Progress, -Duration, -Finished, -dismiss_1, -Q22, -Q22_6_TEXT 
    ) %>% 
    mutate(across("interest":"anger", as.numeric),
           mean_landscape = rowMeans(across("interest":"anger"), na.rm = T),
           sd_landscape = apply(across("interest":"anger"), 1, sd, na.rm = T)) %>% 
    filter(
      !(is.na(mean_landscape)),
      !(is.na(sd_landscape)), 
      !(is.nan(mean_landscape))
    )
}


emotion_function = function(data, catvar) {
  data %>%   
    group_by({{catvar}}) %>%
    summarise(across(any_of (feelings),
                     ~ mean(.x, na.rm = TRUE),
                     .names = "mean_{.col}"))
}

mean_function = function(data, catvar) {
  data %>% 
    mutate(mean_emotion = rowMeans(across(c(starts_with("mean_"))))) %>% 
    select({{catvar}}, mean_emotion) %>% 
    arrange(mean_emotion)
}

# Research Stage - geom_point plot functions 
## prepare data
scatter_long <- function(data) {
  data %>% 
    pivot_longer(
      cols = starts_with("mean_"),
      names_to = "emotion",
      values_to = "mean_score"
    ) %>% 
    mutate(emotion = gsub("mean_", "", emotion))
  }

scatterplot <- function(data, palette_type) {
  ggplot(data, aes(x = research_stage, y = mean_score, colour = emotion)) +
    geom_point(size = 3) +
    labs(
      title = "Mean Emotion Scores by Research Stage",
      x = "Emotion",
      y = "Mean Score",
      color = "Research Stage"
    ) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.key.size = unit(0.4, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9)) +
    scale_y_continuous(limits = c(1, 7)) +
    scale_color_brewer(palette = palette_type)
  }

# Post project stage 
## pivot_longer, prepare df for heatmaps 
longer_heatmap = function(df) {
  df %>% 
    pivot_longer(
      cols = interest:anger,
      names_to = "emotion", 
      values_to = "intensity", 
      values_transform = list(intensity = as.numeric
      )) %>% 
    select(
      ResponseId, emotion, intensity) %>% 
    mutate(emotion = factor(emotion, 
                            levels = unique(emotion)))
}
