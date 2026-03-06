# Functions combined
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

## =======================================================================================================
# Functions Bogi
## =======================================================================================================

### 1. functions for the heatmaps
analysis_select = function(df) { 
  df %>% select(-Q22, -Q22_6_TEXT, -dismiss_1)
} ## basic function: excludes irrelevant columns

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

## heatmap plot function
heatmap_plot = function(df) {
  df %>% 
    ggplot(aes(x = emotion, y = ResponseId, fill = intensity)) +
    geom_tile(
      inherit.aes = T,
      position = position_identity(), na.rm = T) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
      axis.text.y = element_blank())
}

### 2. functions for the mean_intensity plots
sum_analysis = function(df) {
  df %>% 
    group_by(emotion) %>% 
    summarise(mean_intensity = mean(intensity, na.rm = T))
}

## plot overall emotions
plot_overall = function(df) {
  df %>% 
    ggplot(aes(x = emotion, 
               y = mean_intensity)) +
    scale_y_continuous(limits = c(0, 7)) +
    geom_col(
      fill = color_vector[stage], na.rm = T
    ) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}

### 3. Functions for the the descriptive statistics of people with neg_avg > pos_avg, and pos_avg > neg_avg
## prepares df for piecharts
categories_prepare = function(df) {
  df %>% 
    filter(
      !is.na(category)
    ) %>% 
    select(ResponseId, category) %>% 
    group_by(category) %>% 
    summarise(n = n()) %>% ## counts each category and puts it into n columns
    mutate(
      perc = n / sum(n) * 100
    ) ## percentage of each position 
}

## piechart plot
piechart_plot = function(df) {
  ggplot(df, aes(x = "", y = n, fill = category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(perc), "%")), 
              position = position_stack(vjust = 0.5), 
              size = 3.5) +
    scale_fill_manual(values = c("#ff6600", "#ffcc00", "#99cc00", "#009933", "#66ccff", "#6699ff", "#9966ff", "#ff66cc", "#ffccff"))
}

## classifying the columns
pos_neg_classify = function(df) {
  df %>% 
    mutate(
      age = as.numeric(age), 
      gender = as.factor(gender)
    ) %>% 
    filter(!is.na(age), !is.na(gender)) 
}

## creating age groups for the age pyramid plot 
age_grouping = function(df) {
  df %>% 
    select(
      ResponseId, age, gender
    ) %>% 
    filter(!(gender %in% c("Non-binary / third gender / gender non-conforming", NA, "Prefer not to say"))) %>% 
    mutate(
      age_group = 
        case_when(
          (age >= 20 & age <= 30) ~ "20-30", 
          (age >= 31 & age <= 40) ~ "31-40", 
          (age >= 41 & age <= 50) ~ "41-50", 
          (age >= 51 & age <= 60) ~ "51-60", 
          (age >= 61 & age <= 70) ~ "61-70", 
          (age >= 71 & age <= 80) ~ "71-80", 
          (age >= 81 & age <= 90) ~ "81-90", 
          (age >= 91 & age <= 100) ~ "91-100"
        ), ## creating age groups, then converting the age_group variable to factor, so that the age_pyramid can be made
      age_group = factor(
        age_group,
        levels = c(
          "20-30","31-40","41-50","51-60",
          "61-70","71-80","81-90","91-100")
      )
    )
}

## age pyramid plot
pyramid_plot = function(df) {
  df %>% 
    filter(!(gender %in% c("Non-binary / third gender / gender non-conforming", NA, "Prefer not to say"))) %>% 
    age_pyramid(age_group = age_group, split_by = gender) +
    labs(x = "Age group", 
         y = "Number of people", 
         title = "Age distribution chart")
}

# 4. -- ggsave function -- 
ggsave_fn = function(filename, plot = NULL, width = 10, height = 7, dpi = 300, dir) {
  
  ## Create directory if it doesn't exist
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = T)
  } 

  filepath <- file.path(dir, paste0(filename, ".png")) 
  
    ggsave(
      filename = filepath,
      plot = if(is.null(plot)) ggplot2::get_last_plot() else plot,  ## if plot argument is NULL, then get_last_plot()
      width = width, 
      height = height, 
      dpi = dpi,
      create.dir = T, 
      device = "png", 
    ) 
  message("Saved as .png:", filename, ".png")
}

## =======================================================================================================
# Functions Sanyi
## =======================================================================================================

# Functions and setup
overall_emotion_plot_valence <- function(data, group_var, mean_var, color_low = "orange", color_high = "red",
                                         title_text = "Overall Emotional Landscape") {
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
      axis.text.y   = element_text(size = 11, angle = 45, vjust = -1),
      plot.title    = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none")
}



emotion_function = function(data, catvar) {
  data %>%   
    group_by({{catvar}}) %>%
    summarise(across(any_of (feeling_cols),
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
      axis.text.y = element_text(size = 11),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none"
    )
}

# NA counter function
Nacount = function(data, catvar){ 
  data %>% 
    group_by({{catvar}}) %>% 
    summarise(NAcountneg = sum(NAcountneg), NAcountpos = sum(NAcountpos))
}

## proportion of NA values in positive or negative emotions - preparation of data and visualizing with ggplot
na_prop_diverging_plot = function(data,
                                  group_var,
                                  title_text = "Proportion of Negative vs Positive NA by Group") {
  df_group <- data %>%
    group_by({{group_var}}) %>%
    summarise(
      NApos = sum(NAcountpos, na.rm = TRUE),
      NAneg = sum(NAcountneg, na.rm = TRUE)
    ) %>%
    mutate(
      total_NA = NApos + NAneg,
      pos_prop = NApos / total_NA,
      neg_prop = NAneg / total_NA,
      balance  = pos_prop - neg_prop
    )
  
  df_group[[deparse(substitute(group_var))]] <- 
    reorder(df_group[[deparse(substitute(group_var))]], df_group$balance)
  
  ggplot(df_group, aes(y = {{group_var}})) +
    geom_col(aes(x = -neg_prop), fill = "steelblue", width = 0.6) +
    geom_col(aes(x =  pos_prop), fill = "firebrick", width = 0.6) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(
      limits = c(-1, 1),
      breaks = seq(-1, 1, by = 0.25),
      labels = paste0(abs(seq(-1, 1, by = 0.25))* 100, "%")
    ) +
    labs(title = title_text,
         x = "Proportion within category",
         y = "") +
    theme_minimal()
}

# Countries

## continents (?) 
continent_plot_function = function(data, title_text){
  data %>% 
    mutate(dev = mean_emotion - 4) %>% 
    arrange(mean_emotion) %>% 
    ggplot(aes(y = reorder(country, mean_emotion), x = dev,fill = mean_emotion )) +
    geom_col(width = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(-3, 3), breaks = -3:3, labels = 1:7)+
    scale_fill_gradient(low = "orange", high = "red") +
    theme_tufte() +
    labs(title = {{title_text}}, x = "", y = "") +
    theme(axis.text.y = element_text(size = 11),
          plot.title  = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none")
  
}

## valence by continent 
continent_plot_function_valence = function(data, color_low = "orange",
                                           color_high = "red", title_text) {
  data %>% 
    arrange(mean_emotion) %>% 
    ggplot(aes(y = reorder(country, mean_emotion), x = mean_emotion,fill = mean_emotion )) +
    geom_col(width = 0.3) +
    scale_x_continuous(
      limits = c(0, 7),
      breaks = 0:7,
      labels = 0:7) +
    scale_fill_gradient(low = {{color_low}}, high = {{color_high}}) +
    theme_tufte() +
    labs(title = {{title_text}}, x = "", y = "") +
    theme(axis.text.y = element_text(size = 11),
          plot.title  = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none")
}

## mean emotional intensity by continenty
continent_emotions_mean_function <- function(data, continent_name) {
  data %>%
    filter(continent == continent_name) %>%
    mutate(across(interest:anger, ~ na_if(.x, 0))) %>% 
    group_by(country) %>%
    summarise(
      mean_emotion = mean(mean_feeling, na.rm = TRUE),
      .groups = "drop"
    )
}

## positive emotions by continent 
continent_emotions_mean_function_pos <- function(data, continent_name) {
  data %>%
    filter(continent == continent_name) %>%
    mutate(across(interest:compassion, ~ na_if(.x, 0))) %>% 
    group_by(country) %>%
    summarise(
      mean_emotion = mean(mean_feeling, na.rm = TRUE),
      .groups = "drop"
    )
}

## negative emotions by continent
continent_emotions_mean_function_neg <- function(data, continent_name) {
  data %>%
    filter(continent == continent_name) %>%
    mutate(across(sadness:anger, ~ na_if(.x, 0))) %>% 
    group_by(country) %>%
    summarise(
      mean_emotion = mean(mean_feeling, na.rm = TRUE),
      .groups = "drop"
    )
}

#Categorical Distribution Plot Function
plot_distribution_categorical <- function(data,
                                          variable_name,
                                          title,
                                          custom_levels = NULL,
                                          remove_parentheses = FALSE,
                                          angle = 30) {
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
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3)))
  
  if (remove_parentheses) {
    p <- p + scale_x_discrete(
      labels = function(x) sub("\\s*\\(.*\\)", "", x)
    )
  }
  
  return(p)
}