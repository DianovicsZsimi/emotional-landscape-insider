library(tidyverse)
library(dplyr)
library(tidyverse)
library(tidyr)
library(magrittr)

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
  ggplot(df, 
         aes(x = emotion, 
             y = ResponseId, 
             fill = intensity)) +
    geom_tile(
      inherit.aes = T,
      position = position_identity(),
      height = 5, na.rm = T) +
    scale_fill_gradient(
      low = "white", 
      high = "#2b4a78", 
      limits = c(0, 7)) + 
    labs(
      title = "Intensity of emotions on the level of participants", 
      tag = "1.") +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
      axis.text.y = element_blank()
    )
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
      fill = "#76ad6f", na.rm = T
    ) +
    labs(
      title = "Overall intensity of emotions", 
      tag = "2.") +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}

### 3. Functions for the the descriptive statistics of people with neg_avg > pos_avg
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