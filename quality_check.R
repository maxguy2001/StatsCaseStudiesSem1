#load data and libraries
library(tidyverse)

load("C:/Users/maxgu/Documents/stats_cs/StatsCaseStudiesSem1/data/easySHARE_rel8_0_0.rda")
df <- easySHARE_rel8_0_0

#select numeric columns
bool_df <- df %>% select_if(is.numeric)

bool_df[bool_df < 0] <-0
bool_df[bool_df >= 0] <-1

bool_df$wave <- df$wave
bool_df$country <- df$country


