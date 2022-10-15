#load data and libraries
library(tidyverse)

load("C:/Users/maxgu/Documents/stats_cs/StatsCaseStudiesSem1/data/easySHARE_rel8_0_0.rda")
df <- easySHARE_rel8_0_0

#select numeric columns
bool_df <- df %>% select_if(is.numeric)

functional_approach <- function(x){
  return(floor(sqrt(sign(x)+1)))
}

bool_df <- map_df(bool_df, .f=functional_approach)


bool_df$wave <- df$wave
bool_df$country <- df$country

selected_waves <- c(6,7)
selected_countries <- c(15,16,23,25)

score_combo <- function(dataframe, wavenum, countrynum){
  df_total <- dataframe %>% 
    filter(wave == wavenum) %>%
    filter(country == countrynum) %>%
    select(-c(wave, country)) %>%
    mutate(sum = rowSums(.)) 
  
  total <- sum(df_total$sum)
  
  possible_total <- dim(df_total)[1] * dim(df_total)[2]
  
  quality <<- total/possible_total

  return(quality)
}


for(i in selected_waves){
  for(j in selected_countries){
    quality <- score_combo(bool_df, i, j)
    print(i)
    print(j)
    print(quality)
    print(" ")
  }
}


