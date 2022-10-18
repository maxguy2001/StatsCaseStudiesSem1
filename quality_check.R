#load data and libraries
library(tidyverse)

load("data/easySHARE_rel8_0_0.rda")
df <- easySHARE_rel8_0_0

#select numeric columns
bool_df <- df %>% select_if(is.numeric)

#maps negative numbers to 0 and all other numbers to 1
functional_approach <- function(x){
  return(floor(sqrt(sign(x)+1)))
}

#make df of booleans
bool_df <- map_df(bool_df, .f=functional_approach)

#replace necessary information columns
bool_df$wave <- df$wave
bool_df$country <- df$country

selected_waves <- c(6,7)
selected_countries <- c(15,16,23,25)

#get number of missing values in each wave-country combo
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

#get proportion of data present in each wave-country combination
for(i in selected_waves){
  for(j in selected_countries){
    quality <- score_combo(bool_df, i, j)
    print(i)
    print(j)
    print(quality)
    print(" ")
  }
}

#make smaller dataframe on subset we want to analyse
small_bool <- bool_df %>%
  filter(wave %in% selected_waves) %>%
  filter(country %in% selected_countries)


#make function to give info on how useful a wave is
score_wave <- function(dataframe, wave_num){
  temp_df <- dataframe %>%
    filter(wave == wave_num) 
  
  length = dim(temp_df)[1]
  totals <- c()
  colnames <- colnames(temp_df)
  cols <- c()
  for(i in 1:ncol(temp_df)){
    total <- sum(temp_df[i])
    print(total)
    totals <- append(totals, total)
  }
  props <- totals/length
  
  viable_columns <- c()
  for(i in 1:length(props)){
    viable <- 0
    if(props[i] > 0.85){
      viable <- 1
    }
    viable_columns <- append(viable_columns, viable)
  }
  df <- data.frame(colnames, props, viable_columns)
  return(df)
}


#make function to give info on how useful a country is
score_country <- function(dataframe, country_num){
  temp_df <- dataframe %>%
    filter(country == country_num) 
  
  length = dim(temp_df)[1]
  totals <- c()
  colnames <- colnames(temp_df)
  cols <- c()
  for(i in 1:ncol(temp_df)){
    total <- sum(temp_df[i])
    print(total)
    totals <- append(totals, total)
  }
  props <- totals/length
  
  viable_columns <- c()
  for(i in 1:length(props)){
    viable <- 0
    if(props[i] > 0.85){
      viable <- 1
    }
    viable_columns <- append(viable_columns, viable)
  }
  df <- data.frame(colnames, props, viable_columns)
  return(df)
}



wave_6 <- score_wave(small_bool, 6)
wave_7 <- score_wave(small_bool, 7)

country_15 <- score_country(small_bool, 15)
country_16 <- score_country(small_bool, 16)
country_23 <- score_country(small_bool, 23)
country_25 <- score_country(small_bool, 25)

w6_num_viable <- sum(wave_6$viable_columns)
w7_num_viable <- sum(wave_7$viable_columns)

c15_num_viable <- sum(country_15$viable_columns)
c16_num_viable <- sum(country_16$viable_columns)
c23_num_viable <- sum(country_23$viable_columns)
c25_num_viable <- sum(country_25$viable_columns)


print(paste(w6_num_viable, w7_num_viable))
print(paste(c15_num_viable, c16_num_viable, c23_num_viable, c25_num_viable))

#conclusion: use wave 6 only but use all 4 countries


#wave 6 investigation
small_bool <- small_bool %>%
  filter(wave ==6)

country_15 <- score_country(small_bool, 15)
country_16 <- score_country(small_bool, 16)
country_23 <- score_country(small_bool, 23)
country_25 <- score_country(small_bool, 25)

c15_num_viable <- sum(country_15$viable_columns)
c16_num_viable <- sum(country_16$viable_columns)
c23_num_viable <- sum(country_23$viable_columns)
c25_num_viable <- sum(country_25$viable_columns)

print(paste(c15_num_viable, c16_num_viable, c23_num_viable, c25_num_viable))

