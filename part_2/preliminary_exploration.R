#import data
load("data/easySHARE_rel8_0_0.rda")
df <- easySHARE_rel8_0_0
rm(easySHARE_rel8_0_0)

#look at size of data from each country per wave
table(df$country, df$wave)

# choose wave 4, country 35 (estonia), 6863 observations
df <- df %>%
  filter(country == 35) %>%
  filter(wave == 4)

#add na values to df
bool_errors_df <- df
bool_errors_df[bool_errors_df<0]<-NA
