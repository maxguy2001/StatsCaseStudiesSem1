#load libraries
library(tidyverse)
library(gridExtra)

#load data
load("data/easySHARE_rel8_0_0.rda")
dat<-easySHARE_rel8_0_0

#make predictor column
make_predictor <- function(dat){
  cogvars <- c("recall_1", "recall_2", "orienti", "numeracy_1", "numeracy_2")
  cog = dat[cogvars]
  numeracy = rep(NA,dim(cog)[1])
  numeracy[cog$numeracy_1 >= 0 & cog$numeracy_2 >= 0] = (cog$numeracy_1[cog$numeracy_1 >= 0 & cog$numeracy_2 >= 0] + cog$numeracy_2[cog$numeracy_1 >= 0 & cog$numeracy_2 >= 0] )/2
  numeracy[cog$numeracy_1 >= 0 & cog$numeracy_2 < 0] = cog$numeracy_1[cog$numeracy_1 >= 0 & cog$numeracy_2 < 0]
  numeracy[cog$numeracy_1 < 0 & cog$numeracy_2 >= 0] = cog$numeracy_2[cog$numeracy_1 < 0 & cog$numeracy_2 >= 0]
  cog$numeracy = numeracy
  cogscore = rep(NA,dim(cog)[1])
  ind = cog$recall_1 >= 0 & cog$recall_2 >= 0 & cog$orienti >= 0 & !is.na(numeracy)
  cogscore[ind] = cog$recall_1[ind] + cog$recall_2[ind] +  cog$orienti[ind] + cog$numeracy[ind]
  cog$cogscore = cogscore
  dat$cogscore<-cog$cogscore
  return(dat)
}

#function to remove columns
filter_dat <- function(dat){
  #filter wave 4, country 35
  dat <- dat %>%
    filter(wave == 4) %>%
    filter(country == 35)
  
  #remove uneccessary columns
  dat <- dat %>%
    select(-c("mergeid", "hhid", "wave", "wavepart", "int_version", 
              "int_year", "int_month", "country", "country_mod", 
              "language", "birth_country", "citizenship", "mar_stat", 
              "partnerinhh", "ch001_", "sp002_mod", "adlwa", "iadla", 
              "bmi2", "co007_", "thinc_m","dn002_mod", "dn003_mod", 
              "dn004_mod", "euro1", "euro2", "euro3", "euro4", 
              "euro6", "euro7", "euro8", "euro9", "euro10", "euro11", 
              "euro12", "coupleid", "iv009_mod",
              "books_age10", "maths_age10", "language_age10", 
              "vaccinated", "childhood_health", "q34_re",
              "ch021_mod", "ch007_hh", "maxgrip", "int_partner", 
              "age_partner", "gender_partner", "mother_alive", 
              "father_alive", "siblings_alive", "sp003_1_mod", 
              "sp003_2_mod",  "sp009_3_mod", "sp003_3_mod", 
              "sp009_1_mod", "sp009_2_mod", "sp008_", "bfi10_extra_mod", 
              "bfi10_agree_mod", "bfi10_consc_mod", "bfi10_neuro_mod", 
              "bfi10_open_mod", "ep005_", "ep009_mod", "ep011_mod", 
              "ep013_mod", "ep026_mod", "ep036_mod", "income_pct_w1",
              "income_pct_w2", "income_pct_w5", "income_pct_w6",
              "income_pct_w7","income_pct_w8"))
    return(dat)
}


#define function to combine columns
combine_columns <- function(dat, cols_to_combine, method="mean", weights=NA){
  
  #select columns
  subset <- dat(select(cols_to_combine))
  
  if(method == "mean"){
    return(rowMeans(subset))
  }
  else if(method == "weighted"){
    for(i in 1:ncols(subset)){
      subset[i] <- subset[i]*weights[i]
      return(rowMeans(subset))
    }
  }
  
}



#column combination