#Functions for making a linear model generatively

#import libraries
library(tidyverse)

#quick function to assess quality of dataframe
negativeNumberMapping <- function(x){
  return(floor(sqrt(sign(x)+1)))
}

#remove columns where >15% of values are nan
removeBadColumns <-function(df){
  #turn all negative numbers into nan values
  bool_df <- map_df(df, .f=negativeNumberMapping)
  
  #get length of dataframe for finding % good data entries
  length_df <- dim(df)[2]
  lockBinding("length_df", environment())
  
  #create empty vector of columns to remove
  cols_to_remove <- c()
  
  #get proportion good data values. If proportion too low, add to list to be removed
  for(i in 1:dim(df)[2]){
    num_useful <- sum(bool_df[i])
    prop_numeric <- num_useful/length_df
    if(prop_numeric < 0.85){
      cols_to_remove <- append(cols_to_remove, i)
    }
  }
  
  #remove low quality columns from dataframe
  df <- df[-cols_to_remove]
  
  return(df)
}


#replace all negative numbers with nan values
#this function takes a while unfortunately :(
setNanValues <- function(df){
  for(i in 1:dim(df)[1]){
    for(j in 1:dim(df)[2]){
      if(df[i,j] < 0){
        df[i,j] = NA
      }
    }
  }
  return(df)
}


#initialise analysis
initialiseSearch <- function(df){
  
  #make global vector for storing evaluations
  evaluations <<- c()
  
  #make global log for model construction process
  sink("output.txt")
  cat("Log Initialised")
  cat("\n")
  sink()
  
  #initialise iteration number
  iternum <<- 1
  
  #select data we want to work on
  countries <- c(15,16,23,25)
  df <- df %>%
    filter(country %in% countries) %>%
    filter(wave == 6) %>%
    select(-c("mergeid", "hhid", "coupleid"))
  
  #remove low quality columns
  df <- removeBadColumns(df)
  
  #flag bad datapoints as NAc
  df <- setNanValues(df)

  #initialise df_all and df_model
  df_all <<- data.frame(df)
  df_model <<- data.frame(df) %>%
    select(bmi)
}

#make logging function
logger <- function(string){
  sink("output.txt", append=TRUE)
  cat("\n")
  cat(string)
  sink()
}


findMaxCorr <- function(df_all, df_model, iternum){
  #find maximum correlation against bmi in df_all (excluding bmi against itself)
  
  #log status
  logger(paste("Started iteration: ", iternum))
  
  #get column names and make constant in current scope
  column_names <- colnames(df_all)
  lockBinding("column_names", environment())
  
  #initialize empty vector for storing correlations
  correlations <- c()
  
  #populate correlations vector
  for(i in 1:dim(df_all)[2]){
    correlation <- cor(df_all$bmi, df_all[[i]], use="complete.obs")
    correlations <- append(correlations, correlation)
  }
  
  #sort correlations vector to get largest values
  sorted_correlations <- sort(abs(correlations), decreasing=TRUE, na.rm=TRUE, index.return=TRUE)
  
  #log best (3) correlated variables in dataframe
  best_3_cors_column_names <- column_names[sorted_correlations$ix[2:4]]
  best_3_cors_values <- sorted_correlations$x[2:4]
  logger(paste("Best columns: ", best_3_cors_column_names))
  logger(paste("With absolute correlation values: ", best_3_cors_values))
  
  sorted_correlations$colnames <- column_names[sorted_correlations$ix]
  
  return(sorted_correlations)
}


switchColumnToModel <- function(df_all, df_model, column_to_switch){
  #switches column from df_all to df_model
  #note no direct return type, only modifies objects in the global scope
  df_model[[column_to_switch]] <<- df_all[[column_to_switch]]
  df_all <<- df_all %>%
    select(-c(column_to_switch))
}


replaceNanVals <- function(df_model){
  #replaces the nan values in a column with the mean of the column
  df_model <<- df_model %>%
    mutate_all(~ifelse(is.na(.x), mean(.xm na.rm = TRUE), .x))
}

removeNanVals <- function(df_model){
  #removes all nan values from dataframe
  df_model <<- df_model %>%
    drop_na()
}

makeModel <- function(df_model, iternum, nan_method = "remove"){
  #remove all na from dataframe or replace with mean?
  if(nan_method == "remove"){
    removeNanVals()
  }else{
    replaceNanVals()
  }
  
  #construct linear model of bmi against rest of dataframe
  #return linear model?
}


evaluateModel <- function(df_model, iternum, kwargs=None){
  #call make model
  #evaluate model (plots, stats)
  #store numerical evaluation somewhere
  #return selection of evaluations
}


quantifyImprovements <- function(iternum){
  #check we're not on iteration 1
  #return some measure of how much the model n has
  #improved from model n-1
  #add 1 to iteration number
}
