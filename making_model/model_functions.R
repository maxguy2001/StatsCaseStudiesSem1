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
  
  #log status
  logger(paste("Started iteration: ", iternum))
  
  #find maximum correlation against bmi in df_all (excluding bmi against itself)
  #move that column into df_model
  #remove that column from df_all
  #log information\ 
  #log best (3?) correlated variables in dataframe
  #return modified df_all, modified df_model
}

replaceNanVals <- function(df_model){
  #replaces the nan values in a column with the mean of the column
}

removeNanVals <- function(df_model){
  #removes all nan values from dataframe
}

makeModel <- function(df_model, iternum, nan_method = "remove"){
  #remove all na from dataframe or replace with mean?
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

idea <- function(df_all, df_model){
  column_names <- colnames(df_all)
  lockBinding("column_names", environment())
  
  correlations <- c()
  
  for(i in 1:dim(df_all)[2]){
    correlation <- cor(df_all$bmi, df_all[[i]], use="complete.obs")
    correlations <- append(correlations, correlation)
  }
  sorted_correlations <- sort(correlations, decreasing=TRUE, na.rm=TRUE, index.return=TRUE)
  top_three_cor_columns <- sorted_correlations$ix[2:4]
  #TODO: figure out a way to choose what column to add to model df.
  return(correlations)
}

