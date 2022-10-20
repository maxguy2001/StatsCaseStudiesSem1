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
  
  #remove wave and year columns
  df <- df %>%
    select(-c(wave, int_year))
  
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
  evaluations <<- c(0)
  
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
  
  rm(df)
  
}

#make logging function
logger <- function(string){
  sink("output.txt", append=TRUE)
  cat("\n")
  cat(string)
  sink()
}

#checks if 2 vectors can be correlated against each other
checkVectorsUseable <- function(vec1, vec2){
  nan_index_values <- c()
  
  for(i in 1:length(vec1)){
    if(is.na(vec1[i]) || is.na(vec2[i])){
      nan_index_values <- append(nan_index_values, i)
    }
  }
  
  if(length(nan_index_values) == 0){
    return(TRUE)
  }
  
  vec1 <- vec1[-c(nan_index_values)]
  vec2 <- vec2[-c(nan_index_values)]
  
  if(length(unique(vec1)) == 1 || length(unique(vec2)) == 1){
    return(FALSE)
  }else{
    return(TRUE)
  }
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
    if(checkVectorsUseable(df_all$bmi, df_all[[i]]) == TRUE){
      correlation <- cor(df_all$bmi, df_all[[i]], use="complete.obs")
      correlations <- append(correlations, correlation)
    }else{
      correlations <- append(correlations, NA)
    }
  }
  
  #sort correlations vector to get largest values
  sorted_correlations <- sort(abs(correlations), decreasing=TRUE, index.return=TRUE)
  
  #log best (3) correlated variables in dataframe
  best_3_cors_column_names <- column_names[sorted_correlations$ix[2:4]]
  best_3_cors_values <- sorted_correlations$x[2:4]
  logger(paste("Best columns: ", best_3_cors_column_names))
  logger(paste("With absolute correlation values: ", best_3_cors_values))
  
  sorted_correlations$colnames <- column_names[sorted_correlations$ix]
  logger("found best correlations")
  return(sorted_correlations)
}

#prints the most correlated values and what column it is to assist manual decision making
printNBestCorrelations <- function(correlations, n){
  correlation_vals <- correlations$x
  correlations_indexes <- correlations$ix
  correlation_names <- correlations$colnames
  for(i in 1:n){
    print(paste(correlation_names[i], correlation_vals[i], correlations_indexes[i]))
  }
  logger(paste("Printed best ", n, " correlations"))
}


switchColumnToModel <- function(df_all, df_model, column_to_switch){
  #switches column from df_all to df_model
  #note no direct return type, only modifies objects in the global scope
  df_model[[column_to_switch]] <<- df_all[[column_to_switch]]
  df_all <<- df_all %>%
    select(-c(column_to_switch))
  logger(paste("Swiched column ", column_to_switch, " from df_all to df_model"))
}


replaceNanVals <- function(df){
  #replaces the nan values in a column with the mean of the column
  df <- df %>%
    mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  return(df)
}

removeNanVals <- function(df){
  #removes all nan values from dataframe
  df <- df %>%
    drop_na()
  return(df)
}

makeModel <- function(df_model, nan_method = "remove"){
  #remove all na from dataframe or replace with mean?
  if(nan_method == "remove"){
    df <- removeNanVals(df_model)
  }else{
    df <- replaceNanVals(df_model)
  }
  
  #construct linear model of bmi against rest of dataframe
  model <- lm(bmi~ ., data=df)
  
  #assign global
  model <<- model
  logger("Sucessfully made model")
  
  #no return statement necessary
}


evaluateModel <- function(){
  #model has already been made and is in global scope
  #therefore we only need to return summary and append eval to evals vector
  model_summary <- summary(model)
  r_squared <- model_summary$r.squared
  evaluations <<- append(evaluations, r_squared)
  
  logger("Evaluted model")
  return(model_summary)
}

#check how much the r squared has improved on this iteration of the model
quantifyImprovements <- function(){
  #improvement = r^2(n) - r^2(n-1)
  improvement <- evaluations[length(evaluations)] - evaluations[length(evaluations)-1]
  logger("Evaluated model improvement")
  logger(paste("Finished iteration", iternum))
  
  #add 1 to iteration number
  iternum <- iternum + 1

  return(improvement)
}
