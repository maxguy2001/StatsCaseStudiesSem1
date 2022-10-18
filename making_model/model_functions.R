#Functions for making a linear model generatively

initialise <- function(df){
  #make global structures for storing evaluations
  #make global log for model construction process
  #initialise iteration number
  #initialise df_all and df_model
}


findMaxCorr <- function(df_all, df_model, iternum){
  #find maximum correlation against bmi in df_all (excluding bmi against itself)
  #move that column into df_model
  #remove that column from df_all
  #log information\ 
  #log best (3?) correlated variables in dataframe
  #return modified df_all, modified df_model
}


makeModel <- function(df_model, internum){
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
  #return some measure of how much the model n has
  #improved from model n-1
}

