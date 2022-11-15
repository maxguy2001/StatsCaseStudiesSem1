# script provides functionality to perform knn imputation on data
source("part_2/combining_columns.R")

df <- make_predictor(dat)
df <- filter_dat(df)

find_k_closest <- function(df_no_na, row_values, column_index_na, k){
  df <- df_no_na
  df <- df %>% select(-column_index_na)
  #subtract row_values which has had the na areas removed 
}

normalise_dataframe <- function(df){
  for(i in 1:ncol(df)){
    col <- df[i]
    df[i] <- col/max(col)
  }  
  return(df)
}

normalisation_values <- function(df){
  max_values <- c()
  for(i in 1:ncol(df)){
    max_values <- append(max_values, max(df[i]))
  }
  return(max_values)
}

impute_row <- function(df, row_index, normalisation_values, norm_df){
  normed_rowvec <- as.numeric(as.vector(df[i,]))/normalisation_values
  closeness_df <- abs(sweep(norm_df, axis=1, normed_rowvec, '-'))
  
  
}


impute_dataframe <- function(df, k){
  
  #get dataframe of all na and no na
  df_no_na <- df %>%
    drop_na()
 # df_all_na <- df[rowSums(is.na(df)) > 0,]
  
  #normalise no na dataframe
  norm_vals <- normalisation_values(df_no_na)
  norm_df <- normalise_dataframe(df_no_na)
  rm(df_no_na)
  
  #iterate through na rows and fill in missing data
  for(i in 1:nrows(df)){
    row <- df[i,]
    if(nrow(drop_na(df)) != nrow(df)){
      df <- impute_row(df, i, norm_vals, norm_df)
    }
  }
  
  
  

  #TODO
  #plan:
  
  #find every row with na's
  #find k closest rows to each na row (euclidean difference)
  #replace na with average
}
