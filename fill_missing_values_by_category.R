fill_missing_values_by_category<-function(data_frame, category, col_to_impute, alt_fill = "None", method = "median", round_to_whole = FALSE){
  #
  # 
  # category      column name (of data_frame) by which the data can be categorized / grouped (i.e. state)
  # col_to_impute column name of missing or NA values to impute
  # alt_fill      string storing name of dataframe column with which to first impute values

  # Strategy: First, fill missing values with value from alt_fill, if available. For values which are still missing,
  # then impute using median value from 'col_to_impute'.
  
  records = which(is.na(data_frame[,col_to_impute]))
  if (length(records) == 0){
    return(data_frame)
  }
  
  if (alt_fill != "None"){
    print("About to fill with alternate data")
    data_frame[records,col_to_impute] = data_frame[records,alt_fill]
    records = which(is.na(data_frame[,col_to_impute]))
    if (length(records) == 0){
      return(data_frame)
    }
  }
  
  unique_categories = sort(unique(data_frame[records,category]))
  
  #if (sub_category != "None"){
  #  unique_subcategories = sort(unique(data_frame[,sub_category]))      
  #}  
  

  # Loop through each category that has missing values, and impute with the median value of all values in that category 
  for (categ_value in unique_categories){
    indices = which(data_frame[records, category] == categ_value)
    records_iter = records[indices]
    data_frame[data_frame[,category] == categ_value,] = impute_missing_values(data_frame[data_frame[,category] == categ_value,], col_to_impute, method, round_to_whole)
  }
  return(data_frame)
}