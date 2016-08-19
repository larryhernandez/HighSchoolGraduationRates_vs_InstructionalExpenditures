fill_with_median<-function(unique_id, name_unique_id, name_of_state_fips_col, col_to_update, data_frame, median_values, median_vals_unique_id, median_val_column_name){
  # Uses the median_values data_frame to fill the missing record, identified by 'id_of_blank_record', by looking at 'column_to_use',
  # the name of the column within median_values
  #
  
  # For the record of interest, extract the state fips number from the data_frame
  index_of_record = which(data_frame[,name_unique_id] == unique_id)
  fips_state_id_of_record = data_frame[index_of_record,name_of_state_fips_col]
  value_to_update = as.numeric(unlist(median_values[[fips_state_id_of_record,median_val_column_name]]))
  data_frame[index_of_record,col_to_update] = value_to_update
  
  #global assignment of updated dataframe
  assign("ed_data",data_frame, pos = .GlobalEnv, envir = .GlobalEnv)
  return(1)
}