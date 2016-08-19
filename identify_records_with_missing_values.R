identify_records_with_missing_values<-function(data_frame,name_unique_id,col_to_update){
  # Given a data_frame, the name of unique identifier for each record, and the name of the column to update, return
  # the value of the unique identifier
  #
  # INPUTS:
  #   data_frame      data.frame containing the data of interest. Has columns specified by name_unique_id and col_to_update
  #   name_unique_id  name of column containing unique identifier for each record
  #   col_to_update   name of column which contains NA values
  #
  # OUTPUTS:
  #   unique_id_values the values of the unique identifiers
  
  indices_of_missing = which(is.na(data_frame[,col_to_update]))
  unique_id_values = as.double(data_frame[indices_of_missing, name_unique_id])
  return(unique_id_values)
}