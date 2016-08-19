determine_imputed_numeric_value<-function(state_fips_id, name_of_state_fips_column, data_frame,  col_of_interest){
  # For the given numerical state_fips, determine the median value of "White"
  state_indices = which(data_frame[,name_of_state_fips_column] == state_fips_id)
  state_records = data_frame[state_indices,]
  value_for_imputing = median(state_records[,col_of_interest], na.rm=TRUE)
  return(value_for_imputing)
}