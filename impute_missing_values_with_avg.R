impute_missing_values_with_avg<-function(data_frame, identifier, col2distinguish, col_to_update)
{
  # Determine the unique FIPS codes for the states
  #fips_values = sort(unique(data_frame[,col2distinguish]))

  # Acquire col2distinguish corresponding to the record_to_update
  index_of_record = which(data_frame[,identifier] == identifier)
  
  target_ = data_frame[index_of_record,col2distinguish]
  
  # The number of states in this data frame:
  num_states = length(fips_values)
  num_cols2distinguish = levels(data_frame[,col2distinguish])
  
  # Create temporary data-frame
  median_vals_by_state = data.frame(fips = numeric(0),levels[1])
  
  # For 'column_of_interest' and for each for each value in fips_values, 
  # calculate the median value of the 'col_to_update'

  
  # For each record that has missing values use the calculated median value and insert
  return(data_frame)
}