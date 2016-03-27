median_of_numerical_range_represented_as_char <-function(range_as_char){

  my_str_list = strsplit(range_as_char,'-')
  low_num = as.integer(my_str_list[[1]][1])
  high_num = as.integer(my_str_list[[1]][2])
  average = as.numeric((low_num + high_num)/2)
  return(average)
  
  }