median_of_numerical_range_represented_as_char <-function(range_as_char){
# DESCRIPTION:  Accepts a character string which represents a numerical range (i.e. '50-60') and returns the average of the endpoints.
#               Assumes the endpoints of the range are integers.
#
# EXAMPLE:     median_of_numerical_range_represented_as_char('50-60') returns the numeric value 55.0
#
# INPUTS:
#   range_as_char         character variable that represents a range of values (i.e. 50-60)
# 
# OUTPUT:
#   median_value          the median (or here, average) value of the two endpoints (i.e. 55)
    
  # Remove the hyphen (-) character from the input variable range_as_char. The output is a list
  my_str_list = strsplit(range_as_char,'-')
  
  # Identify the two values in my_str_list (i.e. 50 and 60 in this running example)
  low_num = as.integer(my_str_list[[1]][1])
  high_num = as.integer(my_str_list[[1]][2])

  # The median of two numbers is simply their average, so calculate it and return that value
  median_value = as.numeric((low_num + high_num)/2) # runif(n=1,min=low_num,max=high_num) #
  
  return(median_value)
  }