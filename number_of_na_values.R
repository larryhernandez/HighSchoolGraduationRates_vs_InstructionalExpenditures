number_of_na_values <-function(my_vector){
# DESCRIPTION: Counts the number of NA values stored in the input variable my_vector
# 
# EXAMPLE:     number_of_na_values([NA,11,NA,22,NA]) returns 3
#
# INPUTS:
#   my_factor             factor variable that needs to be converted to an integer
  
  # Use which() and is.na() functions to yield a vector containing the locations of the NA values within my_vector 
  na_vector = which(is.na(my_vector))
  
  # The length of na_vector is the number of NA values that are in my_vector
  return(length(na_vector))
}
