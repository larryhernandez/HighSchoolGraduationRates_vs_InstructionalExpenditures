process_rate<-function(rate_as_factor){
# DESCRIPTION: Converts the factor variable rate_as_factor into a numeric variable. 
#              Note: This function calls two other user-defined functions to do a lot of the processing:
#                   (1) process_special_rate_codes
#                   (2) median_of_numerical_range_represented_as_char
#  
#             Assumes that rate_as_factor can be classified as a variable of class 'numeric' or 'character' which appears to be a 
#             legitimate graduation rate or specialized code for a graduation rate
#
# EXAMPLES:     process_rate(rate_as_factor = '50-60') returns 55
#               process_rate(rate_as_factor = 'LE20') returns 10
#               process_rate(rate_as_factor = '6.2') returns 6.2
#               process_rate(rate_as_factor = 'xyZ') returns -1
#
# INPUTS:
#   rate_as_factor    factor variable that needs to be converted to an integer
  
  # unfactor() converts rate_as_fator into numeric or character variable and stores result in rate
  rate = unfactor(rate_as_factor)
  
  # If rate is numeric, return it immediately. Otherwise continue processing using other functions
  if(class(rate) == "numeric"){
    return(rate)
  } else if (class(rate) == "character"){
      # If the rate contains letters, process it using the special 'process_special_rate_codes' function
      if(is.letter(rate)){
        return(process_special_rate_codes(rate))
      }else{
      # If the rate character variable is a range of value (i.e. '50-60')
        return(median_of_numerical_range_represented_as_char(rate))
      }
  } else {
    # When rate is neither a numeric nor a character variable identify it by returning the value -1
      return(NA)
  }
}