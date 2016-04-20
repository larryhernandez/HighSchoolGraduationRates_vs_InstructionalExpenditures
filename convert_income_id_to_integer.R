convert_income_id_to_integer <-function(my_factor, desired_int_length, num_offset){
  # DESCRIPTION: Converts the factor variable my_factor into an integer of specified length given by desired_int_length. 
  #              Allows user to specificy that the first num_offset characters of my_factor are to be discarded
  #
  #              Note: This function was written to convert the school district ID values from the data set containing median income
  #                    data and is the reason for the odd name of this function.
  # 
  # EXAMPLE:     convert_income_id_to_integer('123459876',desired_int_length=4,num_offset=5) returns the integer 9876
  #
  # INPUTS:
  #   my_factor             factor variable that needs to be converted to an integer
  #   desired_int_length    integer representing the desired number of digits that represent the output value
  #   num_offset            integer representing the number of extraneous characters at the beginning of my_factor
  
  # First convert the factor into a character string called my_string
  my_string = unfactor(my_factor)
  char_length = nchar(my_string)

  # When my_string contains the correct number of "digits" and simply needs 
  # to be converted to an integer.
  if(char_length == desired_int_length){
      output_int = as.integer(my_string)
      return(output_int)
  }
  
  # The case when my_string contains too many digits which need to be stripped off.
  # Assumes extraneous digits are at the beginning of the string
    if(char_length >= num_offset + desired_int_length){
      start = num_offset + 1
      finish = start + desired_int_length - 1
      output_int1 = substr(my_string,start,finish)
      output_int = as.integer(output_int1)
  } else if( char_length < (num_offset + desired_int_length) & (char_length >= desired_int_length)){
      start = (char_length - standard_id_length) + 1
      finish = start + standard_id_length - 1
      output_int1 = substr(my_string,start,finish)
      output_int = as.integer(output_int1)
  } else {
    # A simple way to identify the occasions when the input variable 'my_factor' does not fit the 
    # assumptions of this user-defined function
      output_int = as.integer(-1)
  }
  
  return(output_int)
}
