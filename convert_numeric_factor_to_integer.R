convert_numeric_factor_to_integer <-function(my_factor){
# DESCRIPTION: Converts the factor variable my_factor, which appears to be an integer, into an integer type variable
#              Also, this function removes a leading = sign.
#              Note: to be used on factor variables that contain 
# EXAMPLE:     convert_numeric_factor_to_integer('12345') returns the integer 12345
#
# INPUTS:
#   my_factor             factor variable posing as an integer

  # Convert my_factor into a string called character_string
  character_string = unfactor(my_factor)
  
  # If the first character is an "=" sign, remove it.  
  if (is.element(substr(character_string,1,1), '=')){
     character_string = substr(character_string,2,nchar(character_string))
    }

  # Convert character_string into an integer and return its value
  return(as.integer(character_string))
}
