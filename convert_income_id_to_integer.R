convert_income_id_to_integer <-function(my_factor, desired_int_length, num_offset){
  
  my_string = unfactor(my_factor)
  char_length = nchar(my_string)

  if(char_length == desired_int_length){
    output_int = as.integer(my_string)
    return(output_int)
  }
  
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
    output_int = as.integer(-1)
  }
  
  return(output_int)
}