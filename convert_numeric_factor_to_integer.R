convert_numeric_factor_to_integer <-function(my_factor){
  character_string = unfactor(my_factor)
  if (is.element(substr(character_string,1,1), '=')){
     character_string = substr(character_string,2,nchar(character_string))
    }

  return(as.integer(character_string))
}