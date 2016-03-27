process_rate<-function(rate_as_factor){
  
  rate = unfactor(rate_as_factor)
  if(class(rate) == "numeric"){
    return(rate)
  } else if (class(rate) == "character"){
      if(is.letter(rate)){
        return(process_special_rate_codes(rate))
      }else{
        return(median_of_numerical_range_represented_as_char(rate))
      }
  } else {
      return(-1)
  }
}