number_of_na_values <-function(my_vector){
  na_vector = which(is.na(my_vector))
  return(length(na_vector))
}