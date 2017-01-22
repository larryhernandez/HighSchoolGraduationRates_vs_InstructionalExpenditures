linear_scale<-function(x, new_scale = 1){
  # Scales the input vector x so that its elements reside between 0 and new_scale
  # Assumes all elements of x are non-negative
  
  M = max(x)
  x = round(x / M * new_scale,2)
  return(x)
}