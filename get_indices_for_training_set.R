get_indices_for_training_set<-function(cleaned_dataframe, percentage){
  
  if (percentage >= 1 || percentage <= 0){
    stop('Value of input argument percentage must reside in (0,1). Typical value is 0.10')
  }else{
    n_training  = floor(percentage*nrow(cleaned_dataframe))
    all_indices = 1:nrow(cleaned_dataframe)
    indices_for_training = sample(all_indices,n_training,replace=FALSE)
    return(indices_for_training)
  }
}