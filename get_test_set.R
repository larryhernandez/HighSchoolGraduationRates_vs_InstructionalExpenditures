get_test_set<-function(cleaned_dataframe,indices_for_training){
  all_indices = 1:nrow(cleaned_dataframe)
  indices_for_test_set = all_indices[-indices_for_training]
  return(cleaned_dataframe[indices_for_test_set,])
}