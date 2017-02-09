remove_records<-function(data_frame,indices){
  
  #indices = which(is.na(ed_data[,column]))
  if(length(indices)>0){
    data_frame = data_frame[-indices,]  
  }
  
  return(data_frame)
}