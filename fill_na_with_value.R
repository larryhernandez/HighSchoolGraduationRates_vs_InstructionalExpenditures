fill_na_with_value<-function(data_frame,colname,value){

  data_frame[c(colname)][is.na(data_frame[c(colname)])] = value
  return(data_frame)
}