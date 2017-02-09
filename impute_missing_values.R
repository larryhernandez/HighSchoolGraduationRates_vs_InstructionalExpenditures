impute_missing_values<-function(data_frame, colname, method = "median", round_to_whole = FALSE)
{
  #Determine class types for each column of data_frame
  #data_types = sapply(data_frame,class)
  #num_cols = ncol(data_frame)

  #datatype = class(data_frame[,colname])
  #print(datatype)
  #if (datatype != "integer" || datatype != "numeric" || datatype != "double") {
  #  stop("Values corresponding to 'colname' must be integer, numeric, or double!")
  #  geterrmessage()
  #}
  if (method == "mean"){
    value = mean(data_frame[,colname],na.rm=TRUE)
  }else{
    value = median(data_frame[,colname],na.rm=TRUE)
  }
    
  if (round_to_whole == TRUE){
    value = round(value)
  }
  data_frame = fill_na_with_value(data_frame,colname,value)
  return(data_frame)
}