merge_data_with_shape<-function(shape, ed_data_to_plot,by_shape_id, by_ed_data_id){
# 
# Description:        inserts data from 'ed_data_to_plot' into 'shape'. Utilizes 
#                     'merge' function
#
# Input:
#   shape:            spatial polygon dataframe
#   ed_data_to_plot   data that will be inserted into 'shape'
#   by_shape_id       name of column within 'shape' spatial polygon dataframe that contains
#                     unique record identifiers. Used for matching records within ed_data_to_plot
#   by_ed_data_id     name of column within 'ed_data_to_plot' that contains unique record identifier
#
# Output:
#   An updated version of 'shape' spatial polygon dataframe that now contains
#   data from ed_data_to_plot 

  shape@data[,by_shape_id] <- unlist(lapply(shape@data[,by_shape_id],convert_numeric_factor_to_integer))
  shape_2plot      <- merge(shape, ed_data_to_plot, by.x = by_shape_id, by.y = by_ed_data_id)
  return(shape_2plot)
}