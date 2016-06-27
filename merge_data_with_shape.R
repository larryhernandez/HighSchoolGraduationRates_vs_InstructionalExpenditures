merge_data_with_shape<-function(shape, ed_data_to_plot,by_shape_id, by_ed_data_id){
  shape@data$GEOID <- unlist(lapply(shape@data$GEOID,convert_numeric_factor_to_integer))
  shape_2plot      <- merge(shape, ed_data_to_plot, by.x = by_shape_id, by.y = by_ed_data_id)
  return(shape_2plot)
}