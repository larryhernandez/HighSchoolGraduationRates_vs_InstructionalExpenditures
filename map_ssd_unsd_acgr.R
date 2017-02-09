map_ssd_unsd_acgr<-function(shape_secondary,shape_unified,ed_data_to_plot, final_model, title_content){
  # This function maps the secondary and unified school districts along with their measured acgr data, 
  # and predicted acgr data using the provided linear model ('final_model')
  #
  # INPUTS:
  #   shape_secondary       R shape file of the secondary school districts
  #   shape_unified         R shape file of unified school districts
  #   ed_data_to_plot       the education dataframe containing the acgr values of interest
  #   final_model           lm model of the data
  #   title_content         Descriptor string variable which gets inserted into various titles. Typically holds
  #                         values such as state names (i.e. "CA", "WI", "AZ", etc)
  # OUTPUTS:
  #   Three spatial maps of (1) the measured acgr values, (2) predicted acgr values, and (3) the residuals
    
  sh_secondary_with_acgr <- merge_data_with_shape(shape_secondary,ed_data_to_plot,"GEOID","leaid12")
  sh_unified_with_acgr   <- merge_data_with_shape(shape_unified, ed_data_to_plot,"GEOID","leaid12")

  # Map outcome of interest (i.e. acgr)
  x11()
  acgr_title = paste("Measured Adj. Cohort Grad. Rates in ", title_content)
  map_shape_file(sh_secondary_with_acgr, ed_data_to_plot$acgr, ed_data_to_plot$leaid12, acgr_title, add_plot = FALSE)
  map_shape_file(sh_unified_with_acgr,   ed_data_to_plot$acgr, ed_data_to_plot$leaid12, acgr_title, add_plot = TRUE)
 # dev.off()
  
  # Map predicted values of acgr
  x11()
  predicted_values = predict.lm(final_model,ed_data_to_plot)
  pred_title = paste("Predicted Adj. Cohort Grad. Rates in ", title_content)
  map_shape_file(sh_secondary_with_acgr, predicted_values, ed_data_to_plot$leaid12, pred_title, add_plot = FALSE)
  map_shape_file(sh_unified_with_acgr, predicted_values, ed_data_to_plot$leaid12, pred_title, add_plot = TRUE)
#  dev.off()

  # Map residuals
  x11()
  title = paste("Residuals for ", title_content)
  map_shape_file(sh_secondary_with_acgr, final_model$residuals, ed_data_to_plot$leaid12, title, add_plot = FALSE)
  map_shape_file(sh_unified_with_acgr, final_model$residuals, ed_data_to_plot$leaid12, title, add_plot = TRUE)
#  dev.off() 
}