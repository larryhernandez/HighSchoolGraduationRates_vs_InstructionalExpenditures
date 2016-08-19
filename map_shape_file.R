map_shape_file<-function(shape_2plot,values,values_id,my_title="",add_plot=FALSE)
{
  # Description:  Takes a shape files and creates visual plot on a map
  # Requires use of the following libraries: sp, maptools, RColorBrewer,classInt
  # This code is largely borrowed from the mapIt() function listed on Karl Rohe's website
  # and has been adapted for use with shapefiles:
  #     http://pages.stat.wisc.edu/~karlrohe/lr333/mapIt.R
  # INPUTS:
  #   shape_2plot     An R-shape / polygon that will be filled with 'values'
  #   values          (numeric) data that will be represented / mapped by the shapefile
  #   values_id       the unique identifier that will be used to pair 'values' with 'shape_2plot'
  #
  # OUTPUTS: spatial map of 'values'

  nclr<-10 # number of bins (i.e. breaks)
  breaks <-quantile(values, seq(0, 1, len=nclr+1))
  #breaks = rep(0,length.out=nclr+1)
  #for (i in seq(1,length(breaks)-1)){
  #  breaks[i+1] = 10*(i)
  #}
  
  # the next line is fun to play with. See http://colorbrewer2.org
  plotclr <- brewer.pal(nclr, "RdYlGn")  # ?brewer.pal  # this is a diverging pal
  indices_2map = match(as.numeric(shape_2plot@data$GEOID),values_id)
  plotvar <- values[indices_2map]
  #plotvar <- na.omit(plotvar)
    
  #print(paste("The length of the plot variable is ", length(plotvar),"."))

  class <- classIntervals(var = plotvar, n = nclr, style = "fixed",fixedBreaks = breaks)
  colcode <- findColours(class, plotclr)
  NAColor <- "Black"
  
  if(sum(is.na(colcode))>0){
    colcode[is.na(colcode)] = NAColor
  }
  
  plotclr <- c(plotclr, NAColor)
  
  # Plot the school districts
  if (add_plot == FALSE){
    plot(shape_2plot, col=NAColor)
  }else{
    plot(shape_2plot, col=NAColor,add=add_plot)  
  }
  
  # Plot the acgr data
  plot(shape_2plot, col=colcode, add=TRUE)
  
  # Create Title for color map
  title(main=my_title)

  # set legend text:
  legendText2 <- c()
  
  for(i in 1:(length(breaks)-1)) legendText2 = c(legendText2, paste(round(breaks[i],0), "--", round(breaks[i+1],0)))
  legendText2 = c(legendText2, "NA")
  
  legend("bottomleft", # position
         legend = legendText2, 
         title = "",
         fill = plotclr,
         cex = .5,   # if the legend is too big or small, fiddle with this number.
         bty = "n") # border
}