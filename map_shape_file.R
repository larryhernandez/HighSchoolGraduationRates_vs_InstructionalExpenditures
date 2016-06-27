map_shape_file<-function(shape_2plot,values,values_id,my_title="",add_plot=FALSE)
{
  # Description:  Takes a shape files and creates visual plot on a map
  # Requires use of the following libraries: sp, maptools, RColorBrewer,classInt

  nclr<-10 # number of bins (i.e. breaks)
  #breaks <-quantile(values, seq(0, 1, len=nclr+1))
  breaks = rep(0,length.out=nclr+1)
  for (i in seq(1,length(breaks)-1)){
    breaks[i+1] = 10*(i)
  }
  
  # the next line is fun to play with. See http://colorbrewer2.org
  plotclr <- brewer.pal(nclr, "RdYlGn")  # ?brewer.pal  # this is a diverging pal
  indices_2map = match(as.numeric(shape_2plot@data$GEOID),values_id)
  plotvar <- values[indices_2map]
  #plotvar <- na.omit(plotvar)
    
  print(length(plotvar))
  
  class <- classIntervals(var = plotvar, n = nclr, style = "fixed",fixedBreaks = breaks)
  colcode <- findColours(class, plotclr)
  NAColor <- "White"
  
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
  
  for(i in 1:(length(breaks)-1)) legendText2 = c(legendText2, paste(breaks[i], "--", breaks[i+1]))
  legendText2 = c(legendText2, "NA")
  
  legend("bottomleft", # position
         legend = legendText2, 
         title = "",
         fill = plotclr,
         cex = .5,   # if the legend is too big or small, fiddle with this number.
         bty = "n") # border
}