#'
#' Main Function DwC_Summary_Spatial 
#'
#'
#' @param  X (data.frame)
#' @param  DECIMALLATITUDE (double)
#' @param  DECIMALLONGITUDE (double)
#' @param  COORDINATEPRECISION (integer)
#' @param  COORDINATEUNCERTAINITYINMETERS (double)
#' @param  COUNTRYCODE (char)
#'
#' @export
#' 
#' @description 
#' DwC summary table.
#' When dealing with large volume of data if initially some information about the 
#' data set in provided, it is vital. 
#' A DwC summary table is one suvh table which provides the summary of different 
#' fields in the data set based on Darwin Core vocabularies.
#' Below is the DwC summary table for Spatial fields.
#'
#'
#' @example 
#' suppressMessages(library(rgbif))
#' suppressMessages(library(sp))
#' suppressMessages(library(spatialEco))
#' suppressMessages(library(ggmap))
#' suppressMessages(library(plyr))
#' suppressMessages(library(htmlTable))
#' suppressMessages(library(bdvis))
#' suppressMessages(library(plotly))
#' suppressMessages(library(pracma))
#' suppressMessages(library(xtable))
#' suppressMessages(library(htmlTable))
#'
#' d1 <- occ_data(
#' country = "AU",     # Country code for australia
#' classKey= 359,      # Class code for mammalia
#' limit=50000,
#' hasCoordinate = T
#'  
#' )
#'
#' X<-d1$data
#'
#'
#' DwC_Summary_spatial(X)
#'
#'
#'
#'
#'
#Function to plot maps of the data based on coordinate uncertainity in meters and coordinate precision
plot_map<-function(X){
  mapgilbert <- get_map(location = c(lon = mean(X$decimalLongitude), lat = mean(X$decimalLatitude)), zoom = 4,
                        maptype = "terrain", scale = 2)
  
  # Normal plotting
  normal_plotting<-ggmap(mapgilbert) +
    geom_point(data = X, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.8, size=5,colour= 'red',fill="red" ),shape=20) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE)+ggtitle("The plot of the whole dataset")
  
  
  # Plotting with coordinatePrecision
  coordinatePrecision_plotting<-ggmap(mapgilbert) +
    geom_point(data = X, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.5, size =coordinatePrecision, colour= coordinatePrecision ),shape=20)+ggtitle("The plot of data with coordinate precision")
  
  
  #Plotting with coordinateuncertainityinmeters
  coordinateuncertainityinmeters_plotting<-ggmap(mapgilbert) +
    geom_point(data = X, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.5, size =coordinateUncertaintyInMeters, colour= coordinateUncertaintyInMeters ),shape=20)+ggtitle("The plot of data with coordinate uncertainity in meters")
  
  cat(paste("Plot of the data\n"))
  print(normal_plotting)
  cat(paste("Plot of the data with coordinate precision\n"))
  print(coordinatePrecision_plotting)
  cat(paste("Plot of the data with coordinate uncertainity in meters\n"))
  print(coordinateuncertainityinmeters_plotting)
  
  
  
  cat(paste(" Bounding box of records ",min(X$decimalLatitude,na.rm=T),",",min(X$decimalLongitude,na.rm=T),
            "------",max(X$decimalLatitude,na.rm=T),",",max(X$decimalLongitude,na.rm=T),"\n"))
  
  
  cat(paste("The bounding box is depicted by\n"))
  
  #Bounding box figure.
  data1<-unique(X[2:1])
  bounding_box<-ggplot() + 
    geom_point( data=data1, aes(x=decimalLongitude, y=decimalLatitude),colour="black", fill="white" )+
    xlab('Longitude')+
    ylab('Latitude')+geom_rect(data=data1, aes(xmin=min(data1$decimalLongitude,na.rm=T) , xmax=max(data1$decimalLongitude,na.rm=T), ymin=min(X$decimalLatitude,na.rm=T), ymax=max(X$decimalLatitude,na.rm=T) ), color="red", fill="transparent") + 
    ggtitle("Plot representing the bounding box of data")
  print(bounding_box)
  
  mean_centerX <- mean(as.matrix(X[,1]))
  mean_centerY <- mean(as.matrix(X[,2]))
  
  cat(sprintf("Centre of the data set- %.4f% .4f\n",mean_centerX,mean_centerY))
  
  standard_deviationX <- sd(as.matrix(X[,1]))
  standard_deviationY <- sd(as.matrix(X[,2]))
  standard_distance <- sqrt(sum(((X[,1]-mean_centerX)^2+(X[,2]-mean_centerY)^2))/(nrow(X)))
  
  cat(sprintf("Standard deviation in coordinates %.4f %.4f\n",standard_deviationX,standard_deviationY))
  cat(sprintf("Standard distnace of points %.4f\n", standard_distance))
  
  cat(sprintf("Area of the ploygon formed by the coordinates %.4f\n",pracma::polyarea(as.matrix(X[,1]),as.matrix(X[,2]))))
  
  
  #Inter Quartile range
  ll<-as.matrix(X[,1])
  iqr1<-IQR(ll,type = 4)
  ll<-as.matrix(X[,2])
  iqr2<-IQR(ll,type=4)
  
}

# Function to plot a denisty circle, a area encircled which covers the most dense area of points in map.

density_circle<-function(X){
  
  cat(sprintf("Below is the density circle","\n"))
  
  # Plotting density circle
  data1<-unique(X[,2:1])
  mean_centre <- apply(data1, 2, mean)
  
  # standard distance
  sd <- sqrt(sum((data1[,1] - mean_centre[1])^2 + (data1[,2] - mean_centre[2])^2) / nrow(data1))
  
  
  # visualising the data based on measures of central tendency
  plot(data1, col='light blue')
  points(data1, cex=.5)
  points(cbind(mean_centre[1], mean_centre[2]), pch='*', col='red', cex=5)
  
  # make a circle
  bearing <- 1:360 * pi/180
  cx <- mean_centre[1] + sd * cos(bearing)
  cy <- mean_centre[2] + sd * sin(bearing)
  circle <- cbind(cx, cy)
  lines(circle, col='red', lwd=2)
  title(main = "Plot for desnity circle")
}

nearest_neighour_index<-function(X){
  
  
  
  # nearest neighbour index
  
  #The nearest neighbour index is expressed as the ratio of the observed distance divided by the expected
  #distance. The expected distance is the average distance between neighbours in a hypothetical
  #random distribution. If the index is less than 1, the pattern exhibits clustering; if the index is
  #greater than 1, the trend is toward dispersion or competition. The Nearest Neighbour Index is calculated
  #as: Mean Nearest Neighbour Distance (observed) D(nn) = sum(min(Dij)/N) Mean Random
  #Distance (expected) D(e) = 0.5 SQRT(A/N) Nearest Neighbour Index NNI = D(nn)/D(e) Where;
  #D=neighbour distance, A=Area
  
  data1<-unique(X[,2:1])
  sp_mydata <- data1
  coordinates(sp_mydata) <- ~decimalLongitude+decimalLatitude
  
  NNI<-spatialEco::nni(sp_mydata)
  
  cat(sprintf("The expected nearest neighbour distance is %.4f\n",NNI$expected.mean.distance))
  cat(sprintf("The observed nearest neighbour distance is %.4f\n",NNI$observed.mean.distance))
  
  if(NNI$NNI>1){
    cat(sprintf("The nearest neigbhour index is greater that one- %.4f therefore the data is dispersed",NNI$NNI))
  }
  
  if(NNI$NNI<1){
    cat(sprintf("The nearest neigbhour index is greater that one- %.4f therefore the data is clustered",NNI$NNI))
  }
  
  if(NNI$NNI<0.01 && NNI$NNI>-0.01){
    cat(sprintf("The nearest neigbhour index is very close to zero- %.4f therefore the data is normal\n",NNI$NNI))
  }
  
  if((NNI$z.score<-2.58 || NNI$NNI>2.58) && NNI$p<0.01){
    cat(paste("The null hypothesis can be rejected and data is significant and not random","\n"))
  }
  else{
    cat(paste("The null hypothesis cannot be rejected and data can be random","\n"))
  }
  
  #Kernel density
  sp_mydata <- X
  coordinates(sp_mydata) <- ~decimalLongitude+decimalLatitude
  nni(sp_mydata)
  nni(sp_mydata)
  #View(coordinates(sp.mydata))
  kde<-spatialEco::kde2D(sp_mydata)
  
  cat(paste("The plot for kernel density","\n"))
  plot(kde$kde)
  plot(sp_mydata, pch=20, cex=0.75, col="red", add=TRUE)
  title(main = "Plot for kernel density")
  
  cat(paste("The bandwidth for the kernel density is","\n"))
  cat(sprintf("%.4f",kde$bandwidth))
  cat(paste("\n"))
  
}

plot_table<-function(X,COORDINATEPRECISION=NULL,
                     COORDINATEUNCERTAINITYINMETERS=NULL){
  
  l <- htmltools::tagList()
  #l1<- htmltools::tagList()
  i=1
  #j=1
  
  #CoordinatePrecision & coordinateuncertainityinmeters
  if(!is.null(COORDINATEPRECISION)){
    data_cp<-subset(X,X$coordinatePrecision<COORDINATEPRECISION)
    if(nrow(data_cp)==0){
      cat(paste("There are no observations with coordinate precision greater than ",COORDINATEPRECISION,"\n"))
    }
    else{
      c_1<-ddply(data_cp,~coordinatePrecision,summarise,number_of_distinct_orders=length((coordinatePrecision)))
      
      plot1<-plot_ly(c_1, x= ~coordinatePrecision, y= ~number_of_distinct_orders,type="bar")
      table1<-htmlTable(c_1)
      l[[i]]=table1
      i=i+1
      #htmlTableWidgetOutput(b)
      #print(a)
      #htmltools::tagList(list(a, a))
      l[[i]]=plot1
      i=i+1
      
    }
  }
  else{
    c_3<-ddply(X,~coordinatePrecision,summarise,number_of_distinct_orders=length((coordinatePrecision)))
    
    plot1<-plot_ly(c_3, x= ~coordinatePrecision, y= ~number_of_distinct_orders,type="bar")
    
    #htmlTable(c_3)
    
    #shiny::renderPlot(a)
    table1<-htmlTable::htmlTable(c_3)
    
    #renderPlotly(a)
    max1<-max(c_3[,2])
    cat(paste("The maximum value for coordinatePrecision is",max1,"\n"))
    print(c_3[c_3$number_of_distinct_orders==max1,])
    
    min2<-min(c_3[,2])
    cat(paste("The minimum value for coordinatePrecision is",min2,"\n"))
    print(c_3[c_3$number_of_distinct_orders==min2,])
    
    #htmlTableWidgetOutput(b)
    #print(a)
    #htmltools::tagList(list(a, a))
    l[[i]]=table1
    i=i+1
    l[[i]]=plot1
    i=i+1
    
  }
  
  if(!is.null(COORDINATEUNCERTAINITYINMETERS)){
    data_cum<-subset(X,X$coordinateUncertaintyInMeters<COORDINATEUNCERTAINITYINMETERS)
    if(nrow(data_cum)==0){
      cat(paste("There are no observations with COORDINATE UNCERTAINITY IN METERS less than ",COORDINATEUNCERTAINITYINMETERS,"\n"))
    }
    else{
      c_2<-ddply(data_cum,~coordinateUncertaintyInMeters,summarise,number_of_distinct_orders=length((coordinateUncertaintyInMeters)))
      
      plot1<-plot_ly(c_2, x= ~coordinatePrecision, y= ~number_of_distinct_orders,type="bar")
      
      #htmlTable(c_2)
      
      table1<-htmlTable::htmlTable(c_2)
      
      #print(a)
      #htmlTableWidgetOutput(b)
      #htmltools::tagList(list(a, a))
      l[[i]]=table1
      i=i+1
      l[[i]]=plot1
      i=i+1
      
    }
  }
  else{
    c_2<-ddply(X,~coordinateUncertaintyInMeters,summarise,number_of_distinct_orders=length((coordinateUncertaintyInMeters)))
    plot1<-plot_ly(c_2, x= ~coordinateUncertaintyInMeters, y= ~number_of_distinct_orders,type="bar")
    
    #htmlTable(c_2)
    #htmlTableWidgetOutput(b)
    
    table1<-htmlTable::htmlTable(c_2)
    max1<-max(c_2[,2])
    cat(paste("The maximum value for coordinateUncertaintyInMeters is",max1,"\n"))
    print(c_2[c_2$number_of_distinct_orders==max1,])
    
    min2<-min(c_2[,2])
    cat(paste("The minimum value for coordinateUncertaintyInMeters is",min2,"\n"))
    print(c_2[c_2$number_of_distinct_orders==min2,])
    #print(a)
    #htmlTableWidgetOutput(b)
    #htmltools::tagList(list(a, a))
    l[[i]]=table1
    i=i+1
    l[[i]]=plot1
    i=i+1
    
  }
  l
  #l1
}

#Driver function for whole summary table

DwC_Summary_spatial<-function(X,DECIMALLATITUDE=NULL,
                              DECIMALLONGITUDE=NULL,
                              COORDINATEPRECISION=NULL,
                              COORDINATEUNCERTAINITYINMETERS=NULL,
                              COUNTRYCODE=NULL){
  
  
  
  
  #options(digits = 4)
  if(nrow(X)==0){
    stop(sprintf("The data set is empty"))
  }
  else{
    X<-subset(X,select=c(decimalLatitude,decimalLongitude,coordinatePrecision,
                         coordinateUncertaintyInMeters))
    
    
    plot_map(X)
    
    density_circle(X)
    
    nearest_neighour_index(X)
    
    plot_table(X)
    
  }
  
}

