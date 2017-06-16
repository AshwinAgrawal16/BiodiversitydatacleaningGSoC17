plot_map<-function(X){
  mapgilbert <- get_map(location = c(lon = mean(X$decimalLongitude), lat = mean(X$decimalLatitude)), zoom = 4,
                        maptype = "terrain", scale = 2)
  
  # Normal plotting
  a<-ggmap(mapgilbert) +
    geom_point(data = X, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.8, size=5,colour= 'red',fill="red" ),shape=20) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE)
  
  
  # Plotting with coordinatePrecision
  b<-ggmap(mapgilbert) +
    geom_point(data = X, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.5, size =coordinatePrecision, colour= coordinatePrecision ),shape=20)
  
  
  #Plotting with coordinateuncertainityinmeters
  c<-ggmap(mapgilbert) +
    geom_point(data = X, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.5, size =coordinateUncertaintyInMeters, colour= coordinateUncertaintyInMeters ),shape=20)
  
  cat(paste("Plot of the data\n"))
  print(a)
  cat(paste("Plot of the data with coordinate precision\n"))
  print(b)
  cat(paste("Plot of the data with coordinate uncertainity in meters\n"))
  print(c)
  
  
  
  cat(paste(" Bounding box of records ",min(X$decimalLatitude,na.rm=T),",",min(X$decimalLongitude,na.rm=T),
            "------",max(X$decimalLatitude,na.rm=T),",",max(X$decimalLongitude,na.rm=T),"\n"))
  
  
  mean_centerX <- mean(X[,1])
  mean_centerY <- mean(X[,2])
  
  cat(paste("Centre of the data set",mean_centerX,",",mean_centerY,"\n"))
  
  standard_deviationX <- sd(X[,1])
  standard_deviationY <- sd(X[,2])
  standard_distance <- sqrt(sum(((X[,1]-mean_centerX)^2+(X[,2]-mean_centerY)^2))/(nrow(X)))
  
  cat(paste("Standard deviation in coordinates", standard_deviationX,"," ,standard_deviationY,"\n"))
  cat(paste("Standard distnace of points", standard_distance,"\n"))
  
  cat(paste("Area of the ploygon formed by the coordinates",pracma::polyarea(X[,1],X[,2]),"\n"))
  
  
  #Inter Quartile range
  ll<-as.double(X[,1])
  iqr1<-IQR(ll,type = 4)
  ll<-as.double(X[,2])
  iqr2<-IQR(ll,type=4)
  
}

density_circle<-function(X){
  
  cat(sprintf("Below is the density circle","\n"))
  
  # Plotting density circle
  X1<-unique(X[,2:1])
  mc <- apply(X1, 2, mean)
  
  # standard distance
  sd <- sqrt(sum((X1[,1] - mc[1])^2 + (X1[,2] - mc[2])^2) / nrow(X1))
  
  
  # visualising the data based on measures of central tendency
  plot(X1, col='light blue')
  points(X1, cex=.5)
  points(cbind(mc[1], mc[2]), pch='*', col='red', cex=5)
  
  # make a circle
  bearing <- 1:360 * pi/180
  cx <- mc[1] + sd * cos(bearing)
  cy <- mc[2] + sd * sin(bearing)
  circle <- cbind(cx, cy)
  lines(circle, col='red', lwd=2)
}

nearest_neighour_index<-function(X){
  
  # nearest neighbour index
  X1<-unique(X[,2:1])
  sp.mydata <- X1
  coordinates(sp.mydata) <- ~decimalLongitude+decimalLatitude
  
  NNI<-spatialEco::nni(sp.mydata)
  
  cat(paste("The expected nearest neighbour distance is",NNI$expected.mean.distance,"\n"))
  cat(paste("The observed nearest neighbour distance is",NNI$observed.mean.distance,"\n"))
  
  if(NNI$NNI>1){
    cat(paste("The nearest neigbhour index is greater that one- ",NNI$NNI," therefore the data is dispersed","\n"))
  }
  
  if(NNI$NNI<1){
    cat(paste("The nearest neigbhour index is greater that one-",NNI$NNI," therefore the data is clustered","\n"))
  }
  
  if(NNI$NNI<0.01 && NNI$NNI>-0.01){
    cat(paste("The nearest neigbhour index is very close to zeor-",NNI$NNI," therefore the data is normal","\n"))
  }
  
  if((NNI$z.score<-2.58 || NNI$NNI>2.58) && NNI$p<0.01){
    cat(paste("The null hypothesis can be rejected and data is significant and not random","\n"))
  }
  else{
    cat(paste("The null hypothesis cannot be rejected and data can be random","\n"))
  }
  
  #Kernel density
  sp.mydata <- X
  coordinates(sp.mydata) <- ~decimalLongitude+decimalLatitude
  nni(sp.mydata)
  nni(sp.mydata)
  #View(coordinates(sp.mydata))
  kde<-spatialEco::kde2D(sp.mydata)
  
  cat(paste("The plot for kernel density","\n"))
  plot(kde$kde)
  plot(sp.mydata, pch=20, cex=0.75, col="red", add=TRUE)
  
  cat(paste("The bandwidth for the kernel density is","\n"))
  cat(paste(kde$bandwidth))
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
    XCP<-subset(X,X$coordinatePrecision<COORDINATEPRECISION)
    if(nrow(XCP)==0){
      cat(paste("There are no observations with coordinate precision greater than ",COORDINATEPRECISION,"\n"))
    }
    else{
      c_1<-ddply(XCP,~coordinatePrecision,summarise,number_of_distinct_orders=length((coordinatePrecision)))
      
      a<-plot_ly(c_1, x= ~coordinatePrecision, y= ~number_of_distinct_orders,type="bar")
      #b<-htmlTable(c_1)
      l[[i]]=b
      i=i+1
      #htmlTableWidgetOutput(b)
      #print(a)
      #htmltools::tagList(list(a, a))
      l[[i]]=a
      i=i+1
      
    }
  }
  else{
    c_3<-ddply(X,~coordinatePrecision,summarise,number_of_distinct_orders=length((coordinatePrecision)))
    
    a<-plot_ly(c_3, x= ~coordinatePrecision, y= ~number_of_distinct_orders,type="bar")
    
    #htmlTable(c_3)
    
    #shiny::renderPlot(a)
    b<-htmlTable::htmlTable(c_3)
    
    #renderPlotly(a)
    v1<-max(c_3[,2])
    cat(paste("The maximum value for coordinatePrecision is",v1,"\n"))
    print(c_3[c_3$number_of_distinct_orders==v1,])
    
    v2<-min(c_3[,2])
    cat(paste("The minimum value for coordinatePrecision is",v2,"\n"))
    print(c_3[c_3$number_of_distinct_orders==v2,])
    
    #htmlTableWidgetOutput(b)
    #print(a)
    #htmltools::tagList(list(a, a))
    l[[i]]=b
    i=i+1
    l[[i]]=a
    i=i+1
    
  }
  
  if(!is.null(COORDINATEUNCERTAINITYINMETERS)){
    XCP<-subset(X,X$coordinateUncertaintyInMeters<COORDINATEUNCERTAINITYINMETERS)
    if(nrow(XCP)==0){
      cat(paste("There are no observations with COORDINATE UNCERTAINITY IN METERS less than ",COORDINATEUNCERTAINITYINMETERS,"\n"))
    }
    else{
      c_2<-ddply(XCP,~coordinateUncertaintyInMeters,summarise,number_of_distinct_orders=length((coordinateUncertaintyInMeters)))
      
      a<-plot_ly(c_2, x= ~coordinatePrecision, y= ~number_of_distinct_orders,type="bar")
      
      #htmlTable(c_2)
      
      b<-htmlTable::htmlTable(c_2)
      
      #print(a)
      #htmlTableWidgetOutput(b)
      #htmltools::tagList(list(a, a))
      l[[i]]=b
      i=i+1
      l[[i]]=a
      i=i+1
      
    }
  }
  else{
    c_2<-ddply(X,~coordinateUncertaintyInMeters,summarise,number_of_distinct_orders=length((coordinateUncertaintyInMeters)))
    a<-plot_ly(c_2, x= ~coordinateUncertaintyInMeters, y= ~number_of_distinct_orders,type="bar")
    
    #htmlTable(c_2)
    #htmlTableWidgetOutput(b)
    
    b<-htmlTable::htmlTable(c_2)
    v1<-max(c_2[,2])
    cat(paste("The maximum value for coordinateUncertaintyInMeters is",v1,"\n"))
    print(c_2[c_2$number_of_distinct_orders==v1,])
    
    v2<-min(c_2[,2])
    cat(paste("The minimum value for coordinateUncertaintyInMeters is",v2,"\n"))
    print(c_2[c_2$number_of_distinct_orders==v2,])
    #print(a)
    #htmlTableWidgetOutput(b)
    #htmltools::tagList(list(a, a))
    l[[i]]=b
    i=i+1
    l[[i]]=a
    i=i+1
    
  }
  l
  #l1
}


DwC_Summary_spatial<-function(X,DECIMALLATITUDE=NULL,
                              DECIMALLONGITUDE=NULL,
                              COORDINATEPRECISION=NULL,
                              COORDINATEUNCERTAINITYINMETERS=NULL,
                              COUNTRYCODE=NULL){
  
  
  
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


#EXAMPLE
suppressMessages(library(rgbif))
suppressMessages(library(sp))
suppressMessages(library(spatialEco))
suppressMessages(library(ggmap))
suppressMessages(library(plyr))
suppressMessages(library(htmlTable))
suppressMessages(library(bdvis))
suppressMessages(library(plotly))
suppressMessages(library(pracma))
suppressMessages(library(xtable))
suppressMessages(library(htmlTable))

d1 <- occ_data(
  country = "AU",     # Country code for australia
  classKey= 359,      # Class code for mammalia
  from = 'gbif',
  limit=50000,
  minimal=FALSE,
  hasCoordinate = T
  
)

X<-d1$data


DwC_Summary_spatial(X)
