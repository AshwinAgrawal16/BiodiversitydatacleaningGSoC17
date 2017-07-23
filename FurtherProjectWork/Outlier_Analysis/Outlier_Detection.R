#'
#' @param 
#' X (data.frame) Contains biodiversity data from gbif portal
#'
#' @description 
#' Below method is based on combination of Alpha Hull and Reverse Jack knife to identify the
#' outliers.
#' If the alpha hull technique dosent converge to from a hull then the reverse jack knife
#' method is used for identification of outliers.
#' 
#' @return (X) data.frame 
#'   
#' @example 
#' library(alphahull)
#' library(rgbif)
#' library(plyr)
#' library(maptools)
#' library(raster)
#' library(biogeo)
#'
#'
#'  d1111 <- occ_data(
#'  country = "AU",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  from = 'gbif',
#'  limit=50000,
#'  minimal=FALSE,
#'  hasCoordinate = T
#'  
#' )
#'
#' X1<-d1111$data
#' Outlier_Detection(X1)
#' 
#' 
#'  d111 <- occ_data(
#'  country = "US",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  from = 'gbif',
#'  limit=50000,
#'  minimal=FALSE,
#'  hasCoordinate = T
#'  
#' )
#'
#' X2<-d111$data
#' Outlier_Detection(X2)

#'  d11 <- occ_data(
#'  country = "CN",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  from = 'gbif',
#'  limit=50000,
#'  minimal=FALSE,
#'  hasCoordinate = T
#'  
#' )
#'
#' X3<-d11$data
#' Outlier_Detection(X3)

#'  d1 <- occ_data(
#'  country = "IN",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  from = 'gbif',
#'  limit=50000,
#'  minimal=FALSE,
#'  hasCoordinate = T
#'  
#' )
#'
#' X4<-d1$data
#' Outlier_Detection(X4) 
#' 
#'  d <- occ_data(
#'  country = "JP",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  from = 'gbif',
#'  limit=50000,
#'  minimal=FALSE,
#'  hasCoordinate = T
#'  
#' )
#'
#' X5<-d$data
#' Outlier_Detection(X5)
#' 
#' 
#' 
#' 
Outlier_Detection<-function(X){
  
  ##########################################
  
  DATA<-X[,4:3]
  
  # Alpha Hull approach
  Z<-unique(DATA)
  ahull.obj<-ahull(Z,alpha = 2)
  plot(ahull.obj)
  # ashape.obj<-ashape(Z,alpha = 0.2)
  
  # View(DATA)
  # Now checking the points which are in hull using alpha hull and identifying outliers which are outside hull.
  V<-inahull(ahull.obj,p = cbind(DATA$decimalLongitude,DATA$decimalLatitude))
  V<-as.integer(V)
  Val1<-nrow(DATA)
  DATA1<-DATA
  DATA1[,3]<-V
  count<-subset(DATA1,DATA1$V3==0)
  Val2<-nrow(count)
  # This is the condition which checks whether Hull is formed successfully, if not 
  # Reverse Jack knife is used
  if(Val2<=(Val1/2)){
    
    return(DATA1)
    
  }else{
    
    # Reverse Jack Knife approach
    XXXX1<-rjack((X1$decimalLatitude))
    # XXXX1
    XXXX2<-rjack((X1$decimalLongitude))
    # XXXX2
    
   
    DATA<-as.data.frame(DATA)
    #View(DATA_ALL)
    DATA[,3]<-0
    DATA[XXXX1,3]<-1
    DATA[XXXX2,3]<-1
    
    return(DATA)
  }
  
}

