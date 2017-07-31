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
#'  limit=50000,
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
#'  limit=50000,
#'  hasCoordinate = T
#'  
#' )
#'
#' X2<-d111$data
#' Outlier_Detection(X2)

#'  d11 <- occ_data(
#'  country = "CN",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  limit=50000,
#'  hasCoordinate = T
#'  
#' )
#'
#' X3<-d11$data
#' Outlier_Detection(X3)

#'  d1 <- occ_data(
#'  country = "IN",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  limit=50000,
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
#'  limit=50000,
#'  hasCoordinate = T
#'  
#' )
#'
#' X5<-d$data
#' Outlier_Detection(X5)
#' 
#' @return Returns a data frame with flags indicating the outliers with a column indicating the 
#' method used for identifing the outliers.
#' 
#' The outliers are identified based on specific species and if the distinct number of species are 
#' less than 100 then these species are not taken into consideration while identifing the outliers
#' 
Outlier_Detection<-function(X){
  
  ##########################################PART1
  
  DATA<-subset(X,select = c(decimalLongitude,decimalLatitude,scientificName,key))
  
  c_1<-ddply(DATA,~scientificName,summarise,number_of_distinct_orders=length((scientificName)))
  #View(c_1)
  
  result<-data.frame()
  
  for(i in 1:nrow(c_1)){
    if(c_1[i,2]>=100){
  # Alpha Hull approach
  
  data1<-subset(DATA,DATA$scientificName==c_1[i,1])
  data_unique<-unique(data1[,1:2])
  ahull.obj<-ahull(data_unique,alpha = 2)
  #plot(ahull.obj)
  # ashape.obj<-ashape(Z,alpha = 0.2)
  
  # View(DATA)
  # Now checking the points which are in hull using alpha hull and identifying outliers which are outside hull.
  data_inside_hull<-inahull(ahull.obj,p = cbind(data1$decimalLongitude,data1$decimalLatitude))
  head(data_inside_hull)
  data_inside_hull<-as.integer(data_inside_hull==FALSE)
  Value1<-nrow(data1)
  data_after_hull<-data1
  data_after_hull[,5]<-data_inside_hull
  count<-subset(data_after_hull,data_after_hull$V5==1)
  Value2<-nrow(count)
  
  # This is the condition which checks whether Hull is formed successfully, if not 
  # Reverse Jack knife is used
  if(Value2<=(Value1*9/10)){
    
    data_after_hull[,6]<-"alphaHull"
    result<-rbind(result,data_after_hull)
    #View(data_after_hull)
    
  }
  ####################################PART2
  else{
    
    # Reverse Jack Knife approach
    jackknife_flag1<-rjack((data1$decimalLatitude))
    # XXXX1
    jackknife_flag2<-rjack((data1$decimalLongitude))
    # XXXX2
    
   
    data_jack_knife<-as.data.frame(data1)
    #View(DATA_ALL)
    data_jack_knife[,5]<-0
    data_jack_knife[jackknife_flag1,5]<-1
    data_jack_knife[jackknife_flag2,5]<-1
    
    data_jack_knife[,6]<-"jackKnife"
    
    result<-rbind(result,data_jack_knife)
    
    #return(DATA)
      }
    }
    
  }
  
  
  return(result)
}

