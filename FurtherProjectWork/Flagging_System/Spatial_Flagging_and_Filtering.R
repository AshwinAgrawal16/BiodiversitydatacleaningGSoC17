
#'
#'
#'
#'
#' @param X (data.frame) contains gbif data frame
#' @param flag_name (character) contains name of the flag
#' @param flag (logical) If true returns a data frame containing flags
#' @param filter (logical) If true returns a data frame containing filtered data.
#'
#'
#'
#
#'
#'
#'
#' @example 
#' library(rgbif)
#' 
#' d1 <- occ_data(
#' country = "AU",     # Country code for australia
#' classKey= 359,      # Class code for mammalia
#' from = 'gbif',
#' limit=50000,
#' minimal=FALSE,
#' hasCoordinate = T
#' )
#' 
#' X<-d1$data
#' 
#' Z1<-Spatial_Flagging(X,FLAG_NAME = "coordinatePrecision") 
#' View(Z1)
#' 
#' Z2<-Spatial_Filtering(X,FLAG_NAME = "coordinateUncertaintyInMeters")
#' View(Z2)
#' 
#'




# Function 1-Flagging Wrapper Function
Spatial_Flagging<-function(X,FLAG_NAME){
  
  C<-Spatial_Flagging_and_Filtering_Main(X,flag_name=FLAG_NAME,flag=TRUE)
  return(C)
  
}

#Function 2-Filtering Wrapper Function

Spatial_Filtering<-function(X,FLAG_NAME){
  
  C<-Spatial_Flagging_and_Filtering_Main(X,flag_name = FLAG_NAME,filter=TRUE)
  return(C)
}

# Function 3- Main function

Spatial_Flagging_and_Filtering_Main<-function(X,flag_name,filter=FALSE,flag=FALSE){
  
  #Data_flag<-subset(X,select=c(key,decimalLatitude.....))
  
  if(flag_name=="coordinates" || flag_name=="coordinatePrecision" ||
     flag_name=="coordinateUncertaintyInMeters"){
    
    if(flag_name=="coordinates"){
      Data_flag_coordinates<-subset(X,select=c(key,decimalLatitude,decimalLongitude))
      Data_flag_coordinates$flag_latitude<-as.integer(is.na(Data_flag_coordinates$decimalLatitude))
      Data_flag_coordinates$flag_longitude<-as.integer(is.na(Data_flag_coordinates$decimalLongitude))
      
    
    
    if(flag==TRUE){
      
      cat(sprintf("The flags are: if flag value is 1 then NA otherwise observation is present"))
      return(Data_flag_coordinates)
    }
    
    if(filter==TRUE){
      
      X$flag_latitude<-Data_flag_coordinates$flag_latitude
      X$flag_longitude<-Data_flag_coordinates$flag_longitude
      X<-subset(X,X$flag_latitude==0 && X$flag_longitude==0)
      drops <- c("flag_latitude","flag_longitude")
      X<-X[ , !(names(X) %in% drops)]
      
      if(nrow(X)==0){
        stop(paste("The data after filtering is empty"))
      }else{
        cat(sprintf("The filtered data based on coordinates is\n"))
        return(X)
     }
    
   
    }
      
    }
    
    
    if(flag_name=="coordinatePrecision"){
      Data_flag_coordinates<-subset(X,select=c(key,coordinatePrecision))
      Data_flag_coordinates$flag_coordinatePrecision<-as.integer(is.na(Data_flag_coordinates$coordinatePrecision))
      
      if(flag==TRUE){
        
        cat(sprintf("The flags are: if flag value is 1 then NA otherwise observation is present"))
        return(Data_flag_coordinates)
      }
      
      if(filter==TRUE){
        
        X$flag_coordinatePrecision<-Data_flag_coordinates$flag_coordinatePrecision
        X<-subset(X,X$flag_coordinatePrecision==0 )
        drops <- c("flag_coordinatePrecision")
        X<-X[ , !(names(X) %in% drops)]
        
        if(nrow(X)==0){
          stop(paste("The data after filtering is empty"))
        }else{
          cat(sprintf("The filtered data based on coordinatePrecision is\n"))
          return(X)
        }
        
        
      }
      
    }
    
    
    X$coordinateUncertaintyInMeters
    if(flag_name=="coordinateUncertaintyInMeters"){
      Data_flag_coordinates<-subset(X,select=c(key,coordinateUncertaintyInMeters))
      Data_flag_coordinates$flag_coordinateUncertaintyInMeters<-as.integer(is.na(Data_flag_coordinates$coordinateUncertaintyInMeters))
      
      if(flag==TRUE){
        
        cat(sprintf("The flags are: if flag value is 1 then NA otherwise observation is present"))
        return(Data_flag_coordinates)
      }
      
      if(filter==TRUE){
        
        X$flag_coordinateUncertaintyInMeters<-Data_flag_coordinates$flag_coordinateUncertaintyInMeters
        X<-subset(X,X$flag_coordinateUncertaintyInMeters==0 )
        drops <- c("flag_coordinateUncertaintyInMeters")
        X<-X[ , !(names(X) %in% drops)]
        
        if(nrow(X)==0){
          stop(paste("The data after filtering is empty"))
        }else{
          cat(sprintf("The filtered data based on coordinateUncertaintyInMeters is\n"))
          return(X)
        }
        
        
      }
      
    }
    
    
  }else{
    
    stop(paste("The flag name is wrong"))
    }
  
  
  }
  
