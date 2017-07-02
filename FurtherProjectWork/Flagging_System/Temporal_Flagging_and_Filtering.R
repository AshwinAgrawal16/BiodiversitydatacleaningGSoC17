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
#' Z1<-Temporal_Flagging(X,FLAG_NAME = "date") 
#' View(Z1)
#' 
#' Z2<-Temporal_Filtering(X,FLAG_NAME = "date")
#' View(Z2)
#' 
#'





# Check if field is a date using as.Date that looks for unambiguous dates
#   Assumes date format so NA returned not Character error. 
#   with no date format, R tries two defaults then gives error. 
#   BUT With a dateformat R returns NA
# Args
#   Suspected date and optional date format string
# Returns
#   TRUE if thinks it is a date

IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}


# Function 1-Flagging Wrapper Function
Temporal_Flagging<-function(X,FLAG_NAME){
  
  C<-Temporal_Flagging_and_Filtering_Main(X,flag_name=FLAG_NAME,flag=TRUE)
  return(C)
  
}

#Function 2-Filtering Wrapper Function

Temporal_Filtering<-function(X,FLAG_NAME){
  
  C<-Temporal_Flagging_and_Filtering_Main(X,flag_name = FLAG_NAME,filter=TRUE)
  return(C)
}


# Function 3-Main function
Temporal_Flagging_and_Filtering_Main<-function(X,flag_name,flag=FALSE,filter=FALSE){
  
  
  if(flag_name=="date" || flag_name=="country"){
    
    if(flag_name=="date"){
     
      Data_flag_date<-subset(X,select=c(key,eventDate,day,month,year))
      
      Data_flag_date$flag_eventDate<-as.integer(!IsDate(Data_flag_date$eventDate,date.format="%Y-%m-%d"))
      Data_flag_date$flag_day<-as.integer((Data_flag_date$day<1 | Data_flag_date$day>31))
      Data_flag_date$flag_month<-as.integer((Data_flag_date$month<1 | Data_flag_date$month>12))
      Data_flag_date$flag_year<-as.integer((Data_flag_date$year<1900 | Data_flag_date$year>2017))
   
     
    
    if(flag==TRUE){
      
      cat(paste("The flagged data with convention: If flag is 1 then data is not correct or empty otherwise data is correct and validated"))
      return(Data_flag_date)
    }
    

# Here filtering is done based on only event date.        
    if(filter==TRUE){
      
      
      cat(sprintf("The filtering is done based on only eventDate field\n"))
      X$flag_eventDate<-Data_flag_date$flag_eventDate
      X<-subset(X,X$flag_eventDate==0 )
      drops <- c("flag_eventDate")
      X<-X[ , !(names(X) %in% drops)]
      
      if(nrow(X)==0){
        cat(sprintf("The filtered data is empty\n"))
      }
      else{
        return(X)
      }
      
    }
      
    }
    
    
# ToDo Flagging and filtering of country field    
    
    
    
    
  }
  else{
    cat(paste("The flag name is not correct"))
  }
  
  
}
