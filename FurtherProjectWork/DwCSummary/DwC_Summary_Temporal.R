DwC_Summary_Temporal<-function(X,DATESTART=NULL,
                               DATEEND=NULL,
                               MONTH=NULL,
                               YEAR=NULL){
  

  X<-subset(X,select=c(eventDate,month,year))

  
  if((nrow(X))==0){
    stop(sprintf("The data set is empty"))
  }
  else{
    
   if(((!is.null(DATESTART) && !is.null(DATEEND)) || !is.null(MONTH) || !is.null((YEAR)) )){
    
    if((!is.null(DATESTART) && !is.null(DATEEND))){
       d1<- try( as.Date( DATESTART, format= "%Y-%m-%d %H:%M:%S" ) )
      d2<- try( as.Date( DATEEND , format= "%Y-%m-%d %H:%M:%S" ) )
      
      if( class(d1) == "try-error" || is.na( d1 ) || class( d2 ) == "try-error" || is.na(d2)  ){
        stop(sprintf("Please enter the date in %Y-%m-%d %H:%M:%S format"))
      } 
      else{
        DATE1 <- as.Date(DATESTART)
        DATE2 <- as.Date(DATEEND)
        X<-X[X$eventDate>=DATE1 && X$eventDate<=DATE2,]
        
      }
      if((nrow(X))==0){
        stop(sprintf("There are no observations between the specified date"))
        }
      }
    
    if(!is.null(MONTH)){
      X<-subset(X,month==MONTH)
      
      if(MONTH>12 || MONTH<1){
        stop(sprintf("The month is wrong"))
      }
      
      if((nrow(X))==0){
        stop(sprintf("There are no observations in the specified month"))
      }
    }
     
    if(!is.null((YEAR))){
      X<-subset(X,year==YEAR)
      
      if((nrow(X))==0){
        stop(sprintf(("There are no observations in this year")))
        
      }
    }
     
     X1<-X
     names(X1)[names(X1) == "eventDate"] <- "Date_collected"
     chronohorogram(X1)
     
     
     if(is.null(MONTH)){
       c_1<-ddply(X,~month,summarise,number_of_distinct_orders=length((month)))
       
       tempolar(X1)
       plot_ly(c_1, x= ~month, y= ~number_of_distinct_orders,type="bar")
     }
     
     if(is.null(YEAR)){
       c_2<-ddply(X,~year,summarise,number_of_distinct_orders=length((year)))
       if(nrow(c_2)>10){
         htmlTable::htmlTable(c_2)
       }
       else{
         plot_ly(c_2, x= ~year, y= ~number_of_distinct_orders,type="bar")
       }
     }
     
   }
    
    else{
      X1<-X
      names(X1)[names(X1) == "eventDate"] <- "Date_collected"
      chronohorogram(X1)
      
      c_1<-ddply(X,~month,summarise,number_of_distinct_orders=length((month)))
      plot_ly(c_1, x= ~month, y= ~number_of_distinct_orders,type="bar")
      tempolar(X1)
      
      c_2<-ddply(X,~year,summarise,number_of_distinct_orders=length((year)))
      if(nrow(c_2)>10){
        htmlTable::htmlTable(c_2)
      }
      else{
        plot_ly(c_2, x= ~year, y= ~number_of_distinct_orders,type="bar")
      }
    }
  }  
}

#EXAMPLE

library(rgbif)
library(plyr)
library(htmlTable)
library(bdvis)
library(plotly)

DwC_Summary_Temporal(X,MONTH = 0)
DwC_Summary_Temporal(X,MONTH = 2)
DwC_Summary_Temporal(X,YEAR = 2017)
DwC_Summary_Temporal(X,YEAR = 2018)
DwC_Summary_Temporal(X)
