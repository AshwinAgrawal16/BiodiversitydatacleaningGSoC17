
DwC_Summary_Temporal<-function(X,DATESTART=NULL,
                               DATEEND=NULL,
                               MONTH=NULL,
                               YEAR=NULL,
                               DAY=NULL){
  

  X<-subset(X,select=c(eventDate,month,year,day))
  
  X$eventDate<- as.Date(X$eventDate,
                        format = "%Y-%m-%d")
  
  
  if((nrow(X))==0){
    stop(sprintf("The data set is empty"))
  }
  else{
    
   if(((!is.null(DATESTART) && !is.null(DATEEND)) || !is.null(MONTH) || !is.null((YEAR)) || !is.null(DAY) )){
    
    if((!is.null(DATESTART) && !is.null(DATEEND))){
       d1<- try( as.Date( DATESTART, format= "%Y-%m-%d" ) )
      d2<- try( as.Date( DATEEND , format= "%Y-%m-%d" ) )
      
      if( class(d1) == "try-error" || is.na( d1 ) || class( d2 ) == "try-error" || is.na(d2)  ){
        stop(sprintf("Please enter the date in %Y-%m-%d format"))
      } 
      else{
        DATE1 <- as.Date(DATESTART,format= "%Y-%m-%d")
        DATE2 <- as.Date(DATEEND, format= "%Y-%m-%d")
        X<-X[X$eventDate>=DATE1,] 
        X<-X[X$eventDate<=DATE2,]
        
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
     
     if(!is.null((DAY))){
       X<-subset(X,day==DAY)
       
       if((nrow(X))==0){
         stop(sprintf(("There are no observations in this day")))
         
       }
     }
     
     X1<-X
     names(X1)[names(X1) == "eventDate"] <- "Date_collected"
#The function chronohorogram creates another polar plot representation wherein each day is
#represented by a color dot, and each year, as a concentric ring in the plot, with 365 dots for
#each day of that year. The color of the dot summarizes the number of records on that particular
#day. The color scale is from blue to red, i.e. blue indicated few records and red indicated high 
#35 volume of records. This function is useful in highlighting the seasonality of the data collection or
#of the occurrence of the taxa in question. This function is also useful in identifying temporal gaps in data 
     
     chronohorogram(X1)
     
     
     if(is.null(MONTH)){
       c_1<-ddply(X,~month,summarise,number_of_distinct_orders=length((month)))
       
#The function tempolar provides a polar plot of temporal data, which can be plotted using three different time scales (daily, weekly,
#monthly). The advantage of a polar plot is that the temporal continuity is maintained, in the
#sense that December connects to January, unlike the typical linear plots. This function is useful
#in biodiversity data to understand seasonality of data. The graph can be plotted with points (s),
#lines (r), or polygons (p), or a combination of these types, using the plottype parameter    
#Records can be averaged over years, rather than plotting raw values, using the avg parameter 
       
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
     
     if(is.null(DAY)){
       c_3<-ddply(X,~day,summarise,number_of_distinct_orders=length((day)))
       if(nrow(c_3)>20){
         htmlTable::htmlTable(c_3)
       }
       else{
         plot_ly(c_3, x= ~day, y= ~number_of_distinct_orders,type="bar")
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
      
      c_3<-ddply(X,~day,summarise,number_of_distinct_orders=length((day)))
      if(nrow(c_3)>20){
        htmlTable::htmlTable(c_3)
      }
      else{
        plot_ly(c_3, x= ~day, y= ~number_of_distinct_orders,type="bar")
      }
    }
  }  
}




#Example
library(rgbif)
library(plyr)
library(htmlTable)
library(bdvis)
library(plotly)

d1 <- occ_data(
  country = "AU",     # Country code for australia
  classKey= 359,      # Class code for mammalia
  from = 'gbif',
  limit=50000,
  minimal=FALSE,
  hasCoordinate = T
  
)

X<-d1$data

DwC_Summary_Temporal(X,MONTH = 0)
DwC_Summary_Temporal(X,MONTH = 2)
DwC_Summary_Temporal(X,YEAR = 2017)
DwC_Summary_Temporal(X,YEAR = 2018)
DwC_Summary_Temporal(X,DAY=10)
