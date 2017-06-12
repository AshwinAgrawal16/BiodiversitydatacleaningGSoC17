DwC_Summary_Temporal<-function(X,DATESTART=NULL,
                               DATEEND=NULL
                               MONTH=NULL,
                               YEAR=NULL){
  
  
  X<-subset(X,select=c(eventDate,month,year))
  
  if(is.null(nrow(X))){
    cat(paste("The data set is empty"))
  }
  else{
    
   if((!is.null(DATESTART) && !is.null(DATEEND)) || !is.null(MONTH) || !is.null(nrow(YEAR)) ){
    
    if(!is.null(DATESTART) && !is.null(DATEEND)){
      
      
      if(is.null(nrow(X))){
        stop(sprintf("There are no observations between the specified date"))
      }
    }
    if(!is.null(MONTH)){
      X<-subset(X,month=MONTH)
      
      if(is.null(nrow(X))){
        stop(sprintf("There are no observations in the specified month"))
      }
    }
     
    if(!is.null(nrow(YEAR))){
      X<-subset(X,year=YEAR)
      
      if(is.null(nrow(YEAR))){
        stop(sprintf(("There are no observations in this year")))
      }
    }
     
     X1<-X
     names(X1)[names(X1) == "eventDate"] <- "Date_collected"
     chronohorogram(X1)
     
     
     if(is.null(MONTH)){
       c_1<-ddply(X,~month,summarise,number_of_distinct_orders=length((month)))
       plot_ly(c_1, x= ~month, y= ~number_of_distinct_orders,type="bar")
       tempolar(X1)
       
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
      
    }
  }  
}