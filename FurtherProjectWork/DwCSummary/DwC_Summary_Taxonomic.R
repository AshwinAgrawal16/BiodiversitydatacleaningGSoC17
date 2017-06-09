bdsummary_taxonomic<-function(X, NAME=NULL, 
                              COUNTRYCODE=NULL,
                              BASIS_OF_RECORD=NULL,
                              KINGDOM=NULL,
                              PHYLUM=NULL,
                              ORDER=NULL,
                              FAMILY=NULL,
                              GENUS=NULL,
                              CLASS=NULL,
                              SPECIFICEPITHET=NULL,
                              VERNACULARNAME=NULL,
                              INSTITUTIONCODE=NULL,
                              TAXONRANK=NULL,
                              LIMIT=NULL
                              ){

##################    
# Below is the basic outline how I plan to build the taxonomic bdsummary function  
# Some of the parts are not completed because it the outline and moreover the  
# remaining part which is not coded is quite similar to the coded part. 
# More wraper functions can be developed to enhance the visualizing capability 
# of the plots and tables. 
#############  
# Here first I will check what is the query of the user and accordingly I will
# check if user is querring something specific, then I will subset the data set 
# according to the need and accordingly I will omit that particluar field statistic 
# as it has already been fixed by the user.
# For example like user selects that he/she wants a specific species statistic 
# like name="Antechinus stuartii", therefore I will omit the "name" field from 
# the summary. If there are no specific query by the user I will show all summary 
# statistics. This will account for all premutation and combination possible.
# For the rmarkdown file I have used htmlTables or xtable package as it easy to
# implement and easy to customize. I am also exploring the markdown table and pandoc.
# I have also filtered bar chart and htmlTable with a barrier of 50 rows.
#############  
  
  #Name="Antechinus stuartii"
  #X$kin
  #typeof(Name)
  if(nrow(X) == 0){
    stop(sprintf("The data set is empty",call.=FALSE))
  }

  if(COUNTRYCODE!=NULL | BASIS_OF_RECORD!=NULL | KINGDOM!=NULL | PHYLUM!=NULL |
     ORDER!=NULL | NAME!=NULL | FAMILY!=NULL |  GENUS!=NULL | CLASS!=NULL | SPECIFICEPITHET!=NULL | 
     VERNACULARNAME!=NULL | INSTITUTIONCODE!=NULL | TAXONRANK!=NULL | LIMIT!=NULL){
    
  if(COUNTRYCODE!=NULL){
    
    X<-subset(X,countryCode==COUNTRYCODE )
    if(nrow(X)==0){
      stop(sprintf("Error"))
    }
  }
  
  if(BASIS_OF_RECORD!=NULL){
    
    X<-subset(X,basisOfRecord==BASIS_OF_RECORD )
    if(nrow(X)==0){
      stop(sprintf())
    }
  }
  
  if(KINGDOM!=NULL){
    
    X<-subset(X,kingdom==KINGDOM )
    if(nrow(X)==0){
      stop(sprintf())
    }
  }
  
  if(PHYLUM!=NULL){
    
    X<-subset(X,phylum==PHYLUM )
    if(nrow(X)==0){
      stop(sprintf())
    }
  }
  
  if(ORDER!=NULL){
    
    X<-subset(X,order==ORDER )
    if(nrow(X)==0){
      stop(sprintf())
    }
  }
  
  if(NAME!=NULL){
    
    X<-subset(X,name==NAME )
    if(nrow(X)==0){
      stop(sprintf())
    }
  }
  
  if(FAMILY!=NULL){
    
    X<-subset(X,family==FAMILY )
    if(nrow(X)==0){
      stop(sprintf())
    }
  }
    if(GENUS!=NULL){
      
      X<-subset(X,genus==GENUS )
      if(nrow(X)==0){
        stop(sprintf())
      }
    } 
    
    if(VERNACULARNAME!=NULL){
      
      X<-subset(X,vernacularName==VERNACULARNAME )
      if(nrow(X)==0){
        stop(sprintf())
      }
    }
    if(TAXONRANK!=NULL){
      
      X<-subset(X,taxonRank==TAXONRANK )
      if(nrow(X)==0){
        stop(sprintf())
      }
    }
    if(SPECIFICEPITHET!=NULL){
      
      X<-subset(X,specificEpithet==SPECIFICEPITHET )
      if(nrow(X)==0){
        stop(sprintf())
      }
    }
    
  
  if(NAME!=NULL){
  c_1<-ddply(X,~name,summarise,number_of_distinct_orders=length((name)))
  v1<-max(c_1[,2])
  v2<-min(c_1[,2])
  htmlTable::htmlTable(c_1[c_1$number_of_distinct_orders==v1,])
  htmlTable::htmlTable(c_1[c_1$number_of_distinct_orders==v2,])
  
    if(nrow(X>50)){
    htmlTable::htmlTable(c_1)
    }
    else{
      plot_ly(c_1, x= ~name, y= ~number_of_distinct_orders,type="bar")
    
    }
  }
  if(COUNTRYCODE!=NULL){
  c_2<-ddply(X,~countryCode,summarise,number_of_distinct_orders=length((countryCode)))
  v1<-max(c_2[,2])
  v2<-min(c_2[,2])
  htmlTable::htmlTable(c_2[c_2$number_of_distinct_orders==v1,])
  htmlTable::htmlTable(c_2[c_2$number_of_distinct_orders==v2,])
  
    if(nrow(X>50)){
    htmlTable::htmlTable(c_1)
    }
    else{
    plot_ly(c_2, x= ~countryCode, y= ~number_of_distinct_orders,type="bar")
      
    }
  }
  if(PHYLUM!=NULL){
  c_3<-ddply(X,~phylum,summarise,number_of_distinct_orders=length((phylum)))
  v1<-max(c_3[,2])
  v2<-min(c_3[,2])
  htmlTable::htmlTable(c_3[c_3$number_of_distinct_orders==v1,])
  htmlTable::htmlTable(c_3[c_3$number_of_distinct_orders==v2,])
     
  if(nrow(X>50)){
    htmlTable::htmlTable(c_3)
  }
  else{
    plot_ly(c_3, x= ~phylum, y= ~number_of_distinct_orders,type="bar")
    
  }
  
  }
  if()
  c_4<-ddply(X,~order,summarise,number_of_distinct_orders=length((order)))
  if()
  c_5<-ddply(X,~specificEpithet,summarise,number_of_distinct_orders=length((specificEpithet)))
  if()
  c_6<-ddply(X,~family,summarise,number_of_distinct_orders=length((family)))
  if()
  c_8<-ddply(X,~class,summarise,number_of_distinct_orders=length((class)))
  if()
  c_7<-ddply(X,~genus,summarise,number_of_distinct_orders=length((genus)))
  if()
  c_9<-ddply(X,~kingdom,summarise,number_of_distinct_orders=length((kingdom)))
  if()
  c_10<-ddply(X,~taxonRank,summarise,number_of_distinct_orders=length((taxonRank)))
  if()
  c_11<-ddply(X,~collectionCode,summarise,number_of_distinct_orders=length((collectionCode)))
  if()
  c_12<-ddply(X,~vernacularName,summarise,number_of_distinct_orders=length((vernacularName)))
  if()
  c_13<-ddply(X,~institutionCode,summarise,number_of_distinct_orders=length((institutionCode)))
  
  }
  else{
    
      c_1<-ddply(X,~name,summarise,number_of_distinct_orders=length((name)))
      
      v1<-max(c_1[,2])
      v2<-min(c_1[,2])
      htmlTable::htmlTable(c_1[c_1$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_1[c_1$number_of_distinct_orders==v2,])
      
      if(nrow(X>50)){
        htmlTable::htmlTable(c_1)
      }
      else{
        plot_ly(c_1, x= ~name, y= ~number_of_distinct_orders,type="bar")
        
      }
      c_2<-ddply(X,~countryCode,summarise,number_of_distinct_orders=length((countryCode)))
      
      v1<-max(c_2[,2])
      v2<-min(c_2[,2])
      htmlTable::htmlTable(c_2[c_2$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_2[c_2$number_of_distinct_orders==v2,])
      
      if(nrow(X>50)){
        htmlTable::htmlTable(c_1)
      }
      else{
        plot_ly(c_2, x= ~countryCode, y= ~number_of_distinct_orders,type="bar")
        
      }
      
      c_3<-ddply(X,~phylum,summarise,number_of_distinct_orders=length((phylum)))
    
      v1<-max(c_3[,2])
      v2<-min(c_3[,2])
      htmlTable::htmlTable(c_3[c_3$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_3[c_3$number_of_distinct_orders==v2,])
      
      if(nrow(X>50)){
        htmlTable::htmlTable(c_3)
      }
      else{
        plot_ly(c_3, x= ~phylum, y= ~number_of_distinct_orders,type="bar")
        
      }
      
      c_4<-ddply(X,~order,summarise,number_of_distinct_orders=length((order)))
    
      c_5<-ddply(X,~specificEpithet,summarise,number_of_distinct_orders=length((specificEpithet)))
    
      c_6<-ddply(X,~family,summarise,number_of_distinct_orders=length((family)))
    
      c_8<-ddply(X,~class,summarise,number_of_distinct_orders=length((class)))
    
      c_7<-ddply(X,~genus,summarise,number_of_distinct_orders=length((genus)))
   
      c_9<-ddply(X,~kingdom,summarise,number_of_distinct_orders=length((kingdom)))
    
      c_10<-ddply(X,~taxonRank,summarise,number_of_distinct_orders=length((taxonRank)))
    
      c_11<-ddply(X,~collectionCode,summarise,number_of_distinct_orders=length((collectionCode)))
    
      c_12<-ddply(X,~vernacularName,summarise,number_of_distinct_orders=length((vernacularName)))
      
      c_13<-ddply(X,~institutionCode,summarise,number_of_distinct_orders=length((institutionCode)))
  }
  
  
}