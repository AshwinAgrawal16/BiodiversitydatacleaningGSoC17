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

  if(!is.null(COUNTRYCODE) || !is.null(BASIS_OF_RECORD) || !is.null(KINGDOM) || !is.null(PHYLUM) ||
     !is.null(ORDER) || !is.null(NAME) || !is.null(FAMILY) ||  !is.null(GENUS) || !is.null(CLASS) || !is.null(SPECIFICEPITHET) || 
     !is.null(VERNACULARNAME) || !is.null(INSTITUTIONCODE) || !is.null(TAXONRANK) || is.null(LIMIT)){
    
  if(!is.null(COUNTRYCODE)){
    
    X<-subset(X,countryCode==COUNTRYCODE)
    if(nrow(X)==0){
      stop(paste("The country code is not valid"))
    }
  }
  
  if(!is.null(BASIS_OF_RECORD)){
    
    X<-subset(X,basisOfRecord==BASIS_OF_RECORD)
    if(nrow(X)==0){
      stop(paste("Basis of record not valid"))
    }
  }
  
  if(!is.null(KINGDOM)){
    
    X<-subset(X,kingdom==KINGDOM)
    if(nrow(X)==0){
      stop(paste("The kingdom is not valid"))
    }
  }
  
  if(!is.null(PHYLUM)){
    
    X<-subset(X,phylum==PHYLUM)
    if(nrow(X)==0){
      stop(paste("The phylum is not valid"))
    }
  }
  
  if(!is.null(ORDER)){
    
    X<-subset(X,order==ORDER)
    if(nrow(X)==0){
      stop(paste("The order is not valid"))
    }
  }
  
  if(!is.null(NAME)){
    
    X<-subset(X,name==NAME)
    if(nrow(X)==0){
      stop(paste("The name is not valid"))
    }
  }
  
  if(!is.null(FAMILY)){
    
    X<-subset(X,family==FAMILY)
    if(nrow(X)==0){
      stop(paste("The family is not valid"))
    }
  }
    if(!is.null(GENUS)){
      
      X<-subset(X,genus==GENUS)
      if(nrow(X)==0){
        stop(paste("The genus is not valid"))
      }
    } 
    
    if(!is.null(VERNACULARNAME)){
      
      X<-subset(X,vernacularName==VERNACULARNAME)
      if(nrow(X)==0){
        stop(paste("The vernacular name is not valid"))
      }
    }
    if(!is.null(TAXONRANK)){
      
      X<-subset(X,taxonRank==TAXONRANK)
      if(nrow(X)==0){
        stop(paste("The taxon rank is not valid"))
      }
    }
    if(!is.null(SPECIFICEPITHET)){
      
      X<-subset(X,specificEpithet==SPECIFICEPITHET)
      if(nrow(X)==0){
        stop(paste("The specific epithet is not valid"))
      }
    }
    
  
  if(is.null(NAME)){
  c_1<-ddply(X,~name,summarise,number_of_distinct_orders=length((name)))
  v1<-max(c_1[,2])
  v2<-min(c_1[,2])
  htmlTable::htmlTable(c_1[c_1$number_of_distinct_orders==v1,])
  htmlTable::htmlTable(c_1[c_1$number_of_distinct_orders==v2,])
  
    if(nrow(X>10)){
    htmlTable::htmlTable(c_1)
    }
    else{
      plot_ly(c_1, x= ~name, y= ~number_of_distinct_orders,type="bar")
    
    }
  }
  if(is.null(COUNTRYCODE)){
  c_2<-ddply(X,~countryCode,summarise,number_of_distinct_orders=length((countryCode)))
  v1<-max(c_2[,2])
  v2<-min(c_2[,2])
  htmlTable::htmlTable(c_2[c_2$number_of_distinct_orders==v1,])
  htmlTable::htmlTable(c_2[c_2$number_of_distinct_orders==v2,])
  
    if(nrow(X>10)){
    htmlTable::htmlTable(c_1)
    }
    else{
    plot_ly(c_2, x= ~countryCode, y= ~number_of_distinct_orders,type="bar")
      
    }
  }
  if(is.null(PHYLUM)){
  c_3<-ddply(X,~phylum,summarise,number_of_distinct_orders=length((phylum)))
  v1<-max(c_3[,2])
  v2<-min(c_3[,2])
  htmlTable::htmlTable(c_3[c_3$number_of_distinct_orders==v1,])
  htmlTable::htmlTable(c_3[c_3$number_of_distinct_orders==v2,])
     
  if(nrow(X>10)){
    htmlTable::htmlTable(c_3)
  }
  else{
    plot_ly(c_3, x= ~phylum, y= ~number_of_distinct_orders,type="bar")
    
  }
  
  }
  if(is.null(ORDER)){
    c_4<-ddply(X,~order,summarise,number_of_distinct_orders=length((order)))
    v1<-max(c_4[,2])
    v2<-min(c_4[,2])
    htmlTable::htmlTable(c_4[c_4$number_of_distinct_orders==v1,])
    htmlTable::htmlTable(c_4[c_4$number_of_distinct_orders==v2,])
    
    if(nrow(X>10)){
      htmlTable::htmlTable(c_4)
    }
    else{
      plot_ly(c_4, x= ~order, y= ~number_of_distinct_orders,type="bar")
    
    }
  }
  
  if(is.null(specificEpithet)){
    c_5<-ddply(X,~specificEpithet,summarise,number_of_distinct_orders=length((specificEpithet)))
    v1<-max(c_5[,2])
    v2<-min(c_5[,2])
    htmlTable::htmlTable(c_5[c_5$number_of_distinct_orders==v1,])
    htmlTable::htmlTable(c_5[c_5$number_of_distinct_orders==v2,])
    
    if(nrow(X>10)){
      htmlTable::htmlTable(c_5)
    }
    else{
      plot_ly(c_5, x= ~specificEpithet, y= ~number_of_distinct_orders,type="bar")
      
    }
    
  }
  
  if(is.null(family)){
    c_6<-ddply(X,~family,summarise,number_of_distinct_orders=length((family)))
    v1<-max(c_6[,2])
    v2<-min(c_6[,2])
    htmlTable::htmlTable(c_6[c_6$number_of_distinct_orders==v1,])
    htmlTable::htmlTable(c_6[c_6$number_of_distinct_orders==v2,])
    
    if(nrow(X>10)){
      htmlTable::htmlTable(c_6)
    }
    else{
      plot_ly(c_6, x= ~family, y= ~number_of_distinct_orders,type="bar")
      
    }
  }
 
  if(is.null(genus)){
    c_7<-ddply(X,~genus,summarise,number_of_distinct_orders=length((genus)))
    v1<-max(c_7[,2])
    v2<-min(c_7[,2])
    htmlTable::htmlTable(c_6[c_6$number_of_distinct_orders==v1,])
    htmlTable::htmlTable(c_6[c_6$number_of_distinct_orders==v2,])
    
    if(nrow(X>10)){
      htmlTable::htmlTable(c_7)
    }
    else{
      plot_ly(c_7, x= ~genus, y= ~number_of_distinct_orders,type="bar")
      
    }
    
  }
  
  if(is.null(class)){
    c_8<-ddply(X,~class,summarise,number_of_distinct_orders=length((class)))
    v1<-max(c_8[,2])
    v2<-min(c_8[,2])
    htmlTable::htmlTable(c_8[c_8$number_of_distinct_orders==v1,])
    htmlTable::htmlTable(c_8[c_8$number_of_distinct_orders==v2,])
    
    if(nrow(X>10)){
      htmlTable::htmlTable(c_8)
    }
    else{
      plot_ly(c_8, x= ~class, y= ~number_of_distinct_orders,type="bar")
      
    }
    
  }
  
  
  if(is.null(kingdom)){
    
    c_9<-ddply(X,~kingdom,summarise,number_of_distinct_orders=length((kingdom)))
    v1<-max(c_9[,2])
    v2<-min(c_9[,2])
    htmlTable::htmlTable(c_9[c_9$number_of_distinct_orders==v1,])
    htmlTable::htmlTable(c_9[c_9$number_of_distinct_orders==v2,])
    
    if(nrow(X>10)){
      htmlTable::htmlTable(c_9)
    }
    else{
      plot_ly(c_9, x= ~kingdom, y= ~number_of_distinct_orders,type="bar")
      
    }
  }
  
  if(is.null(taxonRank)){
    c_10<-ddply(X,~taxonRank,summarise,number_of_distinct_orders=length((taxonRank)))
    v1<-max(c_10[,2])
    v2<-min(c_10[,2])
    htmlTable::htmlTable(c_10[c_10$number_of_distinct_orders==v1,])
    htmlTable::htmlTable(c_10[c_10$number_of_distinct_orders==v2,])
    
    if(nrow(X>10)){
      htmlTable::htmlTable(c_10)
    }
    else{
      plot_ly(c_10, x= ~kingdom, y= ~number_of_distinct_orders,type="bar")
      
    }
    
  }
  
  if(is.null(collectionCode)){
    c_11<-ddply(X,~collectionCode,summarise,number_of_distinct_orders=length((collectionCode)))
    v1<-max(c_11[,2])
    v2<-min(c_11[,2])
    htmlTable::htmlTable(c_11[c_11$number_of_distinct_orders==v1,])
    htmlTable::htmlTable(c_11[c_11$number_of_distinct_orders==v2,])
    
    if(nrow(X>10)){
      htmlTable::htmlTable(c_11)
    }
    else{
      plot_ly(c_11, x= ~kingdom, y= ~number_of_distinct_orders,type="bar")
      
    }
    
    
  }
  
  if(is.null(vernacularName)){
    c_12<-ddply(X,~vernacularName,summarise,number_of_distinct_orders=length((vernacularName)))
    v1<-max(c_12[,2])
    v2<-min(c_12[,2])
    htmlTable::htmlTable(c_12[c_12$number_of_distinct_orders==v1,])
    htmlTable::htmlTable(c_12[c_12$number_of_distinct_orders==v2,])
    
    if(nrow(X>10)){
      htmlTable::htmlTable(c_12)
    }
    else{
      plot_ly(c_12, x= ~vernacularName, y= ~number_of_distinct_orders,type="bar")
      
    }
    
  }
  
  if(is.null(institutionCode)){
    
    c_13<-ddply(X,~institutionCode,summarise,number_of_distinct_orders=length((institutionCode)))
    v1<-max(c_13[,2])
    v2<-min(c_13[,2])
    htmlTable::htmlTable(c_13[c_13$number_of_distinct_orders==v1,])
    htmlTable::htmlTable(c_13[c_13$number_of_distinct_orders==v2,])
    
    if(nrow(X>10)){
      htmlTable::htmlTable(c_13)
    }
    else{
      plot_ly(c_13, x= ~institutionCode, y= ~number_of_distinct_orders,type="bar")
      
    }
  }
  
    
  }
  else{
    
    
      c_1<-ddply(X,~name,summarise,number_of_distinct_orders=length((name)))
      v1<-max(c_1[,2])
      v2<-min(c_1[,2])
      htmlTable::htmlTable(c_1[c_1$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_1[c_1$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
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
      
      if(nrow(X>10)){
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
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_3)
      }
      else{
        plot_ly(c_3, x= ~phylum, y= ~number_of_distinct_orders,type="bar")
        
      }
      
    
    
      c_4<-ddply(X,~order,summarise,number_of_distinct_orders=length((order)))
      v1<-max(c_4[,2])
      v2<-min(c_4[,2])
      htmlTable::htmlTable(c_4[c_4$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_4[c_4$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_4)
      }
      else{
        plot_ly(c_4, x= ~order, y= ~number_of_distinct_orders,type="bar")
        
      }
    
    
 
      c_5<-ddply(X,~specificEpithet,summarise,number_of_distinct_orders=length((specificEpithet)))
      v1<-max(c_5[,2])
      v2<-min(c_5[,2])
      htmlTable::htmlTable(c_5[c_5$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_5[c_5$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_5)
      }
      else{
        plot_ly(c_5, x= ~specificEpithet, y= ~number_of_distinct_orders,type="bar")
        
      }
      
    
    
    
      c_6<-ddply(X,~family,summarise,number_of_distinct_orders=length((family)))
      v1<-max(c_6[,2])
      v2<-min(c_6[,2])
      htmlTable::htmlTable(c_6[c_6$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_6[c_6$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_6)
      }
      else{
        plot_ly(c_6, x= ~family, y= ~number_of_distinct_orders,type="bar")
        
      }
    
    
    
      c_7<-ddply(X,~genus,summarise,number_of_distinct_orders=length((genus)))
      v1<-max(c_7[,2])
      v2<-min(c_7[,2])
      htmlTable::htmlTable(c_6[c_6$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_6[c_6$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_7)
      }
      else{
        plot_ly(c_7, x= ~genus, y= ~number_of_distinct_orders,type="bar")
        
      }
      
    
    
    
      c_8<-ddply(X,~class,summarise,number_of_distinct_orders=length((class)))
      v1<-max(c_8[,2])
      v2<-min(c_8[,2])
      htmlTable::htmlTable(c_8[c_8$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_8[c_8$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_8)
      }
      else{
        plot_ly(c_8, x= ~class, y= ~number_of_distinct_orders,type="bar")
        
      }
      
    
    
    
  
      
      c_9<-ddply(X,~kingdom,summarise,number_of_distinct_orders=length((kingdom)))
      v1<-max(c_9[,2])
      v2<-min(c_9[,2])
      htmlTable::htmlTable(c_9[c_9$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_9[c_9$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_9)
      }
      else{
        plot_ly(c_9, x= ~kingdom, y= ~number_of_distinct_orders,type="bar")
        
      }
    
    
    
      c_10<-ddply(X,~taxonRank,summarise,number_of_distinct_orders=length((taxonRank)))
      v1<-max(c_10[,2])
      v2<-min(c_10[,2])
      htmlTable::htmlTable(c_10[c_10$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_10[c_10$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_10)
      }
      else{
        plot_ly(c_10, x= ~kingdom, y= ~number_of_distinct_orders,type="bar")
        
      }
      
    
    
    
      c_11<-ddply(X,~collectionCode,summarise,number_of_distinct_orders=length((collectionCode)))
      v1<-max(c_11[,2])
      v2<-min(c_11[,2])
      htmlTable::htmlTable(c_11[c_11$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_11[c_11$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_11)
      }
      else{
        plot_ly(c_11, x= ~kingdom, y= ~number_of_distinct_orders,type="bar")
        
      }
      
      
    
    
    
      c_12<-ddply(X,~vernacularName,summarise,number_of_distinct_orders=length((vernacularName)))
      v1<-max(c_12[,2])
      v2<-min(c_12[,2])
      htmlTable::htmlTable(c_12[c_12$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_12[c_12$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_12)
      }
      else{
        plot_ly(c_12, x= ~vernacularName, y= ~number_of_distinct_orders,type="bar")
        
      }
      
    
    
   
      
      c_13<-ddply(X,~institutionCode,summarise,number_of_distinct_orders=length((institutionCode)))
      v1<-max(c_13[,2])
      v2<-min(c_13[,2])
      htmlTable::htmlTable(c_13[c_13$number_of_distinct_orders==v1,])
      htmlTable::htmlTable(c_13[c_13$number_of_distinct_orders==v2,])
      
      if(nrow(X>10)){
        htmlTable::htmlTable(c_13)
      }
      else{
        plot_ly(c_13, x= ~institutionCode, y= ~number_of_distinct_orders,type="bar")
        
      }
    
  }
  
  
}