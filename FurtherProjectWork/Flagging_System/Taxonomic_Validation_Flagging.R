#'
#'
#' @param X (data.frame) Input gbif data frame
#' @param Species_Name (character) The name of the species for which validation is to be performed
#' @return Data frame-Flagged
#'
#' @description The functions validates all the major taxonomic fields using the scientific name as the standard basis.
#'  
#' 
#' @example 
#' library(taxize)
#' library(rgbif)
#' library(plyr)
#' 
#' d1 <- occ_data(
#' country = "AU",     # Country code for australia
#' classKey= 359,      # Class code for mammalia
#' limit=50000,
#' hasCoordinate = T
#' )
#' 
#' X<-d1$data
#' CC1<-Taxonomic_validation_Flagging(X,Species_Name = "Bettongia penicillata")
#' View(CC1)
#'
#' CC2<-Taxonomic_validation_Flagging(X,Species_Name = "Wallabia bicolor")
#' View(CC2)
#'

Taxonomic_validation_Flagging<-function(X,Species_Name){
  
    Data_validation<-subset(X,select=c(key,name,phylum,genus,class,family,order,kingdom))
    
    Data_validation<-subset(Data_validation,Data_validation$name==Species_Name)
    
    if(nrow(Data_validation)==0){
      
      cat(paste("The input species name is not present or the data set is empty"))
    }
    else{
      out <- classification(Species_Name, db='gbif')
      1                                                # Here 1 is present beacuse sometimes if more than 1 records are found it asks for choice, 1st choice is the most correct one, hence 1 is present
      out<-as.data.frame(out[1])
      
      Data_validation$flag_kingdom<-Data_validation$kingdom==out[1,1]
      Data_validation$flag_phylum<-Data_validation$phylum==out[2,1]
      Data_validation$flag_class<-Data_validation$class==out[3,1]
      Data_validation$flag_order<-Data_validation$order==out[4,1]
      Data_validation$flag_family<-Data_validation$family==out[5,1]
      Data_validation$flag_genus<-Data_validation$genus==out[6,1]
      #Data_validation$flag_species_name<-Data_validation$name==out[7,1]
      
      cat(sprintf("If the flag value is TRUE means the data is correct else there is some discripancy in actual and validated data"))
      
      return(Data_validation)
    }
    
    # Here the above validation is only species specific, I am trying to extend it to whole data frame,
    # but in doing so I am ending up using loops which are very slow, I will optimize it to work on whole data frame and not only species specific
    
    
    ######  
    #c_1<-ddply(Data_validation,~name,summarise,number_of_distinct_orders=length((name)))
    #View(c_1)
    #for(i in 1:nrow(c_1)){
    #out <- classification(c_1[146,1], db='gbif')
    #1
    #out<-as.data.frame(out[1])
    #Z<-((Data_validation$name==c_1[147,1])==TRUE)
    #View(Z)
    #Data_validation$name[1]==c_1[147,1]
    ####### 
    
    
  
}


