
#' Function for flagging and filtering data based on Taxonomic fields
#' 
#' 
#' 
#' 
#' 
#' 
#' @author Ashwin Agrawal
#' @param X (data.frame) Data from gbif
#' @param flag_name (character) Name of the flag 
#' @param flag (logical) If TRUE will return dataframe with flags and flagged data column
#' @param filter (logical) If TRUE data will be filtered otherwise not, default=FALSE
#' @return (data.frame) If filter is TRUE then filtered data frame is returned otherwise if flag is TRUE flag and flag column is retured.
#' 
#' @details 
#' 
#' Exaplanation of function and method:
#' 
#' Function 1:Wrapper for flagging data-"Taxonomic_Flagging"
#' Simple function to call the main function for flagging data and returning the flagged data frame
#' 
#' Function 2:Wrapper for filtering the flag data-"Taxonomic_Filtering"
#' Simple function to call the main function for filtering the data returning the filtererd data frame
#' 
#' Function 3:Main function:"Taxonomic_Flagging_and_Filtering_main"
#' This function checks whether the input flag is valid and if so it then flags the data
#' based on the flags and filters the data based on the flag, it returns both flagged 
#' and filtered data frame depending on the user.
#' Now this function only contains simple flagging anf filtering mechanism based on 
#' NA values, I wil be adding more flags and more fields for flagging and filtering.
#' 
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
#' Z1<-Taxonomic_Flagging(X,FLAG_NAME = "name") 
#' View(Z)
#' 
#' Z2<-Taxonomic_Filtering(X,FLAG_NAME = "name")
#' View(Z2)
#' 
#' 



# Function 1-Flagging Wrapper Function
Taxonomic_Flagging<-function(X,FLAG_NAME){
  
  C<-Taxonomic_Flagging_and_Filtering_main(X,flag_name=FLAG_NAME,flag=TRUE)
  return(C)
  
}

#Function 2-Filtering Wrapper Function

Taxonomic_Filtering<-function(X,FLAG_NAME){
  
  C<-Taxonomic_Flagging_and_Filtering_main(X,flag_name = FLAG_NAME,filter=TRUE)
  return(C)
}


#Function 3-Flagging and Filtering Main function
Taxonomic_Flagging_and_Filtering_main<-function(X,flag_name,filter=FALSE,flag=FALSE){
  
    if(flag_name=="scientificName" || flag_name=="basisOfRecord" || flag_name=="kingdom" || flag_name=="phylum" ||
       flag_name=="order" || flag_name=="genus" || flag_name=="class" || flag_name=="specificEpithet" || 
       flag_name=="institutionCode" || flag_name=="taxonRank" || flag_name=="vernacularName" || flag_name=="name"){
      
      if(flag_name=="scientificName"){
      Data_flag_name<-subset(X,select=c(key,scientificName))
      
      Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$scientificName))
      
      if(flag==TRUE){
#Flag information section
      
      cat(sprintf("The flag convention are as follows\n"))
      cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
      return(Data_flag_name)
      }
      
      
# Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
# and can give user the option to filter the data based on each flag separately  
      
      if(filter==TRUE){
        X$Flag_1<-Data_flag_name$Flag_1
        X<-subset(X,X$Flag_1==0)
        
        drops <- c("Flag_1")
        X<-X[ , !(names(X) %in% drops)]
        
        if(nrow(X)==0){
          stop(paste("The data after filtering is empty"))
        }else{
          cat(sprintf("The filtered data based on scientificName is\n"))
          return(X)
        }
        
          } 
    
      }
      
      
      if(flag_name=="basisOfRecord"){
        Data_flag_name<-subset(X,select=c(key,basisOfRecord))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$basisOfRecord))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on basisOfRecord is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
      
      if(flag_name=="kingdom"){
        Data_flag_name<-subset(X,select=c(key,kingdom))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$kingdom))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on scientificName is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
      
      if(flag_name=="phylum"){
        Data_flag_name<-subset(X,select=c(key,phylum))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$phylum))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on phylum is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
      if(flag_name=="order"){
        Data_flag_name<-subset(X,select=c(key,order))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$order))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on order is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
      if(flag_name=="genus"){
        Data_flag_name<-subset(X,select=c(key,genus))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$genus))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on genus is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
      if(flag_name=="class"){
        Data_flag_name<-subset(X,select=c(key,class))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$class))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on class is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
      
      if(flag_name=="specificEpithet"){
        Data_flag_name<-subset(X,select=c(key,specificEpithet))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$specificEpithet))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on specificEpithet is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
      
      if(flag_name=="institutionCode"){
        Data_flag_name<-subset(X,select=c(key,institutionCode))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$institutionCode))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on institutionCode is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
      if(flag_name=="taxonRank"){
        Data_flag_name<-subset(X,select=c(key,taxonRank))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$taxonRank))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on taxonRank is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
      if(flag_name=="vernacularName"){
        Data_flag_name<-subset(X,select=c(key,vernacularName))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$vernacularName))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on vernacularName is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
      
      if(flag_name=="name"){
        Data_flag_name<-subset(X,select=c(key,name))
        
        Data_flag_name$Flag_1<-as.integer(is.na(Data_flag_name$name))
        
        if(flag==TRUE){
          #Flag information section
          
          cat(sprintf("The flag convention are as follows\n"))
          cat(sprintf("The flag with column name Flag_1 has two values 0/1, if the value is 1 then data field is empty otherwise some data is present\n"))
          return(Data_flag_name)
        }
        
        
        # Now only one flag is developed therefore filter data based on one flag only, in future we can add as many flags as required
        # and can give user the option to filter the data based on each flag separately  
        
        if(filter==TRUE){
          X$Flag_1<-Data_flag_name$Flag_1
          X<-subset(X,X$Flag_1==0)
          
          drops <- c("Flag_1")
          X<-X[ , !(names(X) %in% drops)]
          
          if(nrow(X)==0){
            stop(paste("The data after filtering is empty"))
          }else{
            cat(sprintf("The filtered data based on name is\n"))
            return(X)
          }
          
        } 
        
      }
      
      
    }else{
          stop(paste("The flag is not present"))
      
    }  
                           
}
