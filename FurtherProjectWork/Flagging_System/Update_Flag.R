#'
#'
#'
#' @param X data.frame
#' @param flagName, Name of the flag
#' @param flagType, Type of the flag:- taxonomic, spatial, temporal
#' @param flagDesc, The description of the flag
#' @param Filter, If TRUE then data can be filtered based on this flag
#' @param Levels, This tells the different levels between which the data is flagged by this flag
#'
#' @description 
#' Similar to Add_Flag function, just that it updates the required flag by the developer.
#'
#'


Update_Flag<-function(X,flagType=NULL,flagName=NULL,flagDesc=NULL,Filter=FALSE,Levels=NULL){
  
  if(Flag_Search(X,NAME=flagName)!=0){
    index=Flag_Search(X,NAME=flagName)
    X[index,2:4]=c(flagType,flagDesc,Filter)
    cat(sprintf("The flag is updated succesfully\n"))
    print(X[index,])
    
    
  }
  else{
    stop(cat(paste("This flag name doesnot exists, choose a different name")))
  }
  
  
}


Flag_Search<-function(X,NAME=NULL){
  
  if(length(which(X$flagName==NAME))==1){
    return(which(X$flagName==NAME))
  }
  else{
    return(0)
  }
}