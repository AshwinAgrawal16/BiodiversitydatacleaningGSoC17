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
#' The Add_Flag function adds any new flag in the system. The Flag_Search is a helper
#' function which identifies if there is any similar flag present in the system.
#' Now the Levels part of the function is not developed as I will use sparse data frame 
#' or even normal data frame to store the data with varrying size. When the Hashmap is fully
#' functional in R, I will import the function with HashMap. 
#'
#' 
#'
#'


Add_Flag<-function(X,flagName=NULL,flagType=NULL,flagDesc=NULL,Filter=FALSE,Levels=NULL){
  
  if(Flag_Search(X,NAME=flagName)==TRUE){
    stop(cat(paste("This flag name already exists choose a different name")))
    
    
  }
  else{
    
    newrow=c(flagName,flagType,flagDesc,Filter)
    X<-rbind(X,newrow)
  }
  
  
  
}


Flag_Search<-function(X,NAME=NULL){
  
  if(length(which(X$flagName==NAME))==1){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}