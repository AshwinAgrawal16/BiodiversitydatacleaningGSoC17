
#' @example1
#'
#'
#' X1<-read_region_input()
#' Give input as 2
#' Then  Give input as australia or any other country
#' Output will be filtered data by the entered country.
#' View(X1)
#'
#'
#' @example2
#' X2<-read_region_input()
#' Give input as 1
#' Then  Give input as any continent 
#' Output will be an error because there is continent field in gbif data.
#' 
#' 
#' 
library(rgbif)
d11 <- occ_data(
  # Country code for australia
  classKey= 359,      # Class code for mammalia
  
  limit=2000,
  
  hasCoordinate = T
  
)
X<-d11$data
# This is the main data which user will be initially querring. There will be wrapper 
# function which contains all the questions and one by one each question will be asked 
# and based on the input the data will be filtered and supplied to the user.
main_data<-X
#View(main_data)

read_region_output<-function(input_1,input_2=NULL){
  if(input_1==-1){
    stop(paste("The entered input is wrong,please enter correct input"))
  }
  else{
    if(input_1==2){
      subset_data<-subset(main_data,tolower(main_data$country)==tolower(input_2))
      if(nrow(subset_data)==0){
        stop(paste("The data set contains no observations of particular country or else the country name is wrong"))
      }
      return(subset_data)
      #View(subset_data)
      
    }
    if(input_1==1){
      stop(paste("There is no continent field in gbif data"))
    }
    
  }
  
}

# The input function which takes the input for the user and calls the output function
# internally to guve the results.

read_region_input <- function(prompt2="What is the region of your interest, 
Enter your choice (Enter 1 for continent and 2 for country):
1) Continent2) Country")
{ 
  input_1 <- readline(prompt=prompt2)
  if(as.integer(input_1)==1 || as.integer(input_1)==2){
    if(as.integer(input_1)==1){
      #In gbif data there is no field as continent
      input_11<-readline(prompt="Enter the continent of your interest.")
      data_out<-read_region_output(input_1,input_11)
      return(data_out)
    }
    else{
      input_12<-readline(prompt="Enter the country of your interest.")
      data_out<-read_region_output(input_1,input_12)
      return(data_out)
    }
    
  }else{
    # If return value is -1 we will prompt wrong input to the user.
    read_region_output(-1)
  }
  
  
}


