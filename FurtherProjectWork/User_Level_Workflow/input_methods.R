

## Read integer function
readinteger <- function()
{ 
  n <- readline(prompt="Enter an integer: ")
  if(!grepl("^[0-9]+$",n))
  {
    return(readinteger())
  }
  
  return(as.integer(n))
}


## Read date function

IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}


readdate <- function()
{ 
  n <- readline(prompt="Enter a date: ")
  if(IsDate(n))
  {
    return(n)
  }
  else{
    print("Wrong input, date not detected")
  }
  
}
#print(readdate())

## Usage of above functions

library(rgbif)
d1 <- occ_data(
country = "AU",     # Country code for australia
classKey= 359,      # Class code for mammalia
limit=500,
hasCoordinate = T)

X<-d1$data
#View(X)
data<-subset(X,select=c(decimalLatitude,decimalLongitude,eventDate))
#View(data)
n<-(readinteger())

# filtering based on gievn input
data_subset<-subset(data,data$decimalLatitude<n)
View(data_subset)

#Similarly many filtering and flagging can be performed based on given inputs.
